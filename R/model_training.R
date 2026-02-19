`%||%` <- function(x, y) if (is.null(x)) y else x

#' Build a parsnip model specification with tune() placeholders
#'
#' Returns a parsnip model spec for the given engine with all tunable
#' hyperparameters marked as \code{tune()}. \pkg{bonsai} must be loaded
#' (listed in \code{tar_option_set} packages) for the lightgbm engine.
#'
#' @param engine Character: "ranger", "xgboost", or "lightgbm".
#' @param seed Integer random seed forwarded to engine-specific arguments.
#' @return A parsnip model specification.
make_parsnip_spec <- function(engine, seed = 42L) {
  if (engine == "ranger") {
    parsnip::rand_forest(
      trees = tune::tune(), mtry = tune::tune(), min_n = tune::tune()
    ) |>
      parsnip::set_engine("ranger",
        importance  = "impurity",
        num.threads = 1L,
        seed        = seed
      ) |>
      parsnip::set_mode("regression")
  } else if (engine == "xgboost") {
    parsnip::boost_tree(
      trees       = tune::tune(),
      tree_depth  = tune::tune(),
      learn_rate  = tune::tune(),
      sample_size = tune::tune()
    ) |>
      parsnip::set_engine("xgboost", nthread = 1L, verbose = 0) |>
      parsnip::set_mode("regression")
  } else if (engine == "lightgbm") {
    parsnip::boost_tree(
      trees      = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      mtry       = tune::tune()
    ) |>
      parsnip::set_engine("lightgbm",
        num_threads = 1L,
        verbose     = -1L,
        seed        = seed
      ) |>
      parsnip::set_mode("regression")
  } else {
    stop(sprintf("Unsupported engine: %s", engine), call. = FALSE)
  }
}

#' Run spatial cross-validation using tidymodels
#'
#' Creates a parsnip workflow for the given engine, optionally tunes
#' hyperparameters via \code{tune::tune_bayes()}, evaluates the workflow
#' across all spatial folds, then fits the final model on the full
#' training set.
#'
#' @param model_spec Named list entry from ml_cfg$ml$models.
#' @param model_data List from prepare_model_data (y, X, feature_names, complete_idx).
#' @param resamples rsample rset from create_spatial_resamples.
#' @param ml_cfg Parsed ML config list.
#' @param seed Random seed.
#' @return Named list: model_id, engine, fold_results, overall_metrics,
#'   final_model (fitted \code{workflows::workflow}), best_hp, feature_names,
#'   n_obs, n_folds.
run_spatial_cv <- function(model_spec, model_data, resamples, ml_cfg, seed = 42L) {
  model_id <- model_spec$id %||% model_spec$engine
  engine   <- model_spec$engine

  # Register a parallel backend for fold and HP-candidate evaluation.
  # Each engine is configured to use 1 internal thread so that the N_workers
  # parallel fold processes do not oversubscribe the CPU.
  n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
  doParallel::registerDoParallel(cores = n_workers)
  on.exit(doParallel::stopImplicitCluster(), add = TRUE)

  # Build parsnip spec and workflow (.outcome ~ . uses all X columns)
  spec <- make_parsnip_spec(engine, seed = seed)
  wf   <- workflows::workflow() |>
    workflows::add_formula(.outcome ~ .) |>
    workflows::add_model(spec)

  # --- Bayesian hyperparameter tuning via tune::tune_bayes() ---
  best_hp <- list()
  if (isTRUE(model_spec$tune)) {
    init_points <- as.integer(model_spec$tune_init_points %||% 8L)
    n_iter      <- as.integer(model_spec$tune_iters        %||% 20L)

    # Auto-extract and finalize dials param set (resolves data-dependent ranges)
    params <- tune::extract_parameter_set_dials(wf) |>
      dials::finalize(model_data$X)

    set.seed(seed)
    tune_res <- tune::tune_bayes(
      object     = wf,
      resamples  = resamples,
      param_info = params,
      initial    = init_points,
      iter       = n_iter,
      metrics    = yardstick::metric_set(yardstick::rmse),
      control    = tune::control_bayes(
        verbose      = FALSE,
        no_improve   = 15L,
        seed         = seed,
        parallel_over = "everything"  # parallelise across folds AND candidates
      )
    )

    best_row <- tune::select_best(tune_res, metric = "rmse")
    best_hp  <- as.list(best_row[, !grepl("^\\.config", names(best_row)), drop = FALSE])
    wf       <- tune::finalize_workflow(wf, best_row)

    message(sprintf(
      "[tune] Best HP for %s via tune_bayes (%d evals): %s",
      engine, init_points + n_iter,
      paste(names(best_hp), round(unlist(best_hp), 4), sep = "=", collapse = ", ")
    ))
  }

  # Evaluate final (tuned) workflow across all spatial folds
  cv_res <- tune::fit_resamples(
    wf,
    resamples = resamples,
    metrics   = yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq),
    control   = tune::control_resamples(save_pred = FALSE, allow_par = TRUE)
  )

  raw_metrics <- tune::collect_metrics(cv_res, summarize = FALSE)
  fold_ids    <- sort(unique(raw_metrics$id))

  fold_results <- lapply(seq_along(fold_ids), function(i) {
    fid <- fold_ids[i]
    fm  <- raw_metrics[raw_metrics$id == fid, ]
    get_m <- function(nm) {
      v <- fm$.estimate[fm$.metric == nm]
      if (length(v)) v[1] else NA_real_
    }
    sp <- resamples$splits[[i]]
    list(
      fold    = fid,
      n_train = nrow(rsample::analysis(sp)),
      n_test  = nrow(rsample::assessment(sp)),
      metrics = list(rmse = get_m("rmse"), mae = get_m("mae"), rsq = get_m("rsq"))
    )
  })

  agg <- tune::collect_metrics(cv_res, summarize = TRUE)
  get_agg <- function(nm) {
    v <- agg$mean[agg$.metric == nm]
    if (length(v)) v[1] else NA_real_
  }
  overall_metrics <- list(
    rmse = get_agg("rmse"), mae = get_agg("mae"), rsq = get_agg("rsq")
  )

  # Fit final model on all complete-case data with best (or default) params
  df <- cbind(
    data.frame(.outcome = model_data$y, check.names = FALSE),
    model_data$X
  )
  final_fit <- parsnip::fit(wf, data = df)

  list(
    model_id        = model_id,
    engine          = engine,
    fold_results    = fold_results,
    overall_metrics = overall_metrics,
    final_model     = final_fit,
    best_hp         = best_hp,
    feature_names   = model_data$feature_names,
    n_obs           = nrow(df),
    n_folds         = length(fold_results)
  )
}

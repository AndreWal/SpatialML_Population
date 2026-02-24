if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

polygon_key_columns <- function(df) {
  cols <- c("country_code", "admin_unit_harmonized", "year")
  cols[cols %in% names(df)]
}

build_polygon_key_table <- function(df) {
  keys <- polygon_key_columns(df)
  if (length(keys) == 0 || nrow(df) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  unique(as.data.frame(df[, keys, drop = FALSE], stringsAsFactors = FALSE))
}

merge_with_polygon_keys <- function(diag_df, polygon_keys) {
  if (nrow(polygon_keys) == 0) return(diag_df)
  keys <- intersect(names(polygon_keys), names(diag_df))
  if (length(keys) == 0) return(diag_df)
  out <- merge(polygon_keys, diag_df, by = keys, all.x = TRUE, sort = FALSE)
  out
}

safe_rel_error <- function(mass_error, observed_population) {
  denom <- pmax(abs(observed_population), 1)
  mass_error / denom
}

#' Validate polygon-year mass preservation for constrained allocations
#'
#' @param allocation_df Cell-allocation table (one row per polygon-year x cell).
#' @param polygon_panel Optional sf/data.frame polygon panel to detect empty-support cases.
#' @param polygon_total_col Source polygon total column name.
#' @param tolerance_rel Relative error tolerance for pass/fail.
#' @return Diagnostics data.frame with one row per polygon-year.
validate_mass_preservation <- function(allocation_df,
                                       polygon_panel = NULL,
                                       polygon_total_col = "population",
                                       tolerance_rel = 1e-8) {
  if (!is.data.frame(allocation_df)) {
    stop("allocation_df must be a data.frame", call. = FALSE)
  }

  keys <- polygon_key_columns(allocation_df)
  out_cols <- c(keys,
                "observed_population", "allocated_population_sum",
                "mass_error", "mass_rel_error",
                "n_cells_intersecting", "n_cells_positive_weight",
                "zero_weight_fallback_used", "qa_status")

  if (nrow(allocation_df) == 0) {
    diag_df <- as.data.frame(setNames(replicate(length(out_cols), logical(0), simplify = FALSE), out_cols))
    for (nm in out_cols) diag_df[[nm]] <- diag_df[[nm]][0]
    return(diag_df)
  }

  alloc <- as.data.frame(allocation_df, stringsAsFactors = FALSE)
  grp <- allocation_group_id(alloc, keys)

  rows <- vector("list", length(levels(grp)))
  k <- 0L
  for (g in levels(grp)) {
    idx <- which(grp == g)
    if (length(idx) == 0) next
    d <- alloc[idx, , drop = FALSE]

    obs_vals <- suppressWarnings(as.numeric(d$population_polygon %||% d[[polygon_total_col]]))
    obs_vals <- obs_vals[is.finite(obs_vals)]
    observed <- if (length(obs_vals) > 0) obs_vals[1] else NA_real_

    allocated_sum <- sum(as.numeric(d$pop_allocated), na.rm = TRUE)
    if (!any(is.finite(as.numeric(d$pop_allocated)))) allocated_sum <- NA_real_

    mass_error <- if (is.finite(observed) && is.finite(allocated_sum)) {
      allocated_sum - observed
    } else {
      NA_real_
    }
    mass_rel_error <- if (is.finite(mass_error) && is.finite(observed)) {
      safe_rel_error(mass_error, observed)
    } else {
      NA_real_
    }

    n_cells_intersecting <- if ("cell_index" %in% names(d)) {
      length(unique(d$cell_index))
    } else {
      nrow(d)
    }
    n_cells_positive_weight <- if ("weight_norm" %in% names(d)) {
      sum(is.finite(d$weight_norm) & d$weight_norm > 0)
    } else NA_integer_
    zero_weight_fallback_used <- if ("fallback_flag" %in% names(d)) {
      any(isTRUE(d$fallback_flag) | d$fallback_flag %in% TRUE, na.rm = TRUE)
    } else FALSE

    qa_status <- if (!is.finite(observed) || !is.finite(allocated_sum)) {
      "fail"
    } else if (!is.finite(mass_rel_error)) {
      "fail"
    } else if (abs(mass_rel_error) <= tolerance_rel) {
      if (zero_weight_fallback_used) "warn" else "pass"
    } else {
      "fail"
    }

    k <- k + 1L
    row <- list(
      observed_population = observed,
      allocated_population_sum = allocated_sum,
      mass_error = mass_error,
      mass_rel_error = mass_rel_error,
      n_cells_intersecting = as.integer(n_cells_intersecting),
      n_cells_positive_weight = as.integer(n_cells_positive_weight),
      zero_weight_fallback_used = isTRUE(zero_weight_fallback_used),
      qa_status = qa_status
    )
    for (nm in keys) row[[nm]] <- d[[nm]][1]
    rows[[k]] <- row
  }
  rows <- rows[seq_len(k)]
  diag_df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  if (!is.null(polygon_panel) && nrow(polygon_panel) > 0) {
    panel_df <- as.data.frame(if (inherits(polygon_panel, "sf")) sf::st_drop_geometry(polygon_panel) else polygon_panel)
    poly_keys <- build_polygon_key_table(panel_df)
    if (length(keys) > 0 && nrow(poly_keys) > 0) {
      if (polygon_total_col %in% names(panel_df)) {
        totals <- unique(panel_df[, c(keys, polygon_total_col), drop = FALSE])
        names(totals)[names(totals) == polygon_total_col] <- "observed_population_panel"
        poly_keys <- merge(poly_keys, totals, by = keys, all.x = TRUE, sort = FALSE)
      }
      diag_df <- merge_with_polygon_keys(diag_df, poly_keys)

      missing_rows <- is.na(diag_df$qa_status)
      if (any(missing_rows)) {
        if ("observed_population_panel" %in% names(diag_df)) {
          diag_df$observed_population[missing_rows] <- diag_df$observed_population_panel[missing_rows]
        }
        diag_df$allocated_population_sum[missing_rows] <- 0
        diag_df$mass_error[missing_rows] <- ifelse(
          is.finite(diag_df$observed_population[missing_rows]),
          -diag_df$observed_population[missing_rows],
          NA_real_
        )
        diag_df$mass_rel_error[missing_rows] <- ifelse(
          is.finite(diag_df$mass_error[missing_rows]) & is.finite(diag_df$observed_population[missing_rows]),
          safe_rel_error(diag_df$mass_error[missing_rows], diag_df$observed_population[missing_rows]),
          NA_real_
        )
        diag_df$n_cells_intersecting[missing_rows] <- 0L
        diag_df$n_cells_positive_weight[missing_rows] <- 0L
        diag_df$zero_weight_fallback_used[missing_rows] <- FALSE
        diag_df$qa_status[missing_rows] <- "fail"
      }

      if ("observed_population_panel" %in% names(diag_df)) {
        keep_obs <- is.na(diag_df$observed_population) & is.finite(diag_df$observed_population_panel)
        diag_df$observed_population[keep_obs] <- diag_df$observed_population_panel[keep_obs]
        diag_df$observed_population_panel <- NULL
      }
    }
  }

  # Stable column order.
  extra <- setdiff(names(diag_df), out_cols)
  diag_df[, c(out_cols[out_cols %in% names(diag_df)], extra), drop = FALSE]
}

allocation_diagnostics_summary <- function(diagnostics_df, tolerance_rel = 1e-8) {
  if (!is.data.frame(diagnostics_df) || nrow(diagnostics_df) == 0) {
    return(list(
      max_abs_mass_rel_error = NA_real_,
      n_zero_weight_fallback = 0L,
      n_empty_support_polygons = 0L,
      n_fail = 0L
    ))
  }
  rel_err <- abs(as.numeric(diagnostics_df$mass_rel_error))
  rel_err <- rel_err[is.finite(rel_err)]
  list(
    max_abs_mass_rel_error = if (length(rel_err) == 0) NA_real_ else max(rel_err),
    n_zero_weight_fallback = sum(isTRUE(diagnostics_df$zero_weight_fallback_used) | diagnostics_df$zero_weight_fallback_used %in% TRUE, na.rm = TRUE),
    n_empty_support_polygons = sum(as.integer(diagnostics_df$n_cells_intersecting) == 0L, na.rm = TRUE),
    n_fail = sum(as.character(diagnostics_df$qa_status) == "fail", na.rm = TRUE),
    tolerance_rel = tolerance_rel
  )
}

#' Write allocation diagnostics to parquet/csv
#'
#' @param diagnostics_df Diagnostics table from `validate_mass_preservation()`.
#' @param model_id Identifier used in file naming.
#' @param year Optional scalar year; when NULL uses `_all`.
#' @param output_dir Output directory path.
#' @param root_dir Project root.
#' @param prefer_parquet Write parquet when `arrow` is installed.
#' @return Character path of written file.
write_allocation_diagnostics <- function(diagnostics_df,
                                         model_id = "uniform_area",
                                         year = NULL,
                                         output_dir = "data/final/diagnostics",
                                         root_dir = ".",
                                         prefer_parquet = TRUE) {
  if (!is.data.frame(diagnostics_df)) {
    stop("diagnostics_df must be a data.frame", call. = FALSE)
  }

  out_dir <- file.path(root_dir, output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  year_tag <- if (is.null(year)) "all" else as.character(as.integer(year))
  base <- file.path(out_dir, paste0("allocation_diagnostics_", year_tag, "_", model_id))

  if (isTRUE(prefer_parquet) && requireNamespace("arrow", quietly = TRUE)) {
    path <- paste0(base, ".parquet")
    arrow::write_parquet(diagnostics_df, path)
    return(path)
  }

  path <- paste0(base, ".csv")
  utils::write.csv(diagnostics_df, path, row.names = FALSE)
  path
}

#' Fail fast when constrained-allocation QA does not pass
#'
#' @param diagnostics_df Allocation diagnostics table.
#' @param tolerance_rel Relative error tolerance (for summary metadata only).
#' @return Invisible diagnostics summary list.
assert_allocation_qc <- function(diagnostics_df, tolerance_rel = 1e-8) {
  summary <- allocation_diagnostics_summary(diagnostics_df, tolerance_rel = tolerance_rel)
  has_fail <- isTRUE(summary$n_fail > 0)
  if (has_fail) {
    stop(
      sprintf(
        "[allocation QA] failed: n_fail=%d, max_abs_mass_rel_error=%s, n_zero_weight_fallback=%d, n_empty_support_polygons=%d",
        summary$n_fail,
        format(summary$max_abs_mass_rel_error, scientific = TRUE),
        summary$n_zero_weight_fallback,
        summary$n_empty_support_polygons
      ),
      call. = FALSE
    )
  }
  invisible(summary)
}

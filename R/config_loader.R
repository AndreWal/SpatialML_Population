`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x)) && any(nzchar(names(x)))
}

is_single_input_object <- function(x) {
  is_named_list(x) && any(c("path", "format", "columns") %in% names(x))
}

new_issue <- function(country, section, field, message) {
  list(
    country = country %||% "UNKNOWN",
    section = section %||% "general",
    field = field %||% "unknown",
    message = message
  )
}

format_issues <- function(issues) {
  lines <- vapply(
    issues,
    function(x) {
      sprintf(
        "- country=%s | section=%s | field=%s | %s",
        x$country,
        x$section,
        x$field,
        x$message
      )
    },
    character(1)
  )
  paste(c("Configuration validation failed:", lines), collapse = "\n")
}

required_columns_for_input <- function(input_type) {
  if (identical(input_type, "tabular")) {
    return(c("admin_id_raw", "admin_name", "year", "population"))
  }
  if (identical(input_type, "geometry")) {
    return(c("admin_id_raw", "admin_name"))
  }
  character(0)
}

as_input_list <- function(x, input_type) {
  if (is.null(x)) {
    return(list())
  }
  if (is_single_input_object(x)) {
    x$id <- x$id %||% paste0(input_type, "_1")
    return(list(x))
  }
  if (is.list(x)) {
    out <- x
    for (i in seq_along(out)) {
      if (is.null(out[[i]]$id)) {
        out[[i]]$id <- paste0(input_type, "_", i)
      }
    }
    return(out)
  }
  list()
}

default_assemblies <- function(cfg, country_code) {
  tabular_ids <- vapply(cfg$inputs$tabular, `[[`, character(1), "id")
  geometry_ids <- vapply(cfg$inputs$geometry, `[[`, character(1), "id")

  list(list(
    id = "default_assembly",
    target_unit_id = paste0(country_code, "_HARMONIZED"),
    tabular_recipe = list(
      strategy = if (length(tabular_ids) > 1) "stack_then_resolve" else "single",
      use_inputs = unname(tabular_ids)
    ),
    geometry_recipe = list(
      strategy = if (length(geometry_ids) > 1) "by_year" else "single",
      use_inputs = unname(geometry_ids),
      year_map = if (length(geometry_ids) > 1) {
        stats::setNames(unname(geometry_ids), unname(geometry_ids))
      } else {
        NULL
      }
    )
  ))
}

normalize_country_config <- function(cfg, file_country) {
  cfg <- cfg %||% list()
  cfg$country <- cfg$country %||% list()
  cfg$country$iso3 <- cfg$country$iso3 %||% file_country
  cfg$country$enabled <- cfg$country$enabled %||% TRUE

  cfg$inputs <- cfg$inputs %||% list()
  cfg$inputs$tabular <- as_input_list(cfg$inputs$tabular, "tabular")
  cfg$inputs$geometry <- as_input_list(cfg$inputs$geometry, "geometry")

  cfg$harmonization <- cfg$harmonization %||% list()
  needs_default_crosswalk <- length(cfg$harmonization) > 0 &&
    !is.null(cfg$harmonization$tabular_join_key) &&
    !is.null(cfg$harmonization$polygon_join_key)
  if (needs_default_crosswalk && is.null(cfg$harmonization$crosswalk_file)) {
    cfg$harmonization$crosswalk_file <- file.path(
      "config",
      "crosswalks",
      paste0(cfg$country$iso3, ".csv")
    )
  }

  cfg$assemblies <- cfg$assemblies %||% default_assemblies(cfg, cfg$country$iso3)
  cfg
}

validate_strategy_name <- function(country, section, field, strategy) {
  allowed <- c("single", "by_year", "stack_then_resolve")
  if (is.null(strategy) || !nzchar(strategy)) {
    return(new_issue(country, section, field, "missing required field"))
  }
  if (!strategy %in% allowed) {
    return(new_issue(
      country,
      section,
      field,
      sprintf("unsupported strategy: %s (allowed: %s)", strategy, paste(allowed, collapse = ", "))
    ))
  }
  NULL
}

validate_input_entry <- function(entry, country, input_type, idx, root_dir) {
  issues <- list()
  section <- paste0("inputs.", input_type, "[", idx, "]")

  for (field in c("id", "format", "columns", "path")) {
    if (is.null(entry[[field]]) || (is.character(entry[[field]]) && !nzchar(entry[[field]]))) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        section,
        field,
        "missing required field"
      )
    }
  }

  if (!is.null(entry$columns) && !is.list(entry$columns)) {
    issues[[length(issues) + 1]] <- new_issue(
      country,
      section,
      "columns",
      "must be a named mapping"
    )
  }

  req_cols <- required_columns_for_input(input_type)
  if (is.list(entry$columns)) {
    missing_cols <- setdiff(req_cols, names(entry$columns))
    for (col_name in missing_cols) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        section,
        paste0("columns.", col_name),
        "missing required column mapping"
      )
    }
  }

  if (!is.null(entry$path) && is.character(entry$path) && nzchar(entry$path)) {
    full_path <- file.path(root_dir, entry$path)
    if (!file.exists(full_path)) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        section,
        "path",
        sprintf("file does not exist: %s", entry$path)
      )
    }
  }

  issues
}

validate_assemblies <- function(cfg, country) {
  issues <- list()
  assemblies <- cfg$assemblies
  if (!is.list(assemblies) || length(assemblies) == 0) {
    issues[[length(issues) + 1]] <- new_issue(
      country,
      "assemblies",
      "assemblies",
      "must contain at least one assembly recipe"
    )
    return(issues)
  }

  tabular_ids <- vapply(cfg$inputs$tabular, `[[`, character(1), "id")
  geometry_ids <- vapply(cfg$inputs$geometry, `[[`, character(1), "id")

  for (i in seq_along(assemblies)) {
    asm <- assemblies[[i]]
    section <- paste0("assemblies[", i, "]")
    if (is.null(asm$id) || !nzchar(asm$id)) {
      issues[[length(issues) + 1]] <- new_issue(country, section, "id", "missing required field")
    }
    issue <- validate_strategy_name(country, section, "tabular_recipe.strategy", asm$tabular_recipe$strategy)
    if (!is.null(issue)) {
      issues[[length(issues) + 1]] <- issue
    }
    issue <- validate_strategy_name(country, section, "geometry_recipe.strategy", asm$geometry_recipe$strategy)
    if (!is.null(issue)) {
      issues[[length(issues) + 1]] <- issue
    }

    if (is.null(asm$tabular_recipe$use_inputs) || length(asm$tabular_recipe$use_inputs) == 0) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        section,
        "tabular_recipe.use_inputs",
        "must list at least one input id"
      )
    } else {
      bad <- setdiff(unlist(asm$tabular_recipe$use_inputs), tabular_ids)
      for (id in bad) {
        issues[[length(issues) + 1]] <- new_issue(
          country,
          section,
          "tabular_recipe.use_inputs",
          sprintf("unknown tabular input id: %s", id)
        )
      }
    }

    if (is.null(asm$geometry_recipe$use_inputs) || length(asm$geometry_recipe$use_inputs) == 0) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        section,
        "geometry_recipe.use_inputs",
        "must list at least one input id"
      )
    } else {
      bad <- setdiff(unlist(asm$geometry_recipe$use_inputs), geometry_ids)
      for (id in bad) {
        issues[[length(issues) + 1]] <- new_issue(
          country,
          section,
          "geometry_recipe.use_inputs",
          sprintf("unknown geometry input id: %s", id)
        )
      }
    }

    if (identical(asm$geometry_recipe$strategy, "by_year")) {
      if (is.null(asm$geometry_recipe$year_map) || length(asm$geometry_recipe$year_map) == 0) {
        issues[[length(issues) + 1]] <- new_issue(
          country,
          section,
          "geometry_recipe.year_map",
          "required when geometry strategy is by_year"
        )
      } else {
        map_ids <- unname(unlist(asm$geometry_recipe$year_map))
        bad <- setdiff(map_ids, geometry_ids)
        for (id in bad) {
          issues[[length(issues) + 1]] <- new_issue(
            country,
            section,
            "geometry_recipe.year_map",
            sprintf("year_map references unknown geometry input id: %s", id)
          )
        }
      }
    }

    if (identical(asm$tabular_recipe$strategy, "by_year")) {
      if (is.null(asm$tabular_recipe$year_map) || length(asm$tabular_recipe$year_map) == 0) {
        issues[[length(issues) + 1]] <- new_issue(
          country,
          section,
          "tabular_recipe.year_map",
          "required when tabular strategy is by_year"
        )
      } else {
        map_ids <- unname(unlist(asm$tabular_recipe$year_map))
        bad <- setdiff(map_ids, tabular_ids)
        for (id in bad) {
          issues[[length(issues) + 1]] <- new_issue(
            country,
            section,
            "tabular_recipe.year_map",
            sprintf("year_map references unknown tabular input id: %s", id)
          )
        }
      }
    }
  }

  issues
}

validate_source_registry <- function(root_dir, features_file) {
  issues <- list()
  features_path <- file.path(root_dir, features_file)
  if (!file.exists(features_path)) {
    issues[[length(issues) + 1]] <- new_issue(
      "GLOBAL",
      "sources",
      "features_file",
      sprintf("missing file: %s", features_file)
    )
    return(issues)
  }

  features_cfg <- yaml::read_yaml(features_path, eval.expr = FALSE)
  registry <- features_cfg$features_registry %||% list()
  for (i in seq_along(registry)) {
    src_cfg <- registry[[i]]$source_config
    if (is.null(src_cfg) || !nzchar(src_cfg)) {
      issues[[length(issues) + 1]] <- new_issue(
        "GLOBAL",
        paste0("sources.features_registry[", i, "]"),
        "source_config",
        "missing required field"
      )
      next
    }
    src_path <- file.path(root_dir, src_cfg)
    if (!file.exists(src_path)) {
      issues[[length(issues) + 1]] <- new_issue(
        "GLOBAL",
        paste0("sources.features_registry[", i, "]"),
        "source_config",
        sprintf("file does not exist: %s", src_cfg)
      )
    }
  }
  issues
}

validate_country_configs <- function(
  countries_dir = file.path("config", "countries"),
  root_dir = ".",
  countries = NULL,
  features_file = file.path("config", "sources", "features.yml")
) {
  files <- list.files(
    path = file.path(root_dir, countries_dir),
    pattern = "\\.yml$",
    full.names = TRUE
  )
  if (!is.null(countries)) {
    keep <- tools::file_path_sans_ext(basename(files)) %in% countries
    files <- files[keep]
  }
  issues <- list()
  configs <- list()

  for (file in files) {
    file_country <- tools::file_path_sans_ext(basename(file))
    raw_cfg <- yaml::read_yaml(file, eval.expr = FALSE)
    if (is.null(raw_cfg) || length(raw_cfg) == 0) {
      issues[[length(issues) + 1]] <- new_issue(
        file_country,
        "country",
        "root",
        "empty configuration file"
      )
      next
    }
    cfg <- normalize_country_config(raw_cfg, file_country)
    country <- cfg$country$iso3 %||% file_country
    configs[[country]] <- cfg

    if (is.null(cfg$inputs) || length(cfg$inputs) == 0) {
      issues[[length(issues) + 1]] <- new_issue(country, "inputs", "inputs", "missing inputs section")
      next
    }

    for (input_type in c("tabular", "geometry")) {
      entries <- cfg$inputs[[input_type]]
      if (!is.list(entries) || length(entries) == 0) {
        issues[[length(issues) + 1]] <- new_issue(
          country,
          paste0("inputs.", input_type),
          input_type,
          "must contain at least one input entry"
        )
      } else {
        ids <- vapply(entries, `[[`, character(1), "id")
        if (anyDuplicated(ids)) {
          dup <- unique(ids[duplicated(ids)])
          for (id in dup) {
            issues[[length(issues) + 1]] <- new_issue(
              country,
              paste0("inputs.", input_type),
              "id",
              sprintf("duplicate input id: %s", id)
            )
          }
        }
        for (i in seq_along(entries)) {
          entry_issues <- validate_input_entry(entries[[i]], country, input_type, i, root_dir)
          issues <- c(issues, entry_issues)
        }
      }
    }

    crosswalk <- cfg$harmonization$crosswalk_file
    if (!is.null(crosswalk) && nzchar(crosswalk) && !file.exists(file.path(root_dir, crosswalk))) {
      issues[[length(issues) + 1]] <- new_issue(
        country,
        "harmonization",
        "crosswalk_file",
        sprintf("file does not exist: %s", crosswalk)
      )
    }

    issues <- c(issues, validate_assemblies(cfg, country))
  }

  issues <- c(issues, validate_source_registry(root_dir, features_file))

  if (length(issues) > 0) {
    stop(format_issues(issues), call. = FALSE)
  }

  invisible(configs)
}

read_enabled_countries <- function(project_file = file.path("config", "global", "project.yml"), root_dir = ".") {
  cfg <- yaml::read_yaml(file.path(root_dir, project_file), eval.expr = FALSE)
  enabled <- cfg$countries$enabled %||% character(0)
  unlist(enabled)
}

validate_enabled_country_configs <- function(
  root_dir = ".",
  project_file = file.path("config", "global", "project.yml"),
  countries_dir = file.path("config", "countries"),
  features_file = file.path("config", "sources", "features.yml")
) {
  enabled <- read_enabled_countries(project_file = project_file, root_dir = root_dir)
  validate_country_configs(
    countries_dir = countries_dir,
    root_dir = root_dir,
    countries = enabled,
    features_file = features_file
  )
  TRUE
}

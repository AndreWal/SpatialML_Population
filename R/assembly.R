`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

stop_assembly_error <- function(country, section, field, message) {
  stop(
    sprintf(
      "country=%s | section=%s | field=%s | %s",
      country %||% "UNKNOWN",
      section %||% "unknown",
      field %||% "unknown",
      message
    ),
    call. = FALSE
  )
}

rename_to_canonical <- function(df, columns, required, country, section) {
  out <- df
  for (canon in required) {
    src <- columns[[canon]]
    if (is.null(src) || !nzchar(src)) {
      stop_assembly_error(country, section, paste0("columns.", canon), "missing mapping")
    }
    if (!src %in% names(out)) {
      stop_assembly_error(country, section, paste0("columns.", canon), sprintf("source column not found: %s", src))
    }
    names(out)[names(out) == src] <- canon
  }
  out
}

attach_year_from_input <- function(df, input_cfg, country, section) {
  if ("year" %in% names(df)) {
    df$year <- as.integer(df$year)
    return(df)
  }

  years <- input_cfg$valid_years %||% input_cfg$years %||% input_cfg$year_filter
  if (is.null(years) || length(years) != 1) {
    stop_assembly_error(
      country,
      section,
      "year",
      "year missing and no single valid_years/years/year_filter value available"
    )
  }
  df$year <- as.integer(years[[1]])
  df
}

read_tabular_input <- function(input_cfg, root_dir, country, section) {
  df <- read_tabular(input_cfg, root_dir = root_dir, country = country, section = section)
  df <- rename_to_canonical(
    df = df,
    columns = input_cfg$columns %||% list(),
    required = c("admin_id_raw", "admin_name", "year", "population"),
    country = country,
    section = section
  )
  df <- attach_year_from_input(df, input_cfg, country, section)
  keep <- c("admin_id_raw", "admin_name", "year", "population")
  df <- df[, keep, drop = FALSE]
  df$admin_id_raw <- as.character(df$admin_id_raw)
  df$admin_name <- as.character(df$admin_name)
  df$population <- as.numeric(df$population)
  df
}

read_geometry_input <- function(input_cfg, root_dir, country, section, require_year = FALSE) {
  g <- read_geometry(input_cfg, root_dir = root_dir, country = country, section = section)
  g <- rename_to_canonical(
    df = g,
    columns = input_cfg$columns %||% list(),
    required = c("admin_id_raw", "admin_name"),
    country = country,
    section = section
  )
  if ("year" %in% names(g)) {
    g$year <- as.integer(g$year)
  } else {
    years <- input_cfg$valid_years %||% input_cfg$years %||% input_cfg$year_filter
    if (!is.null(years) && length(years) == 1) {
      g$year <- as.integer(years[[1]])
    } else if (require_year) {
      stop_assembly_error(
        country,
        section,
        "year",
        "year missing and no single valid_years/years/year_filter value available"
      )
    }
  }
  g$admin_id_raw <- as.character(g$admin_id_raw)
  g$admin_name <- as.character(g$admin_name)
  g
}

resolve_tabular_duplicates <- function(df) {
  ord <- order(df$.priority, na.last = TRUE)
  df <- df[ord, , drop = FALSE]
  key <- paste(df$admin_id_raw, df$year, sep = "||")
  df <- df[!duplicated(key), , drop = FALSE]
  df$.priority <- NULL
  rownames(df) <- NULL
  df
}

resolve_geometry_duplicates <- function(g) {
  ord <- order(g$.priority, na.last = TRUE)
  g <- g[ord, ]
  key <- if ("year" %in% names(g)) {
    paste(g$admin_id_raw, g$year, sep = "||")
  } else {
    as.character(g$admin_id_raw)
  }
  g <- g[!duplicated(key), ]
  g$.priority <- NULL
  g
}

apply_by_year <- function(list_obj, year_map, id_field, country, section) {
  out <- list()
  if (is.null(year_map) || length(year_map) == 0) {
    stop_assembly_error(country, section, "year_map", "required when strategy is by_year")
  }
  for (year_name in names(year_map)) {
    input_id <- as.character(year_map[[year_name]])
    idx <- match(input_id, vapply(list_obj, `[[`, character(1), id_field))
    if (is.na(idx)) {
      stop_assembly_error(country, section, "year_map", sprintf("references unknown input id: %s", input_id))
    }
    obj <- list_obj[[idx]]$data
    obj$year <- as.integer(year_name)
    out[[length(out) + 1]] <- obj
  }
  out
}

assemble_tabular_data <- function(tab_inputs, recipe, root_dir, country, section = "assemblies.tabular_recipe") {
  ids <- recipe$use_inputs %||% character(0)
  strategy <- recipe$strategy %||% "single"

  selected <- list()
  for (i in seq_along(tab_inputs)) {
    if (tab_inputs[[i]]$id %in% ids) {
      selected[[length(selected) + 1]] <- list(
        id = tab_inputs[[i]]$id,
        data = read_tabular_input(tab_inputs[[i]], root_dir, country, paste0(section, ".", tab_inputs[[i]]$id))
      )
    }
  }
  if (length(selected) == 0) {
    stop_assembly_error(country, section, "use_inputs", "no tabular inputs selected")
  }

  if (identical(strategy, "single")) {
    if (length(selected) != 1) {
      stop_assembly_error(country, section, "strategy", "single requires exactly one input")
    }
    return(selected[[1]]$data)
  }

  if (identical(strategy, "by_year")) {
    pieces <- apply_by_year(selected, recipe$year_map, "id", country, section)
    return(do.call(rbind, pieces))
  }

  if (identical(strategy, "stack_then_resolve")) {
    out <- list()
    for (i in seq_along(selected)) {
      d <- selected[[i]]$data
      d$.priority <- i
      out[[i]] <- d
    }
    return(resolve_tabular_duplicates(do.call(rbind, out)))
  }

  stop_assembly_error(country, section, "strategy", sprintf("unsupported strategy: %s", strategy))
}

assemble_geometry_data <- function(geom_inputs, recipe, root_dir, country, section = "assemblies.geometry_recipe") {
  ids <- recipe$use_inputs %||% character(0)
  strategy <- recipe$strategy %||% "single"

  selected <- list()
  for (i in seq_along(geom_inputs)) {
    if (geom_inputs[[i]]$id %in% ids) {
      selected[[length(selected) + 1]] <- list(
        id = geom_inputs[[i]]$id,
        data = read_geometry_input(
          geom_inputs[[i]],
          root_dir,
          country,
          paste0(section, ".", geom_inputs[[i]]$id),
          require_year = FALSE
        )
      )
    }
  }
  if (length(selected) == 0) {
    stop_assembly_error(country, section, "use_inputs", "no geometry inputs selected")
  }

  if (identical(strategy, "single")) {
    if (length(selected) != 1) {
      stop_assembly_error(country, section, "strategy", "single requires exactly one input")
    }
    return(selected[[1]]$data)
  }

  if (identical(strategy, "by_year")) {
    pieces <- apply_by_year(selected, recipe$year_map, "id", country, section)
    return(do.call(rbind, pieces))
  }

  if (identical(strategy, "stack_then_resolve")) {
    out <- list()
    for (i in seq_along(selected)) {
      g <- selected[[i]]$data
      g$.priority <- i
      out[[i]] <- g
    }
    return(resolve_geometry_duplicates(do.call(rbind, out)))
  }

  stop_assembly_error(country, section, "strategy", sprintf("unsupported strategy: %s", strategy))
}

assemble_country_panel <- function(country_cfg, root_dir = ".", assembly_id = NULL) {
  cfg <- country_cfg
  country <- cfg$country$iso3 %||% "UNKNOWN"
  assemblies <- cfg$assemblies %||% list()
  if (length(assemblies) == 0) {
    stop_assembly_error(country, "assemblies", "assemblies", "missing assembly definitions")
  }

  assembly <- assemblies[[1]]
  if (!is.null(assembly_id)) {
    idx <- match(assembly_id, vapply(assemblies, `[[`, character(1), "id"))
    if (is.na(idx)) {
      stop_assembly_error(country, "assemblies", "id", sprintf("assembly not found: %s", assembly_id))
    }
    assembly <- assemblies[[idx]]
  }

  tab <- assemble_tabular_data(
    tab_inputs = cfg$inputs$tabular,
    recipe = assembly$tabular_recipe,
    root_dir = root_dir,
    country = country,
    section = paste0("assemblies.", assembly$id, ".tabular_recipe")
  )
  geom <- assemble_geometry_data(
    geom_inputs = cfg$inputs$geometry,
    recipe = assembly$geometry_recipe,
    root_dir = root_dir,
    country = country,
    section = paste0("assemblies.", assembly$id, ".geometry_recipe")
  )

  if ("year" %in% names(geom)) {
    panel <- merge(
      x = tab,
      y = geom,
      by = c("admin_id_raw", "year"),
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    panel <- merge(
      x = tab,
      y = geom,
      by = "admin_id_raw",
      all.x = TRUE,
      sort = FALSE
    )
  }

  if (!inherits(panel, "sf")) {
    panel <- sf::st_as_sf(panel)
  }
  panel$country_code <- country
  panel
}

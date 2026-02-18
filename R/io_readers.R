`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

stop_cfg_error <- function(country, section, field, message) {
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

require_cfg_fields <- function(cfg, fields, country, section) {
  for (field in fields) {
    val <- cfg[[field]]
    if (is.null(val) || (is.character(val) && !nzchar(val))) {
      stop_cfg_error(country, section, field, "missing required field")
    }
  }
}

normalize_tabular_format <- function(format) {
  fmt <- tolower(format %||% "")
  if (!fmt %in% c("csv", "parquet", "xlsx")) {
    stop(sprintf("unsupported tabular format: %s", format), call. = FALSE)
  }
  fmt
}

normalize_geometry_format <- function(format) {
  fmt <- tolower(format %||% "")
  if (fmt %in% c("shapefile")) {
    return("shp")
  }
  if (!fmt %in% c("shp", "gpkg", "geojson")) {
    stop(sprintf("unsupported geometry format: %s", format), call. = FALSE)
  }
  fmt
}

read_tabular <- function(input_cfg, root_dir = ".", country = "UNKNOWN", section = "inputs.tabular") {
  require_cfg_fields(input_cfg, c("path", "format"), country, section)
  format <- tryCatch(
    normalize_tabular_format(input_cfg$format),
    error = function(e) stop_cfg_error(country, section, "format", conditionMessage(e))
  )
  path <- file.path(root_dir, input_cfg$path)
  if (!file.exists(path)) {
    stop_cfg_error(country, section, "path", sprintf("file does not exist: %s", input_cfg$path))
  }

  out <- switch(
    format,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    parquet = as.data.frame(arrow::read_parquet(path)),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = input_cfg$sheet %||% 1))
  )
  out
}

read_geometry <- function(input_cfg, root_dir = ".", country = "UNKNOWN", section = "inputs.geometry") {
  require_cfg_fields(input_cfg, c("path", "format"), country, section)
  format <- tryCatch(
    normalize_geometry_format(input_cfg$format),
    error = function(e) stop_cfg_error(country, section, "format", conditionMessage(e))
  )
  path <- file.path(root_dir, input_cfg$path)
  if (!file.exists(path)) {
    stop_cfg_error(country, section, "path", sprintf("file does not exist: %s", input_cfg$path))
  }

  layer <- input_cfg$layer %||% NULL
  if (is.null(layer)) {
    return(sf::st_read(path, quiet = TRUE))
  }
  sf::st_read(path, layer = layer, quiet = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

read_yaml_file <- function(path) {
  yaml::read_yaml(path, eval.expr = FALSE)
}

load_country_config <- function(country_code, root_dir = ".") {
  path <- file.path(root_dir, "config", "countries", paste0(country_code, ".yml"))
  if (!file.exists(path)) {
    stop(sprintf("Missing country config: %s", path), call. = FALSE)
  }
  cfg <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(cfg) || length(cfg) == 0) {
    stop(sprintf("Empty country config: %s", path), call. = FALSE)
  }
  normalize_country_config(cfg, country_code)
}

country_inputs_exist <- function(country_cfg, root_dir = ".") {
  tab_ok <- vapply(
    country_cfg$inputs$tabular,
    function(x) file.exists(file.path(root_dir, x$path)),
    logical(1)
  )
  geom_ok <- vapply(
    country_cfg$inputs$geometry,
    function(x) file.exists(file.path(root_dir, x$path)),
    logical(1)
  )
  all(tab_ok) && all(geom_ok)
}

default_mock_years <- function(country_cfg) {
  years <- country_cfg$inputs$tabular[[1]]$year_filter %||%
    country_cfg$inputs$tabular[[1]]$years %||%
    c(2000L, 2010L)
  as.integer(head(years, 2))
}

make_mock_country_panel <- function(country_cfg) {
  country <- country_cfg$country$iso3 %||% "UNK"
  years <- default_mock_years(country_cfg)
  units <- data.frame(
    admin_id_raw = c("1", "2"),
    admin_name = c("Mock A", "Mock B"),
    stringsAsFactors = FALSE
  )
  base <- merge(units, data.frame(year = years), all = TRUE)
  base$population <- c(100, 200, 110, 210)[seq_len(nrow(base))]
  base$country_code <- country

  pt_a <- sf::st_point(c(8.5, 47.3))
  pt_b <- sf::st_point(c(8.6, 47.4))
  geom <- sf::st_sfc(rep(list(pt_a, pt_b), length.out = nrow(base)), crs = 4326)
  sf::st_sf(base, geometry = geom)
}

read_country_inputs <- function(country_cfg, root_dir = ".", mock_mode = TRUE) {
  if (!country_inputs_exist(country_cfg, root_dir = root_dir)) {
    if (isTRUE(mock_mode)) {
      return(make_mock_country_panel(country_cfg))
    }
    stop(sprintf("Missing raw input files for %s", country_cfg$country$iso3), call. = FALSE)
  }
  panel <- tryCatch(
    assemble_country_panel(country_cfg = country_cfg, root_dir = root_dir),
    error = function(e) {
      if (isTRUE(mock_mode)) return(make_mock_country_panel(country_cfg))
      stop(e)
    }
  )
  if (isTRUE(mock_mode) && any(sf::st_is_empty(panel))) {
    return(make_mock_country_panel(country_cfg))
  }
  panel
}

read_crosswalk <- function(country_cfg, panel_sf, root_dir = ".", mock_mode = TRUE) {
  identity_crosswalk <- function() {
    ids <- unique(as.character(panel_sf$admin_id_raw))
    nms <- unique(as.character(panel_sf$admin_name))
    nms <- c(nms, ids)[seq_along(ids)]
    data.frame(
      from_admin_id = ids,
      from_admin_name = nms,
      to_admin_id = ids,
      to_admin_name = nms,
      weight = 1,
      stringsAsFactors = FALSE
    )
  }

  crosswalk_file <- country_cfg$harmonization$crosswalk_file
  if (!is.null(crosswalk_file) && file.exists(file.path(root_dir, crosswalk_file))) {
    cw <- utils::read.csv(file.path(root_dir, crosswalk_file), stringsAsFactors = FALSE, check.names = FALSE)
    required <- c("from_admin_id", "to_admin_id", "to_admin_name")
    missing <- setdiff(required, names(cw))
    if (length(missing) > 0) {
      stop(sprintf("Crosswalk missing columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (nrow(cw) == 0) {
      return(identity_crosswalk())
    }
    return(cw)
  }

  if (!is.null(crosswalk_file) && !isTRUE(mock_mode)) {
    stop(sprintf("Crosswalk file not found for %s: %s", country_cfg$country$iso3, crosswalk_file), call. = FALSE)
  }

  identity_crosswalk()
}

resolve_admin_name <- function(merged) {
  nm <- merged$admin_name
  if (is.null(nm) && "admin_name.x" %in% names(merged)) nm <- merged$admin_name.x
  if (is.null(nm) && "admin_name.y" %in% names(merged)) nm <- merged$admin_name.y
  if (is.null(nm)) nm <- merged$admin_id_raw
  as.character(nm)
}

apply_unmatched_policy <- function(merged, policy = "fail", country = "UNK") {
  mapped <- !is.na(merged$to_admin_id) & merged$to_admin_id != ""
  unmatched_n <- sum(!mapped)
  if (unmatched_n == 0) {
    return(merged)
  }

  msg <- sprintf("%s unmatched admin rows for %s", unmatched_n, country)
  if (identical(policy, "fail")) stop(msg, call. = FALSE)
  if (identical(policy, "warn")) warning(msg, call. = FALSE)
  if (identical(policy, "drop")) return(merged[mapped, , drop = FALSE])
  merged
}

harmonize_keys <- function(panel_sf, country_cfg, root_dir = ".", mock_mode = TRUE) {
  cw <- read_crosswalk(country_cfg, panel_sf, root_dir = root_dir, mock_mode = mock_mode)
  cw$from_admin_id <- as.character(cw$from_admin_id)
  cw$to_admin_id <- as.character(cw$to_admin_id)
  if (is.null(cw$weight)) cw$weight <- 1
  cw$weight <- as.numeric(cw$weight)
  panel_sf$admin_id_raw <- as.character(panel_sf$admin_id_raw)

  merged <- merge(
    x = panel_sf,
    y = cw,
    by.x = "admin_id_raw",
    by.y = "from_admin_id",
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(merged) == 0) {
    stop(sprintf("No rows after crosswalk join for %s", country_cfg$country$iso3 %||% "UNK"), call. = FALSE)
  }

  # Optional temporal crosswalk filtering.
  if ("valid_from_year" %in% names(merged)) {
    from_ok <- is.na(merged$valid_from_year) | merged$year >= as.integer(merged$valid_from_year)
    to_ok <- if ("valid_to_year" %in% names(merged)) {
      is.na(merged$valid_to_year) | merged$year <= as.integer(merged$valid_to_year)
    } else {
      TRUE
    }
    merged <- merged[from_ok & to_ok, , drop = FALSE]
  }

  policy <- country_cfg$harmonization$unmatched_policy %||% "fail"
  merged <- apply_unmatched_policy(merged, policy = policy, country = country_cfg$country$iso3 %||% "UNK")

  raw_admin_name <- resolve_admin_name(merged)
  mapped <- !is.na(merged$to_admin_id) & merged$to_admin_id != ""
  merged$admin_unit_harmonized <- ifelse(mapped, merged$to_admin_id, merged$admin_id_raw)
  merged$admin_name_harmonized <- ifelse(
    mapped & !is.na(merged$to_admin_name) & merged$to_admin_name != "",
    merged$to_admin_name,
    raw_admin_name
  )

  if (is.null(merged$country_code)) {
    merged$country_code <- country_cfg$country$iso3 %||% "UNK"
  }

  merged$population_weighted <- as.numeric(merged$population) * ifelse(is.na(merged$weight), 1, merged$weight)
  keys <- paste(merged$country_code, merged$admin_unit_harmonized, merged$year, sep = "||")
  idx <- split(seq_len(nrow(merged)), keys)

  rows <- lapply(idx, function(i) {
    d <- merged[i, ]
    geom_one <- sf::st_geometry(d)[1]
    sf::st_sf(
      country_code = d$country_code[1],
      admin_unit_harmonized = d$admin_unit_harmonized[1],
      admin_name_harmonized = d$admin_name_harmonized[1],
      year = as.integer(d$year[1]),
      population = sum(as.numeric(d$population_weighted), na.rm = TRUE),
      geometry = geom_one
    )
  })

  out <- do.call(rbind, rows)
  coverage <- if (nrow(panel_sf) == 0) 1 else length(unique(merged$admin_id_raw[mapped])) / length(unique(panel_sf$admin_id_raw))
  attr(out, "join_coverage") <- coverage
  out
}

transform_to_canonical_crs <- function(panel_sf, canonical_crs) {
  if (is.na(sf::st_crs(panel_sf))) {
    sf::st_crs(panel_sf) <- 4326
  }
  sf::st_transform(panel_sf, crs = canonical_crs)
}

#' Validate and optionally fix geometries
#'
#' Checks `st_is_valid`, applies `st_make_valid` when `auto_fix_invalid` is TRUE,
#' and optionally casts MULTI* to single-part when `allow_multipart` is FALSE.
#'
#' @param panel_sf sf object with geometry column.
#' @param qa_cfg Parsed QA config list (from config/global/qa.yml).
#' @param country_code ISO3 country code for error messages.
#' @return sf object with valid geometries.
validate_and_fix_geometry <- function(panel_sf, qa_cfg, country_code = "UNK") {
  require_valid <- qa_cfg$qa$geometry$require_valid_geometry %||% FALSE
  auto_fix      <- qa_cfg$qa$geometry$auto_fix_invalid %||% FALSE
  allow_multi   <- qa_cfg$qa$geometry$allow_multipart %||% TRUE

  if (!require_valid) {
    return(panel_sf)
  }

  # Use GEOS (not s2) for deterministic validity checks across platforms.
  s2_was_on <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(s2_was_on), add = TRUE)
  sf::sf_use_s2(FALSE)

  invalid <- !sf::st_is_valid(panel_sf)
  n_invalid <- sum(invalid, na.rm = TRUE)

  if (n_invalid > 0L) {
    if (isTRUE(auto_fix)) {
      panel_sf <- sf::st_make_valid(panel_sf)
      still_invalid <- sum(!sf::st_is_valid(panel_sf), na.rm = TRUE)
      if (still_invalid > 0L) {
        stop(
          sprintf("Geometry fix failed (%s): %d geometries still invalid after st_make_valid",
                  country_code, still_invalid),
          call. = FALSE
        )
      }
    } else {
      stop(
        sprintf("QA failed (%s): %d invalid geometries (auto_fix_invalid=false)",
                country_code, n_invalid),
        call. = FALSE
      )
    }
  }

  if (!isTRUE(allow_multi)) {
    geom_types <- sf::st_geometry_type(panel_sf)
    has_multi  <- grepl("^MULTI", as.character(geom_types))
    if (any(has_multi)) {
      panel_sf <- suppressWarnings(sf::st_cast(panel_sf, "POLYGON"))
    }
  }

  panel_sf
}

validate_country_panel_qa <- function(panel_sf, panel_harmonized, country_cfg, qa_cfg, canonical_crs) {
  key_fields <- qa_cfg$qa$keys$unique_key %||% c("country_code", "admin_unit_harmonized", "year")
  key_df <- as.data.frame(sf::st_drop_geometry(panel_sf)[, key_fields, drop = FALSE])
  if (anyDuplicated(key_df) > 0) {
    stop(sprintf("QA failed (%s): duplicate unique key", country_cfg$country$iso3), call. = FALSE)
  }

  has_empty <- any(sf::st_is_empty(panel_sf))
  if (isTRUE(has_empty)) {
    n_empty <- sum(sf::st_is_empty(panel_sf))
    warning(
      sprintf("QA (%s): dropping %d rows with empty geometry (%d remain)",
              country_cfg$country$iso3, n_empty, nrow(panel_sf) - n_empty),
      call. = FALSE
    )
    panel_sf <- panel_sf[!sf::st_is_empty(panel_sf), ]
  }

  target_epsg <- sf::st_crs(canonical_crs)$epsg
  panel_epsg <- sf::st_crs(panel_sf)$epsg
  if (!isTRUE(identical(target_epsg, panel_epsg))) {
    stop(sprintf("QA failed (%s): CRS mismatch", country_cfg$country$iso3), call. = FALSE)
  }

  threshold <- country_cfg$qa_override$join_coverage_min %||% qa_cfg$qa$coverage$join_coverage_min %||% 0
  coverage <- attr(panel_harmonized, "join_coverage") %||% 1
  if (coverage < threshold) {
    stop(sprintf("QA failed (%s): join coverage %.3f < %.3f", country_cfg$country$iso3, coverage, threshold), call. = FALSE)
  }

  panel_sf
}

write_country_outputs <- function(panel_sf, country_code, final_data_dir, root_dir = ".") {
  out_dir <- file.path(root_dir, final_data_dir, country_code)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  gpkg_path <- file.path(out_dir, paste0(country_code, "_panel.gpkg"))
  parquet_path <- file.path(out_dir, paste0(country_code, "_panel.parquet"))

  sf::st_write(panel_sf, gpkg_path, quiet = TRUE, delete_dsn = TRUE)
  arrow::write_parquet(sf::st_drop_geometry(panel_sf), parquet_path)
  c(gpkg_path, parquet_path)
}

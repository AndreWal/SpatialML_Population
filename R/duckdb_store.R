`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Open a persistent DuckDB connection
#'
#' Creates the database directory if needed. By default uses an in-memory
#' database for testing; set db_path to a file for persistence.
#'
#' @param db_path Path to the DuckDB database file, or ":memory:".
#' @param root_dir Project root.
#' @return A DBI connection object.
open_duckdb <- function(db_path = ":memory:", root_dir = ".") {
  if (!identical(db_path, ":memory:")) {
    full_path <- file.path(root_dir, db_path)
    dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
    db_path <- full_path
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  con
}

#' Close a DuckDB connection
#'
#' @param con DBI connection object.
#' @return Invisible NULL.
close_duckdb <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

#' Write a data.frame (or sf) to a DuckDB table
#'
#' Drops geometry before writing. Use for intermediate tabular storage.
#'
#' @param con DBI connection.
#' @param df data.frame or sf object.
#' @param table_name Target table name.
#' @param overwrite Logical; drop existing table first.
#' @return Invisible table name.
write_to_duckdb <- function(con, df, table_name, overwrite = TRUE) {
  if (inherits(df, "sf")) {
    df <- sf::st_drop_geometry(df)
  }
  if (isTRUE(overwrite) && DBI::dbExistsTable(con, table_name)) {
    DBI::dbRemoveTable(con, table_name)
  }
  DBI::dbWriteTable(con, table_name, df)
  invisible(table_name)
}

#' Read a DuckDB table back to a data.frame
#'
#' @param con DBI connection.
#' @param table_name Table name.
#' @return A data.frame.
read_from_duckdb <- function(con, table_name) {
  DBI::dbReadTable(con, table_name)
}

#' Run a SQL query on DuckDB and return results
#'
#' @param con DBI connection.
#' @param sql SQL query string.
#' @return A data.frame of results.
query_duckdb <- function(con, sql) {
  DBI::dbGetQuery(con, sql)
}

#' Write a Parquet file from DuckDB using COPY
#'
#' Leverages DuckDB's built-in Parquet writer for efficient export.
#'
#' @param con DBI connection.
#' @param table_name Source table.
#' @param parquet_path Output Parquet file path.
#' @return Invisible path.
export_to_parquet <- function(con, table_name, parquet_path) {
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
  sql <- sprintf("COPY %s TO '%s' (FORMAT PARQUET)", table_name, parquet_path)
  DBI::dbExecute(con, sql)
  invisible(parquet_path)
}

#' Read a Parquet file directly into DuckDB
#'
#' Uses DuckDB's built-in Parquet reader.
#'
#' @param con DBI connection.
#' @param parquet_path Path to Parquet file.
#' @param table_name Target table name in DuckDB.
#' @return Invisible table name.
import_parquet <- function(con, parquet_path, table_name) {
  sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS SELECT * FROM read_parquet('%s')",
    table_name,
    parquet_path
  )
  DBI::dbExecute(con, sql)
  invisible(table_name)
}

#' Store all country panels in a DuckDB database
#'
#' Writes each country panel (without geometry) to a single combined table
#' and per-country tables.
#'
#' @param panels_list A named list of sf country panels (keyed by ISO3).
#' @param db_path DuckDB database path.
#' @param root_dir Project root.
#' @return Character vector of table names created.
store_panels_duckdb <- function(panels_list, db_path = "cache/panels.duckdb",
                                root_dir = ".") {
  con <- open_duckdb(db_path, root_dir = root_dir)
  on.exit(close_duckdb(con), add = TRUE)

  tables_created <- character(0)

  all_dfs <- list()
  for (country in names(panels_list)) {
    df <- sf::st_drop_geometry(panels_list[[country]])
    table_name <- paste0("panel_", tolower(country))
    write_to_duckdb(con, df, table_name)
    tables_created <- c(tables_created, table_name)
    all_dfs[[length(all_dfs) + 1]] <- df
  }

  # Combined table
  combined <- do.call(rbind, all_dfs)
  write_to_duckdb(con, combined, "panel_all")
  tables_created <- c(tables_created, "panel_all")

  tables_created
}

#' Load the combined panel from DuckDB
#'
#' @param db_path DuckDB database path.
#' @param root_dir Project root.
#' @return A data.frame of the combined panel.
load_combined_panel <- function(db_path = "cache/panels.duckdb", root_dir = ".") {
  con <- open_duckdb(db_path, root_dir = root_dir)
  on.exit(close_duckdb(con), add = TRUE)
  read_from_duckdb(con, "panel_all")
}

#' Run an aggregation query on the combined panel
#'
#' Convenience wrapper for common analytical queries.
#'
#' @param db_path DuckDB database path.
#' @param group_by Character vector of grouping columns.
#' @param agg_expr SQL aggregation expression (e.g., "SUM(population)").
#' @param root_dir Project root.
#' @return data.frame with aggregated results.
aggregate_panel <- function(db_path = "cache/panels.duckdb",
                            group_by = "country_code",
                            agg_expr = "SUM(population) AS total_pop",
                            root_dir = ".") {
  con <- open_duckdb(db_path, root_dir = root_dir)
  on.exit(close_duckdb(con), add = TRUE)
  group_clause <- paste(group_by, collapse = ", ")
  sql <- sprintf(
    "SELECT %s, %s FROM panel_all GROUP BY %s ORDER BY %s",
    group_clause, agg_expr, group_clause, group_clause
  )
  query_duckdb(con, sql)
}

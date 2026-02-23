library(terra)
library(sf)

panel <- readRDS("_targets/objects/country_panel_validated_f1934f8a7081266e")
cat("Panel rows:", nrow(panel), "\n")
is_empty <- sf::st_is_empty(panel)
panel_ne <- panel[!is_empty, ]
cat("Non-empty rows:", nrow(panel_ne), "\n")

sg_dir <- "data/intermediate/features/soilgrids"
tifs <- list.files(sg_dir, "\\.tif$", full.names = TRUE)
r_stack <- rast(tifs)
cat("Stack layers:", nlyr(r_stack), " CRS:", crs(r_stack, describe=TRUE)$code, "\n")

v <- vect(panel_ne)
cat("SpatVector extent:", as.vector(ext(v)), "\n")

cat("Test 1: 3 layers x 100 polys...\n")
r3 <- rast(tifs[1:3])
v100 <- vect(panel_ne[1:100, ])
vals <- tryCatch(
  terra::extract(r3, v100, fun = "mean", na.rm = TRUE, exact = TRUE),
  error = function(e) { cat("ERR:", e$message, "\n"); NULL }
)
if (!is.null(vals)) cat("  OK dims:", dim(vals), "\n")

cat("Test 2: 61 layers x 100 polys...\n")
vals <- tryCatch(
  terra::extract(r_stack, v100, fun = "mean", na.rm = TRUE, exact = TRUE),
  error = function(e) { cat("ERR:", e$message, "\n"); NULL }
)
if (!is.null(vals)) cat("  OK dims:", dim(vals), "\n")

cat("Test 3: 61 layers x ALL polys (sequential)...\n")
vals <- tryCatch(
  terra::extract(r_stack, v, fun = "mean", na.rm = TRUE, exact = TRUE),
  error = function(e) { cat("ERR:", e$message, "\n"); NULL }
)
if (!is.null(vals)) cat("  OK dims:", dim(vals), "\n")

cat("Test 4: mclapply with 2 workers, 200 polys...\n")
v200 <- vect(panel_ne[1:200, ])
sub_sf <- panel_ne[1:200, ]
result <- tryCatch(
  parallel::mclapply(
    list(1:100, 101:200),
    function(ix) {
      terra::extract(r_stack, vect(sub_sf[ix, ]), fun = "mean", na.rm = TRUE, exact = TRUE)
    },
    mc.cores = 2L
  ),
  error = function(e) { cat("mclapply ERR:", e$message, "\n"); NULL }
)
if (!is.null(result)) {
  for (i in seq_along(result)) {
    if (inherits(result[[i]], "try-error")) {
      cat("  Worker", i, "FAILED\n")
    } else {
      cat("  Worker", i, "OK dims:", dim(result[[i]]), "\n")
    }
  }
}

cat("\nAll tests complete.\n")

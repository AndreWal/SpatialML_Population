source(file.path("..", "..", "R", "grid_intersections.R"))
source(file.path("..", "..", "R", "dasymetric_allocation.R"))
source(file.path("..", "..", "R", "allocation_validation.R"))

test_that("build_polygon_grid_intersections computes exact overlap areas", {
  grid <- terra::rast(
    nrows = 1, ncols = 2,
    xmin = 0, xmax = 2, ymin = 0, ymax = 1,
    crs = "EPSG:3035"
  )

  poly <- sf::st_sf(
    data.frame(
      country_code = "TST",
      admin_unit_harmonized = "A",
      year = 2000L,
      population = 100
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1.5, 0), c(1.5, 1), c(0, 1), c(0, 0)))),
      crs = "EPSG:3035"
    )
  )

  ints <- build_polygon_grid_intersections(poly, grid, canonical_crs = "EPSG:3035", keep_geometry = FALSE)

  expect_equal(nrow(ints), 2)
  expect_equal(sort(round(ints$overlap_area_m2, 8)), c(0.5, 1.0))
  expect_true(all(ints$cell_area_m2 > 0))
})

test_that("uniform area allocation preserves mass and produces nonnegative counts", {
  ints <- data.frame(
    country_code = c("TST", "TST"),
    admin_unit_harmonized = c("A", "A"),
    year = c(2000L, 2000L),
    population = c(120, 120),
    cell_index = c(1L, 2L),
    cell_id = c("cell_1", "cell_2"),
    cell_area_m2 = c(1, 1),
    overlap_area_m2 = c(1, 3)
  )

  alloc <- allocate_uniform_by_area(ints)
  expect_true(all(alloc$pop_allocated >= 0))
  expect_equal(sum(alloc$pop_allocated), 120, tolerance = 1e-12)
  expect_equal(alloc$weight_norm, c(0.25, 0.75), tolerance = 1e-12)

  diag <- validate_mass_preservation(alloc, tolerance_rel = 1e-12)
  expect_equal(diag$qa_status, "pass")
  expect_equal(diag$mass_error, 0, tolerance = 1e-12)
})

test_that("score-based allocation falls back to uniform area for zero or NA support", {
  ints <- data.frame(
    country_code = c("TST", "TST", "TST", "TST"),
    admin_unit_harmonized = c("A", "A", "B", "B"),
    year = c(2000L, 2000L, 2000L, 2000L),
    population = c(100, 100, 50, 50),
    cell_index = c(1L, 2L, 3L, 4L),
    overlap_area_m2 = c(1, 3, 1, 1),
    score_raw = c(0, NA, 2, 0)
  )

  alloc <- allocate_constrained_from_scores(ints, score_col = "score_raw")

  grp_a <- alloc$admin_unit_harmonized == "A"
  expect_true(all(alloc$fallback_flag[grp_a]))
  expect_true(all(alloc$allocation_mode[grp_a] == "fallback_uniform_area"))
  expect_equal(sum(alloc$pop_allocated[grp_a]), 100, tolerance = 1e-12)
  expect_equal(alloc$weight_norm[grp_a], c(0.25, 0.75), tolerance = 1e-12)

  grp_b <- alloc$admin_unit_harmonized == "B"
  expect_false(any(alloc$fallback_flag[grp_b]))
  expect_equal(sum(alloc$pop_allocated[grp_b]), 50, tolerance = 1e-12)
  expect_true(all(alloc$pop_allocated[grp_b] >= 0))
})

test_that("uniform allocation handles zero-area support via equal-cell fallback", {
  ints <- data.frame(
    country_code = c("TST", "TST"),
    admin_unit_harmonized = c("Z", "Z"),
    year = c(2000L, 2000L),
    population = c(10, 10),
    cell_index = c(1L, 2L),
    overlap_area_m2 = c(0, 0)
  )

  alloc <- allocate_uniform_by_area(ints)
  expect_true(all(alloc$fallback_flag))
  expect_true(all(alloc$allocation_mode == "fallback_equal_cell"))
  expect_equal(alloc$weight_norm, c(0.5, 0.5), tolerance = 1e-12)
  expect_equal(sum(alloc$pop_allocated), 10, tolerance = 1e-12)
})

test_that("mass-preservation diagnostics report empty-support polygons", {
  panel <- sf::st_sf(
    data.frame(
      country_code = c("TST", "TST"),
      admin_unit_harmonized = c("A", "B"),
      year = c(2000L, 2000L),
      population = c(100, 200)
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
      crs = "EPSG:3035"
    )
  )

  alloc <- data.frame(
    country_code = c("TST", "TST"),
    admin_unit_harmonized = c("A", "A"),
    year = c(2000L, 2000L),
    cell_index = c(1L, 2L),
    population_polygon = c(100, 100),
    weight_norm = c(0.5, 0.5),
    pop_allocated = c(50, 50),
    fallback_flag = c(FALSE, FALSE)
  )

  diag <- validate_mass_preservation(alloc, polygon_panel = panel, tolerance_rel = 1e-12)
  expect_equal(nrow(diag), 2)
  expect_true(any(diag$admin_unit_harmonized == "B"))
  expect_equal(diag$n_cells_intersecting[diag$admin_unit_harmonized == "B"], 0L)
  expect_equal(diag$qa_status[diag$admin_unit_harmonized == "B"], "fail")
})

test_that("predict_unconstrained_intensity writes explicitly labeled raster", {
  tmp <- tempdir()
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:3035")
  terra::values(r) <- c(1, 2, 3, 4)

  path <- predict_unconstrained_intensity(
    score_raster = r,
    label = "global_2000",
    model_id = "uniform_area",
    output_dir = "preds",
    root_dir = tmp
  )

  expect_true(file.exists(path))
  expect_match(basename(path), "^global_2000_intensity_uniform_area\\.tif$")
})

test_that("prediction_raster_to_score and join_cell_scores_from_raster work", {
  source(file.path("..", "..", "R", "raster_predict.R"), local = TRUE)

  r <- terra::rast(nrows = 1, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 1, crs = "EPSG:3035")
  terra::values(r) <- c(log1p(0), log1p(2), -10)
  names(r) <- "prediction"

  score_r <- prediction_raster_to_score(r, list(transform = "log1p", needs_area = TRUE))
  expect_equal(round(terra::values(score_r)[1:2], 8), c(0, 2))
  expect_true(terra::values(score_r)[3] >= 0)

  ints <- data.frame(cell_index = c(1L, 2L, 3L))
  joined <- join_cell_scores_from_raster(ints, score_r)
  expect_true("score_raw" %in% names(joined))
  expect_equal(round(joined$score_raw[2], 8), 2)
})

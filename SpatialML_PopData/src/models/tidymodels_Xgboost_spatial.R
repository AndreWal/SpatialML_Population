library(tidyverse)
library(tidymodels)
library(xgboost)
library(spatialsample)
library(sf)
library(vip)

# Data

swdat = readRDS(paste0(getwd(), "/project/Data/processed/swdat.rds"))

# Prediction equation

predictors = colnames(swdat[,c(5:length(swdat))])

response = "firsh"

formula <- as.formula(paste(
  response,
  "~",
  paste(predictors, collapse = " + ")
))


recipe <- recipes::recipe(formula, data = swdat)

# Xgboost

xgb_model <- boost_tree(
  trees      = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  min_n      = tune(),
  loss_reduction = tune()
) |>
  set_engine("xgboost", nthread = 15) |>
  set_mode("regression")

workflow <- workflows::workflow() |>
  workflows::add_recipe(recipe) |>
  workflows::add_model(xgb_model)

# Tuning

keep_pred <- tune::control_resamples(save_pred = TRUE, save_workflow = TRUE)

random_folds <- rsample::vfold_cv(swdat, v = 4)

block_folds <- spatialsample::spatial_block_cv(sf::st_as_sf(swdat), v = 4, n = 4)
spatialsample::autoplot(block_folds)

# Grid

custom_grid <- grid_space_filling(
  trees(range = c(200, 1500)),
  learn_rate(range = c(-4, -1)),
  tree_depth(range = c(2, 10)),
  min_n(range = c(1, 50)),
  loss_reduction(range = c(-5, 2)),
  size = 60
)

# Fit model

xgb_random_tuned <- tune_grid(
  workflow,
  resamples = random_folds,
  grid      = custom_grid, 
  control   = control_grid(save_workflow = TRUE)
)


xgb_spatial_tuned <- tune_grid(
  workflow,
  resamples = block_folds,
  grid      = custom_grid,
  control   = control_grid(save_workflow = TRUE)
)

best_random_params  <- select_best(xgb_random_tuned)
best_spatial_params <- select_best(xgb_spatial_tuned)

final_random_wf <- finalize_workflow(workflow, best_random_params)  %>%
  fit(swdat)
final_spatial_wf <- finalize_workflow(workflow, best_spatial_params) %>%
  fit(swdat)

### German data

gerdat = readRDS(paste0(getwd(), "/project/Data/processed/gerdat.rds"))

gerdat$xgb   <- predict(final_random_wf,  gerdat)$.pred

gerdat$spxgb <- predict(final_spatial_wf, gerdat)$.pred

sqrt(mean((gerdat$firsh - gerdat$xgb)^2))

sqrt(mean((gerdat$firsh - gerdat$spxgb)^2))

test = gerdat |> select(firsh, xgb, spxgb) |> mutate(err1 = abs(firsh - xgb),err2 = abs(firsh - spxgb))

# Diagnostics

imp = extract_fit_parsnip(final_spatial_wf) |>
  vip::vip()
imp

imp = extract_fit_parsnip(final_random_wf) |>
  vip::vip()
imp

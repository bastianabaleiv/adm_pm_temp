library(tidyverse)
library(lubridate)
library(furrr)
library(rsample)
library(Metrics)

# Sets --------------------------------------------------------------------

apt_df <- readRDS('data/adm_pm_temp.rds')

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = 2,
                   "d" = 0,
                   "q" = 2) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = 1,
  "D" = 1,
  "Q" = 1,
  "period" = 7
)  %>%
  cross() %>%
  map(purrr::lift(c))

sarima_hp <- crossing(tibble("order" = order_list),
                      tibble("season" = season_list))

rm(order_list, season_list)

# Time series cross validation (Test set) ---------------------------------

tscv_data <- rsample::rolling_origin(
  data = apt_df,
  initial = 365 * 4,
  assess = 7,
  cumulative = FALSE,
  skip = 6
) %>%
  mutate(
    # Extract train set for each split
    train_set = map(splits, training),
    # Extract validate set for each split
    test_set = map(splits, testing),
    # Extract actual values to validate
    test_actual = map(test_set, ~ .x[["adm"]])
  )

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(sarima_hp)

# SARIMAX function --------------------------------------------------------

fit_sarimax <-
  function(train_set,
           test_set,
           order,
           season,
           ...) {
    fit_start <- Sys.time()
    
    sarimax <- stats::arima(
      train_set$adm,
      order = unlist(order),
      seasonal = unlist(season),
      xreg = train_set[, c("pm", "temp_avg")],
      optim.control = list(maxit = 1000)
    )
    
    fit_time <- Sys.time() - fit_start
    
    pred_sarimax <- stats::predict(object = sarimax,
                                   n.ahead = forecast_horizon,
                                   newxreg = 
                                     test_set[, c("pm", "temp_avg")])$pred
    
    return(list(
      model = sarimax,
      test_predicted = as.numeric(pred_sarimax),
      runtime = fit_time
    ))
  }

plan(multicore, workers = availableCores() - 1)

start <- Sys.time()

tscv_sarimax  <-
  tscv_data %>%
  mutate(model_fit = future_pmap(
    list(train_set,
         test_set,
         order,
         season),
    possibly(fit_sarimax,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

tscv_time <- Sys.time() - start

# Evaluation Metrics ------------------------------------------------------

tscv_sarimax_test <-
  tscv_sarimax %>%
  mutate(
    model = map(tscv_sarimax$model_fit, ~ .x[["model"]]),
    test_predicted = 
      map(tscv_sarimax$model_fit, ~ .x[["test_predicted"]]),
    runtime = map(tscv_sarimax$model_fit, ~ .x[["runtime"]])
  ) %>%
  mutate(
    mape = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::mape(actual = .x, predicted = .y) * 100
    ),
    mae = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::mae(actual = .x, predicted = .y)
    ),
    smape = map2_dbl(
      test_actual,
      test_predicted,
      possibly( ~ Metrics::smape(actual = .x, predicted = .y) * 100, 
                otherwise = NaN)
    ),
    rmse = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::rmse(actual = .x, predicted = .y)
    )
  )

tscv_results_test <- tscv_sarimax_test %>%
  summarise(
    nan_count = sum(is.nan(mape)),
    avg_mape = mean(mape),
    avg_mae = mean(mae),
    avg_smape = mean(smape),
    avg_rmse = mean(rmse)
  ) %>%
  filter(nan_count == 0)


# Pkgs --------------------------------------------------------------------

suppressPackageStartupMessages({
library(tidyverse)
library(lubridate)
library(doMC)
library(rsample)
library(Metrics)
library(forecast)
library(tsutils)
})

# Read data ---------------------------------------------------------------

apt_df <- readRDS('data/adm_pm_temp.rds')

# Train/Validation/Test sets ----------------------------------------------

train <- apt_df %>% filter(date < "2017-01-01")

validation <-
  apt_df %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- apt_df %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Fourier terms -----------------------------------------------------------

train_validation <-
  cbind(train_validation, forecast::fourier(
    x = msts(train_validation$adm,
             seasonal.periods = c(7, 365.25)),
    K = c(3, 3)
  ))

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = 0:3,
                   "d" = 0,
                   "q" = 0:3) %>%
  cross() %>%
  map(purrr:::lift(c))

fourier_list <- list(weekly = 1:3,
                     yearly = 1:3) %>%
  cross() %>%
  map(purrr:::lift(c))

dyn_har_reg_hp <- crossing(tibble("order" = order_list),
                           tibble("fourier" = fourier_list))

rm(order_list, fourier_list)

# Time series cross validation --------------------------------------------

tscv_data <- rsample::rolling_origin(
  data = train_validation,
  initial = 365 * 3,
  assess = forecast_horizon,
  cumulative = FALSE,
  skip = forecast_horizon - 1
) %>%
  mutate(
    # Extract train set for each split
    train_set = map(splits, training),
    # Extract validate set for each split
    validate_set = map(splits, testing),
    # Extract actual values to validate
    validate_actual = map(validate_set, ~ .x[["adm"]])
  )

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(dyn_har_reg_hp)

tscv_data$idx <- as.factor(str_c(
  "idx",
  map(tscv_data$order, function(x)
    str_c(x, collapse = "_")),
  map(tscv_data$season, function(x)
    str_c(x, collapse = "_")),
  sep = "_"
))

# SARIMAX function --------------------------------------------------------

fit_sarimax <-
  function(train_set,
           validate_set,
           order,
           season,
           ...) {
    fit_start <- Sys.time()
    
    sarimax <- stats::arima(
      train_set$adm,
      order = unlist(order),
      seasonal = unlist(season),
      xreg = train_set[, c("pm", "temp_avg")],
      optim.control = list(maxit = 750)
    )
    
    fit_time <- Sys.time() - fit_start
    
    pred_sarimax <- stats::predict(object = sarimax,
                                   n.ahead = forecast_horizon,
                                   newxreg =
                                     validate_set[, c("pm", "temp_avg")])$pred
    
    return(list(
      model = sarimax,
      validate_predicted = as.numeric(pred_sarimax),
      runtime = fit_time
    ))
    
  }


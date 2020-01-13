library(tidyverse)
library(lubridate)
library(furrr)
library(rsample)

apt_df <- readRDS('data/adm_pm_temp.rds')

# Sets --------------------------------------------------------------------

train <- apt_df %>% filter(date < "2017-01-01")

validation <- apt_df %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- apt_df %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = seq(1, 2),
                   "d" = seq(0, 1),
                   "q" = seq(2, 2)) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = seq(1, 2),
  "D" = seq(0, 1),
  "Q" = seq(1, 2),
  "period" = 7
)  %>%
  cross() %>%
  map(purrr::lift(c))

sarima_hp <- crossing(tibble("order" = order_list),
                      tibble("season" = season_list))

rm(order_list, season_list)

# Time series cross validation --------------------------------------------

tscv_data <- rsample::rolling_origin(
  data = train_validation,
  initial = 365 * 3,
  assess = 7,
  cumulative = FALSE,
  skip = 6
) %>%
  mutate(
    # Extract train set for each split
    train_set = map(splits, training),
    # Extract validate set for each split
    validate_set = map(splits, testing),
    # Extract actual values to validate
    validate_actual = map(validate_set, function(x) x %>% pull(adm)))

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(sarima_hp)

# -------------------------------------------------------------------------

fit_sarimax <- function(train_set, validate_set, order, season, ...) {
  fit_start <- Sys.time()
  
  sarimax <- stats::arima(
    train$adm,
    order = unlist(order),
    seasonal = unlist(season),
    xreg = train[, c("pm", "temp_avg")],
    optim.control = list(maxit = 750)
  )
  
  fit_time <- Sys.time() - fit_start
  
  pred_sarimax <- stats::predict(object = sarimax,
                                 n.ahead = 7,
                                 newxreg = validate[, c("pm", "temp_avg")])$pred
  
  return(list(
    model = sarimax,
    validate_predicted = as.numeric(pred_sarimax),
    runtime = fit_time
  ))
  
}

plan(multiprocess, workers = availableCores() - 1)
start <- Sys.time()

tscv_sarimax  <-
  tscv_data[1401:1450,] %>%
  mutate(model_fit = future_pmap(
    list(train,
         validate,
         order,
         season),
    possibly(fit_sarimax,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

tscv_time <- Sys.time() - start

tscv_sarimax <-
  tscv_sarimax %>% 
  mutate(model = map(tscv_sarimax$model_fit, pluck("model")),
         validate_predicted = map(tscv_sarimax$model_fit, pluck("validate_predicted")),
         runtime = map(tscv_sarimax$model_fit, pluck("runtime")))

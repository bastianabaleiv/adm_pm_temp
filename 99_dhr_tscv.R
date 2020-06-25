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

# Functions ---------------------------------------------------------------

source("functions.R")

# Read data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv') 

# Train/Validation/Test sets ----------------------------------------------

train <- adm_pm_temp %>% filter(date < "2017-01-01")

validation <-
  adm_pm_temp %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- adm_pm_temp %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Predictors --------------------------------------------------------------

vars <- c("adm",readRDS("vars_stepAIC.rds"))

temp_lagmax <- max(as.numeric(str_extract(vars[str_detect(vars,"temp")],"[0-9]+")), na.rm = TRUE)

pm_lagmax <- max(as.numeric(str_extract(vars[str_detect(vars,"pm")],"[0-9]+")), na.rm = TRUE)

# Add Lags ----------------------------------------------------------------

train_validation <- cbind(
  train_validation,
  append_lags(
    train_validation$temp,
    lags = 1:temp_lagmax,
    col.name = "temp"
  ),
  append_lags(
    train_validation$pm,
    lags = 1:pm_lagmax,
    col.name = "pm"
  )
) %>% 
  dplyr::select(date,vars)

# Parameters --------------------------------------------------------------

fourier_weekly <- 3
fourier_yearly <- 3

# Fourier terms -----------------------------------------------------------

fourier_vars <- forecast::fourier(
  x = msts(train_validation$adm,
           seasonal.periods = c(7, 365.25)),
  K = c(fourier_weekly, fourier_yearly)
)

vars <- c(vars,colnames(fourier_vars))

train_validation <-
  cbind(train_validation, fourier_vars)


# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = 0:10,
                   "d" = 0,
                   "q" = 0:10) %>%
  cross() %>%
  map(purrr:::lift(c))

fourier_list <- list(weekly = 1:k_fourier,
                     yearly = 1:k_fourier) %>%
  cross() %>%
  map(purrr:::lift(c))

dyn_har_reg_hp <- crossing(tibble("order" = order_list),
                           tibble("fourier" = fourier_list))

rm(order_list, fourier_list)


# -------------------------------------------------------------------------



train_validation <- na.omit(train_validation[vars])

y_train <- train_validation %>% dplyr::select(adm)
x_train <- train_validation %>% dplyr::select(-adm)

# Time series cross validation --------------------------------------------

tscv_data <- 
  train_validation %>% 
  rsample::rolling_origin(
  initial = 365 * 3 - max(colSums(is.na(train_validation))),
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
  map(tscv_data$fourier, function(x)
    str_c(x, collapse = "_")),
  sep = "_"
))

# Sun Jun 14 00:10:29 2020 ------------------------------

nani <- tscv_data[800,]$train_set[[1]]

drops <- c("date",
           "month",
           "day",
           "weekday",
           "weekend",
           "yearday",
           "anomaly",
           "holiday")

dhr <- stats::arima(
  x = nani$adm,
  order = unlist(tscv_data[800,]$order),
  xreg = nani[,c("pm","temp","holiday","S1-7","C1-7","S1-365","C1-365",
                 "S2-7","C2-7","S2-365","C2-365",
                 "S3-7","C3-7","S3-365","C3-365",
                 "temp_lag_1",
                 "temp_lag_2",
                 "temp_lag_7",
                 "pm_lag_1",
                 "pm_lag_7",
                 "anomaly")]
)

plot(nani$adm, type = "l")
lines(nani$adm+dhr$residuals, col = "red")
hist(dhr$residuals)

gku
  function(train_set,
           validate_set,
           order,
           ...) {
      
    fit_start <- Sys.time()
    
    dhr <- stats::arima(
      train_set$adm,
      order = unlist(order),
      xreg = train_set[, c("pm", "temp")],
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


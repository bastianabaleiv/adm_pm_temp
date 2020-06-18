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

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

# Train/Validation/Test sets ----------------------------------------------

train <- adm_pm_temp %>% filter(date < "2017-01-01")

validation <-
  adm_pm_temp %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- adm_pm_temp %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Fourier Terms -----------------------------------------------------------

k_fourier <- 3

# Grid --------------------------------------------------------------------

order_list <- list("p" = 0:3,
                   "d" = 0,
                   "q" = 0:3) %>%
  cross() %>%
  map(purrr:::lift(c))

fourier_list <- list(weekly = 1:k_fourier,
                     yearly = 1:k_fourier) %>%
  cross() %>%
  map(purrr:::lift(c))

dyn_har_reg_hp <- crossing(tibble("order" = order_list),
                           tibble("fourier" = fourier_list))

rm(order_list, fourier_list)

# Lagged predictors -------------------------------------------------------

append_lags <- function(x, lags, col.name = NULL){
  lags_list <-  map(lags, ~ lag(x, .x))
  
  if(!is.null(col.name)){
    lags_names <- paste0(col.name,"_lag_",lags)
  } else {
    lags_names <- paste0("lag_",lags)
  }
  
  names(lags_list) <- lags_names
  
  lags <- bind_rows(lags_list)

}

# Add Lags ----------------------------------------------------------------

train_validation <- cbind(train_validation,
                          append_lags(train_validation$temp, 
                                      lags = 1:7, 
                                      col.name = "temp"))

train_validation <- cbind(train_validation,
                          append_lags(train_validation$pm, 
                                      lags = 1:7, 
                                      col.name = "pm"))


# Fourier terms -----------------------------------------------------------

train_validation <-
  cbind(train_validation, forecast::fourier(
    x = msts(train_validation$adm,
             seasonal.periods = c(7, 365.25)),
    K = c(k_fourier, k_fourier)
  ))


# auto.arima try ----------------------------------------------------------
fitted_autodhrm <- 
  auto.arima(y = na.omit(train_validation)$adm,
             d = 0,
             D = 0,
             max.p = 14,
             max.q = 14,
             stationary = TRUE,
             ic = "aicc",
             trace = TRUE,
             xreg = as.matrix(na.omit(train_validation)[,c("pm",
                                                 "temp",
                                                 "holiday",
                                                 "temp_lag_1",
                                                 "temp_lag_2",
                                                 "temp_lag_3",
                                                 "temp_lag_4",
                                                 "temp_lag_5",
                                                 "temp_lag_6",
                                                 "temp_lag_7",
                                                 "pm_lag_1",
                                                 "pm_lag_2",
                                                 "pm_lag_3",
                                                 "S1-7",
                                                 "C1-7",
                                                 "S2-7",
                                                 "C2-7",
                                                 "S3-7",
                                                 "C3-7",
                                                 "S1-365",
                                                 "C1-365",
                                                 "S2-365",
                                                 "C2-365",
                                                 "S3-365",
                                                 "C3-365")]))

plot(na.omit(train_validation)$adm,type = "l")
lines(fitted_autodhrm$fitted, col = "red")

  # Time series cross validation --------------------------------------------

tscv_data <- 
  train_validation %>% 
  rsample::rolling_origin(
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


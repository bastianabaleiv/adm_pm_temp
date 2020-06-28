# Pkgs --------------------------------------------------------------------

#suppressPackageStartupMessages({
library(tidyverse)
library(lubridate)
library(doMC)
library(rsample)
library(Metrics)
library(forecast)
library(tsutils)
#})

# Functions ---------------------------------------------------------------

source("functions.R")

# Read data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv') 

# Parameters --------------------------------------------------------------

arima_p <- 0:10
arima_d <- 0
arima_q <- 0:10
fourier_weekly <- 1:3
fourier_yearly <- 1:5

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(7,14,21,28)

# Train/Validation/Test sets ----------------------------------------------

train <- adm_pm_temp %>% 
  filter(date < "2017-01-01")

validation <-
  adm_pm_temp %>% 
  filter(date >= "2017-01-01" & date < "2018-01-01")

test <- adm_pm_temp %>% 
  filter(date >= "2018-01-01")

train_validation <- rbind(train, validation) %>% 
  mutate(index = row_number())

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
  dplyr::select(index,date,all_of(vars))

# Grids -------------------------------------------------------------------

order_list <- list("p" = arima_p,
                   "d" = arima_d,
                   "q" = arima_q) %>%
  cross() %>%
  map(purrr:::lift(c))

fourier_list <- list(weekly = fourier_weekly,
                     yearly = fourier_yearly) %>%
  cross() %>%
  map(purrr:::lift(c))

dyn_har_reg_hp <- crossing(tibble("order" = order_list),
                           tibble("fourier" = fourier_list))

# Time series cross validation --------------------------------------------

tscv_data <- 
  train_validation %>% 
  rsample::rolling_origin(
    initial = 365 * 3, # Training setup  |  max(colSums(is.na(train_validation)))
    assess = forecast_horizon, # Forecast setup
    cumulative = FALSE, # growing n?
    skip = forecast_horizon - 1
  ) %>%
  mutate(
    # Extract train set for each split
    train_set = map(splits, training),
    # Extract validate set for each split
    validate_set = map(splits, testing),
    # Extract actual values to validate
    validate_actual = map(validate_set, ~ .x[["adm"]]) # y_{t} name
  )

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% 
  expand_grid(dyn_har_reg_hp)

tscv_data$idx <- as.factor(str_c(
  "idx",
  map(tscv_data$order, function(x)
    str_c(x, collapse = "_")),
  map(tscv_data$fourier, function(x)
    str_c(x, collapse = "_")),
  sep = "_"
))

# Fourier Regressors ------------------------------------------------------

fit_dhr <- function(train_set,
                    validate_set,
                    order,
                    fourier,
                    train_validation){
  
}

# Fourier xreg
fourier_cols <- forecast::fourier(
  x = msts(train_validation$adm,
           seasonal.periods = c(7, 365.25)), # multiseasonal periods
  K = unlist(tscv_data[80000,]$fourier)
)

fourier_train <- fourier_cols[tscv_data$train_set[[80000]]$index,]
fourier_validate <- fourier_cols[tscv_data$validate_set[[80000]]$index,]

train <- cbind(tscv_data$train_set[[80000]], fourier_train)
validate <- cbind(tscv_data$validate_set[[80000]], fourier_validate)

y_train <- na.omit(train) %>% 
  dplyr::select(adm)

x_train <- na.omit(train) %>% 
  dplyr::select(-c(index,date,adm))

fit_start <- Sys.time()

dhr <- stats::arima(
  x = y_train,
  order = tscv_data$order[[80000]],
  xreg = x_train
)

fit_time <- Sys.time() - fit_start

saveRDS(dhr,
        file = str_c("tscv_dhr_", 
                     format(start, "%Y_%m_%d_%H%M"), 
                     ".rds"))

plot(y_train$adm, type = "l")
lines(y_train$adm+dhr$residuals, col = "red")
hist(dhr$residuals)

x_validate <- na.omit(validate) %>% 
  dplyr::select(-c(index,date,adm))

forecast_dhr <- 
  as.numeric(
  stats::predict(
    object = dhr,
    n.ahead = forecast_horizon,
    newxreg = as.matrix(x_validate)
  )$pred)

results <- data.frame(date = validate$date,
                 actual = validate$adm,
      predicted = forecast_dhr)

y_sup <- max(c(validate$adm,forecast_dhr))+100
y_inf <- min(c(validate$adm,forecast_dhr))-100

plot(validate$adm, type = "o", ylim = c(y_inf,y_sup))
lines(forecast_dhr, col = "red", ylim = c(y_inf,y_sup))
points(forecast_dhr, col = "red", ylim = c(y_inf,y_sup))

ggplot(results, aes(date)) +
  geom_line(aes(y = actual, colour = "actual")) +
  geom_line(aes(y = predicted, colour = "predicted"))
    
# save RDS model

return(list(
  validate_predicted = as.numeric(pred_sarimax),
  runtime = fit_time
))
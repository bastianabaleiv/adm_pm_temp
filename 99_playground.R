# Pkgs --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(furrr)
library(rsample)
library(Metrics)

# Read data ---------------------------------------------------------------

apt_df <- readRDS('data/adm_pm_temp.rds')

# Train/Validation/Test sets ----------------------------------------------

train <- apt_df %>% filter(date < "2017-01-01")

validation <-
  apt_df %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- apt_df %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = seq(0, 3),
                   "d" = seq(0, 1),
                   "q" = seq(0, 3)) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = seq(0, 3),
  "D" = seq(0, 1),
  "Q" = seq(0, 3),
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

nani <- tscv_data$train_set[[1]]

rec_nani <- recipe(adm ~ pm + temp_avg, data = nani)

rec_nani <- rec_nani %>% step_center(all_predictors())

train_rec <- prep(rec_nani, training = nani)

train_data <- bake(train_rec, new_data = nani)

# Smooth ------------------------------------------------------------------


# tscv_split$train[[1]]$adm
# smooth::ssarima(
#   tscv_split$train[[1]]$adm,
#   h = 7,
#   silent = FALSE)

# -------------------------------------------------------------------------
# library(smooth)
# ssarima(train$adm,
#         silent = "output")
# 
# ssarima(train$adm, 
#         orders = list(ar=c(0,1),i=c(1,0),ma=c(1,1)),
#         lags=c(1,7),
#         h=7,
#         silent = TRUE,
#         xreg = cbind(train$pm, train$temp_avg))
# 
# model <- auto.ssarima(train$adm, h = 28)
# 
# plot(model)
# 
# plot(train$adm, type = "l", col = "grey")
# lines(model$fitted, col = "orange")
# 
# model_forecast <- forecast(model, h = 28, interval = "none")
# plot(model_forecast)
# 
# hist(residuals(model), breaks = 50)
# 

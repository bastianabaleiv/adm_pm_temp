# Pkgs --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(rsample)
library(Metrics)

registerDoParallel(makeCluster(detectCores()-1)) 

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

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(sarima_hp)

tscv_data$idx <- as.factor(str_c(
  "idx",
  map(tscv_data$order, function(x)
    str_c(x, collapse = "_")),
  map(tscv_data$season, function(x)
    str_c(x, collapse = "_")),
  sep = "_"
))

# sarima function --------------------------------------------------------

fit_sarima <-
  function(train_set,
           validate_set,
           order,
           season,
           ...) {
    fit_start <- Sys.time()
    
    sarima <- stats::arima(
      train_set$adm,
      order = unlist(order),
      seasonal = unlist(season),
      #xreg = train_set[, c("pm", "temp_avg")],
      optim.control = list(maxit = 750)
    )
    
    fit_time <- Sys.time() - fit_start
    
    pred_sarima <- stats::predict(object = sarima,
                                  n.ahead = forecast_horizon #, UNIVARIATE SARIMA
                                  #newxreg = validate_set[, c("pm", "temp_avg")] 
    )$pred
    
    return(list(
      model = sarima,
      validate_predicted = as.numeric(pred_sarima),
      runtime = fit_time
    ))
  }

# Fitting procedure -------------------------------------------------------

tscv_data <- tscv_data[1:1000,]

start <- Sys.time()

tscv_sarima <- foreach(i = 1:nrow(tscv_data), 
                       .combine = rbind,
                       .packages = c("tidyverse")) %dopar% {

  tscv_data[i,] %>%
  mutate(model_fit = pmap(
    list(train_set,
         validate_set,
         order,
         season),
    possibly(fit_sarima,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

}

tscv_time <- Sys.time() - start



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Recipes -----------------------------------------------------------------

# # Pkgs --------------------------------------------------------------------
# 
# library(tidyverse)
# library(lubridate)
# library(furrr)
# library(rsample)
# library(Metrics)
# 
# # Read data ---------------------------------------------------------------
# 
# apt_df <- readRDS('data/adm_pm_temp.rds')
# 
# # Train/Validation/Test sets ----------------------------------------------
# 
# train <- apt_df %>% filter(date < "2017-01-01")
# 
# validation <-
#   apt_df %>% filter(date >= "2017-01-01" & date < "2018-01-01")
# 
# test <- apt_df %>% filter(date >= "2018-01-01")
# 
# train_validation <- rbind(train, validation)
# 
# # Forecast Horizon --------------------------------------------------------
# 
# forecast_horizon <- 7 # c(1,7,14,21,28)
# 
# # Grid --------------------------------------------------------------------
# 
# order_list <- list("p" = seq(0, 3),
#                    "d" = seq(0, 1),
#                    "q" = seq(0, 3)) %>%
#   cross() %>%
#   map(purrr:::lift(c))
# 
# season_list <- list(
#   "P" = seq(0, 3),
#   "D" = seq(0, 1),
#   "Q" = seq(0, 3),
#   "period" = 7
# )  %>%
#   cross() %>%
#   map(purrr::lift(c))
# 
# sarima_hp <- crossing(tibble("order" = order_list),
#                       tibble("season" = season_list))
# 
# rm(order_list, season_list)
# 
# # Time series cross validation --------------------------------------------
# 
# tscv_data <- rsample::rolling_origin(
#   data = train_validation,
#   initial = 365 * 3,
#   assess = forecast_horizon,
#   cumulative = FALSE,
#   skip = forecast_horizon - 1
# ) %>%
#   mutate(
#     # Extract train set for each split
#     train_set = map(splits, training),
#     # Extract validate set for each split
#     validate_set = map(splits, testing),
#     # Extract actual values to validate
#     validate_actual = map(validate_set, ~ .x[["adm"]])
#   )
# 
# nani <- tscv_data$train_set[[1]]
# 
# rec_nani <- recipe(adm ~ pm + temp_avg, data = nani)
# 
# rec_nani <- rec_nani %>% step_center(all_predictors())
# 
# train_rec <- prep(rec_nani, training = nani)
# 
# train_data <- bake(train_rec, new_data = nani)

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

#setwd("/home/bastian/Dropbox/paper_hospital_pm")

library(tidyverse)
library(lubridate)
library(furrr)

adm_pm_temp <- readRDS('data/adm_pm_temp.rds')

train <- adm_pm_temp %>% filter(date < "2017-01-01")
validation <- adm_pm_temp %>% filter(date >= "2017-01-01" & date < "2018-01-01")
test <- adm_pm_temp %>% filter(date >= "2018-01-01")

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = seq(0, 2),
                   "d" = seq(0, 1),
                   "q" = seq(0, 2)) %>%
  cross() %>% 
  map(purrr:::lift(c))

season_list <- list("P" = seq(0, 2),
                    "D" = seq(0, 1),
                    "Q" = seq(0, 2),
                    "period" = 7)  %>%
  cross() %>%
  map(purrr::lift(c))

sarima_hp <- crossing(tibble("order" = order_list), 
                      tibble("season" = season_list))

# Time series cross validation --------------------------------------------

train_validation <- rbind(train,validation)
sample_size <- nrow(train) + nrow(validation)

rollwin_len <- nrow(train) # rolling window size
rollwin_skip <- forecast_horizon # increment between successive rolling window 

# To do: change increment between rolling window --------------------------

time_slices <- caret::createTimeSlices(
  seq(1:sample_size),
  initialWindow = rollwin_len,
  horizon = forecast_horizon,
  fixedWindow = TRUE,
  skip = rollwin_skip - 1
)

time_slices <-
  tibble(train_idx = map(unname(time_slices$train), as.vector),
         test_idx = map(unname(time_slices$test), as.vector))

# -------------------------------------------------------------------------

time_slices <- mutate(time_slices, train_set = map(time_slices$train_idx, function(x){train_validation[unlist(x),]}))
time_slices <- mutate(time_slices, test_set = map(time_slices$test_idx, function(x){train_validation[unlist(x),]}))

adm_tbl <- map(time_slices$train_set, function(x) {x$adm})
pm_tbl <- map(time_slices$train_set, function(x) {x$pm})
temp_tbl <- map(time_slices$train_set, function(x) {x$temp_avg})

# -------------------------------------------------------------------------

fit_start <- Sys.time()
nani <- stats::arima(train$adm,
             order = c(1, 0, 1),
             seasonal = list(order = c(1, 0, 3), period = 7),
             xreg = train[,c("pm", "temp_avg")],
             optim.control = list(maxit = 1000))
fit_time <- Sys.time() - fit_start

forecast_value <- stats::predict(nani,
                                 n.ahead = forecast_horizon,
                                 newxreg = test[1:7, c("pm", "temp_avg")])

y_max <- max(forecast_value$pred,
             pull(test[1:7, "adm"]))+250
y_min <- min(forecast_value$pred,
             pull(test[1:7, "adm"]))-250

plot(x = seq(nrow(test[1:7, "adm"])),
     y = as.matrix(test[1:7, "adm"]), 
     type = "b",
     pch = 19,
     ylim = c(y_min,y_max),
     lty = 2,
     main = "Admissions forecast",
     ylab = "Admissions [person]",
     xlab = "Forecast horizon [day]")

lines(seq(nrow(test[1:7, "adm"])),
      as.numeric(forecast_value$pred), 
      col = "Orange",
      type = "b",
      pch = 19,
      lty = 2)

legend("topleft",
       legend=c("Forecast", "Actual value"),
       col=c("Orange", "black"), 
       lty = c(1,1), 
       cex = 0.8)


# Sarima function ---------------------------------------------------------

nani <- sarima_hp[1,]

fit_sarima <- function(adm, pm, temp, order, seasonal) {
  fit_start <- Sys.time()
  
  fit_sarima <-
    stats::arima(adm, 
                 order = order, 
                 seasonal = seasonal,
                 xreg = cbind(pm,temp))
  
  fit_time <- Sys.time() - fit_start
  
  return(list(model = fit_sarima, time = fit_time))
}

ajuste <- pmap(list(adm_tbl, 
                    pm_tbl, 
                    temp_tbl,
                    sarima_hp[1,]$order,
                    sarima_hp[1,]$season),
               fit_sarima)


ajuste <- fit_sarima(adm_tbl[[1]],
           pm_tbl[[1]],
           temp_tbl[[1]],
           order = unlist(sarima_hp[2,]$order),
           seasonal = unlist(sarima_hp[2,]$season))


nani <- stats::arima(train$adm,
                     order = c(1, 0, 1),
                     seasonal = list(order = c(1, 0, 3), period = 7),
                     xreg = train[,c("pm", "temp_avg")])

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

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


fit_sarimax(tscv_data[2,]$train[[1]],
            tscv_data[2,]$validate[[1]],
            tscv_data[2,]$order[[1]],
            tscv_data[2,]$season[[1]])

sarimax <- stats::arima(tscv_data[2,]$train[[1]]$adm,
                        order = unlist(tscv_data[2,]$order[[1]]),
                        seasonal = unlist(tscv_data[2,]$season[[1]]),
                        xreg = tscv_data[2,]$train[[1]][,c("pm", "temp_avg")],
                        optim.control = list(maxit = 750))


fit_sarimax <- stats::predict(sarimax,
                              n.ahead = 7,
                              newxreg = tscv_data[2,]$validate[[1]][,c("pm", "temp_avg")])

# -------------------------------------------------------------------------

tscv_sarimax  <-
  tscv_data[1400:1410, ] %>%
  mutate(model = pmap(
    list(train,
         validate,
         order,
         season),
    possibly(fit_sarimax2,
             otherwise = NULL)
  ))
fit_start <- Sys.time()

tscv_sarimax  <- 
  mutate(tscv_data[2108:2115,],
         model = future_pmap(
    list(train,
         order,
         season),
    possibly(fit_sarimax,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

fit_time <- Sys.time() - fit_start

# fit_start <- Sys.time()
# 
# tscv_sarimax  <-
#   tscv_data[2108:2115, ] %>%
#   mutate(model = pmap(
#     list(train,
#          order,
#          season),
#     possibly(fit_sarimax,
#              otherwise = NULL)
#   ))
# 
# fit_time <- Sys.time() - fit_start

# -------------------------------------------------------------------------

forecast_sarimax <- function(model, validate_set) {
  pred_sarimax <- stats::predict(
    object = model,
    n.ahead = forecast_value,
    newxreg = validate_set[,c("pm", "temp_avg")]
  )
}

tscv_sarimax <- tscv_sarimax %>%
  mutate(validate_predicted = map2(model,
                                   validate,
                                   safely(forecast_sarimax)))

tscv_sarimax$model[[1]]

# -------------------------------------------------------------------------


modelo <- tscv_sarimax$model[[1]]
pm_temp <- tscv_sarimax$validate[[1]][,c("pm","temp_avg")]

forecast_value2 <- stats::predict(modelo,
                                  n.ahead = forecast_horizon,
                                  newxreg = pm_temp)


fvalue2 <-
  tscv_sarimax %>%
  mutate(validate_predicted = map2(model,
                                   validate,
                                   safely(forecast_sarimax)))

tscv_sarimax2 <- tscv_sarimax %>%
  mutate(validate_predicted =  map2(
    .x = model,
    .y = validate,
    ~ possibly(stats::predict(.x,
                              n.head = forecast_horizon,
                              newxreg = unlist(.y)),
               otherwise = NULL)
  ))



# ------------------------------------------------------------------------

# Smooth ------------------------------------------------------------------


tscv_split$train[[1]]$adm
smooth::ssarima(
  tscv_split$train[[1]]$adm,
  h = 7,
  silent = FALSE)

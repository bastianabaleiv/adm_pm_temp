
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

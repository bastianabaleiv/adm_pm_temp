library(tidyverse)
library(lubridate)
library(furrr)
library(rsample)
library(Metrics)

apt_df <- readRDS('data/adm_pm_temp.rds')

# Sets --------------------------------------------------------------------

train <- apt_df %>% filter(date < "2017-01-01")

validation <- apt_df %>% filter(date >= "2017-01-01" & date < "2018-01-01")

test <- apt_df %>% filter(date >= "2018-01-01")

train_validation <- rbind(train, validation)

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = seq(0, 7),
                   "d" = seq(0, 1),
                   "q" = seq(0, 7)) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = seq(0, 7),
  "D" = seq(0, 1),
  "Q" = seq(0, 7),
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
    validate_actual = map(validate_set, ~.x[["adm"]]))

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(sarima_hp)

# -------------------------------------------------------------------------

fit_sarimax <- function(train_set, validate_set, order, season, ...) {
  
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
                                 n.ahead = 7,
                                 newxreg = validate_set[, c("pm", "temp_avg")])$pred
  
  return(list(
    model = sarimax,
    validate_predicted = as.numeric(pred_sarimax),
    runtime = fit_time
  ))
  
}

plan(multicore, workers = availableCores() - 1)

start <- Sys.time()

tscv_sarimax  <-
  tscv_data %>%
  mutate(model_fit = future_pmap(
    list(train_set,
         validate_set,
         order,
         season),
    possibly(fit_sarimax,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

tscv_time <- Sys.time() - start

# Evaluation Metrics ------------------------------------------------------

tscv_sarimax <-
  tscv_sarimax %>%
  mutate(
    model = map(tscv_sarimax$model_fit, ~ .x[["model"]]),
    validate_predicted = map(tscv_sarimax$model_fit, ~ .x[["validate_predicted"]]),
    runtime = map(tscv_sarimax$model_fit, ~ .x[["runtime"]])
  ) %>%
  mutate(
    mape = map2_dbl(
      validate_actual,
      validate_predicted,
      ~ Metrics::mape(actual = .x, predicted = .y) * 100
    ),
    mae = map2_dbl(
      validate_actual,
      validate_predicted,
      ~ Metrics::mae(actual = .x, predicted = .y)
    ),
    smape = map2_dbl(
      validate_actual,
      validate_predicted,
      possibly(~ Metrics::smape(actual = .x, predicted = .y) * 100, otherwise = NaN)
    ),
    rmse = map2_dbl(
      validate_actual,
      validate_predicted,
      ~ Metrics::rmse(actual = .x, predicted = .y)
    )
  )

saveRDS(tscv_sarimax, file = "tscv_sarimax.rds")

# Log ---------------------------------------------------------------------

log_file <- "tscv_sarimax_log.txt"
cat(
  c(
    "SARIMAX",
    paste0("Fecha ejecución: ", now()),
    paste0("Tiempo total de ejecución (modelo): ", format(tscv_time)),
    paste0("Modelos a ajustar: ", nrow(tscv_sarimax)),
    paste0("Modelos no ajustados: ", sum(unlist(
      lapply(tscv_sarimax$model, is.null)
    ))),
    paste0("Modelos ajustados: ", nrow(tscv_sarimax) - sum(unlist(
      lapply(tscv_sarimax$model, is.null)
    ))),
    paste0(
      "Modelos ajustados del total (%): ",
      sum(unlist(lapply(
        tscv_sarimax$model, is.null
      ))) / nrow(tscv_sarimax) * 100,
      "%"
    ),
    "===================================="
  ),
  file = log_file,
  append = TRUE,
  sep = "\n"
)

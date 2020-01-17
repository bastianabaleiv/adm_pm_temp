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

order_list <- list("p" = seq(1, 2),
                   "d" = seq(0, 1),
                   "q" = seq(1, 2)) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = seq(1, 1),
  "D" = seq(0, 1),
  "Q" = seq(1, 1),
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

# SARIMAX function --------------------------------------------------------

fit_sarimax <-
  function(train_set,
           validate_set,
           order,
           season,
           ...) {
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
                                   n.ahead = forecast_horizon,
                                   newxreg = 
                                     validate_set[, c("pm", "temp_avg")])$pred
    
    return(list(
      model = sarimax,
      validate_predicted = as.numeric(pred_sarimax),
      runtime = fit_time
    ))
    
  }

# Fitting procedure -------------------------------------------------------

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
    validate_predicted = 
      map(tscv_sarimax$model_fit, ~ .x[["validate_predicted"]]),
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
      possibly( ~ Metrics::smape(actual = .x, predicted = .y) * 100, 
                otherwise = NaN)
    ),
    rmse = map2_dbl(
      validate_actual,
      validate_predicted,
      ~ Metrics::rmse(actual = .x, predicted = .y)
    )
  )

saveRDS(tscv_sarimax, 
        file = str_c("tscv_sarimax_", format(start, "%Y_%m_%d_%H%M"), ".rds"))

# Time series cross validation results ------------------------------------

# Non fitted days
# tscv_sarimax %>%
#   group_by(idx) %>%
#   filter(is.nan(mape)) %>%
#   select(start_date) %>% View()

# Validation set results (Nan removed)
# tscv_sarimax %>%
#   group_by(idx) %>%
#   summarise(nan_count = sum(is.nan(mape)),
#             avg_mape = mean(mape, na.rm = TRUE),
#             avg_mae = mean(mae, na.rm = TRUE),
#             avg_smape = mean(smape, na.rm = TRUE),
#             avg_rmse = mean(rmse, na.rm = TRUE)) %>%
#   print(n = Inf)

# True results (NaN)

tscv_results <- tscv_sarimax %>%
  group_by(idx) %>%
  summarise(
    nan_count = sum(is.nan(mape)),
    avg_mape = mean(mape),
    avg_mae = mean(mae),
    avg_smape = mean(smape),
    avg_rmse = mean(rmse)
  ) %>%
  filter(nan_count == 0)

# Log ---------------------------------------------------------------------

log_file <-
  str_c("tscv_sarimax_", format(start, "%Y_%m_%d_%H%M"), ".txt")

cat(
  c(
    "SARIMAX",
    paste0("Fecha ejecución: ", start),
    paste0("Tiempo total de ejecución (modelo): ", format(tscv_time)),
    paste0("Horizonte de pronóstico: ", forecast_horizon),
    paste0("Modelos a ajustar: ", nrow(tscv_sarimax)),
    paste0("Modelos no ajustados: ", sum(unlist(
      lapply(tscv_sarimax$model, is.null)
    ))),
    paste0("Modelos ajustados: ", nrow(tscv_sarimax) - sum(unlist(
      lapply(tscv_sarimax$model, is.null)
    ))),
    paste0(
      "Modelos no ajustados del total (%): ",
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

sink(log_file, append = TRUE)
print(tscv_results)
sink()
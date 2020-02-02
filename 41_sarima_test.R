library(tidyverse)
library(lubridate)
library(furrr)
library(rsample)
library(Metrics)

# Sets --------------------------------------------------------------------

apt_df <- readRDS('data/adm_pm_temp.rds')

# Forecast Horizon --------------------------------------------------------

forecast_horizon <- 7 # c(1,7,14,21,28)

# Grid --------------------------------------------------------------------

order_list <- list("p" = 3,
                   "d" = 1,
                   "q" = 0) %>%
  cross() %>%
  map(purrr:::lift(c))

season_list <- list(
  "P" = 1,
  "D" = 0,
  "Q" = 2,
  "period" = 7
)  %>%
  cross() %>%
  map(purrr::lift(c))

sarima_hp <- crossing(tibble("order" = order_list),
                      tibble("season" = season_list))

params <- c(order_list,season_list) %>% unlist()

# rm(order_list, season_list)

# Time series cross validation (Test set) ---------------------------------

tscv_data <- rsample::rolling_origin(
  data = apt_df,
  initial = 365 * 4,
  assess = forecast_horizon,
  cumulative = FALSE,
  skip = forecast_horizon - 1
) %>%
  mutate(
    # Extract train set for each split
    train_set = map(splits, training),
    # Extract validate set for each split
    test_set = map(splits, testing),
    # Extract actual values to validate
    test_actual = map(test_set, ~ .x[["adm"]])
  )

start_date <-
  map(tscv_data$splits, function(x)
    min(assessment(x)$date))

tscv_data$start_date <- do.call("c", start_date)

tscv_data <- tscv_data %>% expand_grid(sarima_hp)

# sarima function --------------------------------------------------------

fit_sarima <-
  function(train_set,
           test_set,
           order,
           season,
           ...) {
    fit_start <- Sys.time()
    
    sarima <- stats::arima(
      train_set$adm,
      order = unlist(order),
      seasonal = unlist(season),
      #xreg = train_set[, c("pm", "temp_avg")],
      optim.control = list(maxit = 1000)
    )
    
    fit_time <- Sys.time() - fit_start
    
    pred_sarima <- stats::predict(object = sarima,
                                   n.ahead = forecast_horizon#,
                                  # newxreg = test_set[, c("pm", "temp_avg")]
                                  )$pred
    
    return(list(
      model = sarima,
      test_predicted = as.numeric(pred_sarima),
      runtime = fit_time
    ))
  }

plan(multicore, workers = availableCores() - 1)

start <- Sys.time()

tscv_sarima  <-
  tscv_data %>%
  mutate(model_fit = future_pmap(
    list(train_set,
         test_set,
         order,
         season),
    possibly(fit_sarima,
             otherwise = NULL)
    ,
    .progress = TRUE
  ))

tscv_time <- Sys.time() - start

# Evaluation Metrics ------------------------------------------------------

tscv_sarima_test <-
  tscv_sarima %>%
  mutate(
    date = map(tscv_sarima$test_set, ~.x[["date"]]),
    model = map(tscv_sarima$model_fit, ~ .x[["model"]]),
    test_predicted = 
      map(tscv_sarima$model_fit, ~ .x[["test_predicted"]]),
    runtime = map(tscv_sarima$model_fit, ~ .x[["runtime"]])
  ) %>%
  mutate(
    mape = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::mape(actual = .x, predicted = .y) * 100
    ),
    mae = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::mae(actual = .x, predicted = .y)
    ),
    smape = map2_dbl(
      test_actual,
      test_predicted,
      possibly( ~ Metrics::smape(actual = .x, predicted = .y) * 100, 
                otherwise = NaN)
    ),
    rmse = map2_dbl(
      test_actual,
      test_predicted,
      ~ Metrics::rmse(actual = .x, predicted = .y)
    )
  )

tscv_results_test <- tscv_sarima_test %>%
  summarise(
    nan_count = sum(is.nan(mape)),
    avg_mape = mean(mape),
    avg_mae = mean(mae),
    avg_smape = mean(smape),
    avg_rmse = mean(rmse)
  ) %>%
  filter(nan_count == 0)


# Output ------------------------------------------------------------------

output_path <- str_c("tscv_sarima_test_", format(start, "%Y_%m_%d_%H%M"))

tscv_sarima_test %>% 
  select(test_set, forecast = test_predicted) %>% 
  unnest(c(test_set, forecast)) %>% 
  write_csv(path = str_c(output_path, ".csv"))

log_file <-
  str_c(output_path, ".txt")

cat(
  c(
    "SARIMA univariate model - Test Set",
    paste0("Fecha ejecución: ", start),
    paste0("Tiempo total de ejecución (modelo): ", format(tscv_time)),
    paste0("Horizonte de pronóstico: ", forecast_horizon),
    paste0("MAPE: ", round(min(tscv_results_test$avg_mape),3), "%"),
    "====================================",
    "Parámetros especificación"
  ),
  file = log_file,
  append = TRUE,
  sep = "\n"
)

sink(log_file, append = TRUE)
print(params)
print(tscv_results_test)
sink()


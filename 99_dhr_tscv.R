# Pkgs --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(doMC)
library(rsample)
library(Metrics)
library(forecast)

# Paths -------------------------------------------------------------------

main_path <- getwd()

# Create folders for fitted models
fit_folder <- "fitted"

ifelse(!dir.exists(file.path(main_path, fit_folder)), dir.create(file.path(main_path, fit_folder)), FALSE)

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

horizon_folder <- paste0("h_",forecast_horizon)

# Create folders for forecasted horizon 
ifelse(
  !dir.exists(file.path(main_path, fit_folder, horizon_folder)), 
  dir.create(file.path(main_path, fit_folder, horizon_folder)), 
  FALSE)

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

tscv_data$idx <- str_c(
  "dhr",
  map(tscv_data$order, function(x)
    str_c(x, collapse = "_")),
  map(tscv_data$fourier, function(x)
    str_c(x, collapse = "_")),
  sep = "_"
)

# Create folders for fitted models
purrr:::map(tscv_data$idx, ~ ifelse(!dir.exists(
  file.path(main_path, fit_folder, horizon_folder, .x)
),
dir.create(
  file.path(main_path, fit_folder, horizon_folder, .x)
),
FALSE))

# Fourier Regressors ------------------------------------------------------

fit_dhr <- function(train_set,
                    validate_set,
                    order,
                    fourier,
                    idx,
                    id,
                    #train_validation,
                    enhance = FALSE){
  
  model_folder <- idx
  log_file <- paste0(id,".txt")
  dhr_file <- paste0(id,".rds")
  validate_file <- paste0(id,".csv")
  
  out_fitted_log <- 
    file.path(main_path, fit_folder, horizon_folder, model_folder, log_file)
  
  out_fitted_model <- 
    file.path(main_path, fit_folder, horizon_folder, model_folder, dhr_file)
  
  out_validate_set <- 
    file.path(main_path, fit_folder, horizon_folder, model_folder, validate_file)
  
  # Fourier xreg
  fourier_cols <- forecast::fourier(
    x = msts(train_validation$adm,
             seasonal.periods = c(7, 365.25)), # multiseasonal periods
    K = unlist(fourier)
  )
  
  fourier_train <- fourier_cols[train_set$index,]
  fourier_validate <- fourier_cols[validate_set$index,]
  
  train <- cbind(train_set, fourier_train)
  validate <- cbind(validate_set, fourier_validate)
  
  y_train <- na.omit(train) %>% 
    dplyr::select(adm)
  
  x_train <- na.omit(train) %>% 
    dplyr::select(-c(index,date,adm))
  
  fit_start <- Sys.time()
  
  dhr <- stats::arima(
    x = y_train,
    order = order,
    xreg = x_train,
    include.mean = TRUE,
    method = "ML",
    transform.pars = FALSE,
    optim.method = "BFGS"
  )
  
  fit_time <- Sys.time() - fit_start
  
  capture.output(dhr, 
                 file = out_fitted_log)
  
  cat("\n", file = out_fitted_log, append = TRUE)
  
  capture.output(fit_time, 
                 file = out_fitted_log,
                 append = TRUE)
  
  saveRDS(dhr,
          file = out_fitted_model)
  
  #dhr <- readRDS(file = out_fitted_model)
  
  x_validate <- na.omit(validate) %>% 
    dplyr::select(-c(index,date,adm))
  
  forecast_dhr <- 
    stats::predict(
      object = dhr,
      n.ahead = forecast_horizon,
      newxreg = x_validate,
      se.fit = TRUE
    )
  
  validate$adm_pred <- as.numeric(forecast_dhr$pred)
  validate$adm_pred_se = as.numeric(forecast_dhr$se)
  
  validate %>% 
    select(index:adm,adm_pred,adm_pred_se,everything()) %>% 
    write_csv(
      path = out_validate_set
    )
  
  if(enhance == TRUE){
    plot(y_train$adm, type = "l")
    lines(y_train$adm+dhr$residuals, col = "red")
    hist(dhr$residuals, breaks = 50)
    tsdiag(dhr)
    
    
    y_sup <- max(c(validate$adm,validate$adm_pred))+100
    y_inf <- min(c(validate$adm,validate$adm_pred))-100
    
    plot(validate$adm, type = "o", ylim = c(y_inf,y_sup))
    lines(validate$adm_pred, col = "red", ylim = c(y_inf,y_sup))
    points(validate$adm_pred, col = "red", ylim = c(y_inf,y_sup))
    
    ggplot(validate, aes(date)) +
      geom_line(aes(y = adm, colour = "actual")) +
      geom_line(aes(y = adm_pred, colour = "predicted"))
    
    # sweep::sw_tidy(dhr) %>% View()
    # sweep::sw_glance(dhr)
    # sweep::sw_augment(dhr)
    # sw_tidy
  }

  return(
    list(
      validate_pred = validate$adm_pred,
      validate_pred_se = validate$adm_pred_se,
      runtime = fit_time
    )
  )
  
}

# Fitting procedure -------------------------------------------------------

registerDoMC(detectCores()-1)

start <- Sys.time()

tscv_dhr <- foreach(
  i = 80000:80030, #1:nrow(tscv_data)
  .combine = rbind,
  .packages = c("purrr","dplyr")
) %dopar% {
  tscv_data[i,] %>%
    mutate(model_fit = pmap(
      list(train_set,
           validate_set,
           order,
           fourier,
           idx,
           id,
           #train_validation = train_validation,
           enhance = FALSE),
      possibly(fit_dhr,
               otherwise = NULL)
    ))
}

tscv_time <- Sys.time() - start


# Evaluation Metrics ------------------------------------------------------

tscv_dhr <-
  tscv_dhr %>%
  mutate(
    validate_predicted =
      map(tscv_dhr$model_fit, ~ .x[["validate_pred"]]),
    runtime = map(tscv_dhr$model_fit, ~ .x[["runtime"]])
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
      possibly(~ Metrics::smape(actual = .x, predicted = .y) * 100,
               otherwise = NaN)
    ),
    rmse = map2_dbl(
      validate_actual,
      validate_predicted,
      ~ Metrics::rmse(actual = .x, predicted = .y)
    )
  )

results_file <- paste0("dhr_",horizon_folder,".rds")

out_results_set <- file.path(main_path, fit_folder, horizon_folder,results_file)

saveRDS(tscv_dhr,
        file = out_results_set)

#tscv_dhr <- readRDS(file = out_results_set)

# Time series cross validation results ------------------------------------

tscv_results <- tscv_dhr %>%
  group_by(idx) %>%
  summarize(
    nan_count = sum(is.nan(mape)),
    avg_mape = mean(mape),
    avg_mae = mean(mae),
    avg_smape = mean(smape),
    avg_rmse = mean(rmse)
  ) %>%
  filter(nan_count == 0) %>%
  arrange(avg_mape)

# Best model

best_model_idx <- tscv_results %>%
  slice(which.min(avg_mape))

params <-
  str_extract_all(as.character(best_model_idx$idx), pattern = "\\d+") %>%
  unlist()

names(params) <- c("p", "d", "q", "weekly", "yearly")

# Log ---------------------------------------------------------------------

results_log <- paste0("dhr_",horizon_folder,".txt")

out_results_log <- file.path(main_path, fit_folder, horizon_folder,results_log)

cat(
  c(
    "Linear Regression with ARIMA errors",
    paste0("Fecha ejecución: ", start),
    paste0("Tiempo total de ejecución (modelo): ", format(tscv_time)),
    paste0("Horizonte de pronóstico: ", forecast_horizon),
    paste0("Procesadores utilizados: ", detectCores() - 1),
    paste0("Modelos a ajustar: ", nrow(tscv_dhr)),
    paste0("Modelos no ajustados: ", sum(unlist(
      lapply(tscv_dhr$model_fit, is.null)
    ))),
    paste0("Modelos ajustados: ", nrow(tscv_dhr) - sum(unlist(
      lapply(tscv_dhr$model_fit, is.null)
    ))),
    paste0(
      "Modelos no ajustados del total (%): ",
      sum(unlist(lapply(
        tscv_dhr$model_fit, is.null
      ))) / nrow(tscv_dhr) * 100,
      "%"
    ),
    paste0("Mejor MAPE: ", round(min(
      best_model_idx$avg_mape
    ), 3), "%"),
    "====================================",
    "Parámetros mejor especificación"
  ),
  file = out_results_log,
  append = TRUE,
  sep = "\n"
)

sink(out_results_log , append = TRUE)
params
head(tscv_results, 10)
sink()

# Pkgs --------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(MuMIn)
})

# Read data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

# Train/Validation/Test sets ----------------------------------------------

train <- adm_pm_temp %>% filter(date < "2017-01-01")

# -------------------------------------------------------------------------

append_lags <- function(x, lags, col.name = NULL) {
  lags_list <-  map(lags, ~ lag(x, .x))
  
  if (!is.null(col.name)) {
    lags_names <- paste0(col.name, "_lag_", lags)
  } else {
    lags_names <- paste0("lag_", lags)
  }
  
  names(lags_list) <- lags_names
  
  lags <- bind_rows(lags_list)
  
}

# Add Lags ----------------------------------------------------------------

train <- cbind(train,
               append_lags(
                 train$temp,
                 lags = 1:14,
                 col.name = "temp"
               ))

train <- cbind(train,
               append_lags(train$pm,
                           lags = 1:14,
                           col.name = "pm"))

train_tslm <- na.omit(train) %>% 
  as_tibble() %>% 
  dplyr::select(-c(date,month,day,weekday,weekend,yearday,anomaly))

fit <- lm(adm ~ ., data = train_tslm)

fit_stepAIC <- MASS::stepAIC(fit, trace = TRUE, direction = "backward")

names(coef(fit_stepAIC))

vars_stepAIC <- train[which(colnames(train) %in% names(coef(fit_stepAIC)))]

glimpse(vars_stepAIC)
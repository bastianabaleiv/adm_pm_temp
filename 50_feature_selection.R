# Pkgs --------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(MuMIn)
})

# Functions ---------------------------------------------------------------

source("functions.R")

# Read data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

# Train/Validation/Test sets ----------------------------------------------

train <- adm_pm_temp %>% filter(date < "2017-01-01")

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

saveRDS(colnames(vars_stepAIC), file = "vars_stepAIC.rds")

fit_lasso <- glmnet::cv.glmnet(
  x = as.matrix(train_tslm[,2:ncol(train_tslm)]),
  y = train_tslm$adm)

coef_lasso <- as.vector(coef(fit_lasso))

names(coef_lasso) <- rownames(coef(fit_lasso))

coef_lasso <- coef_lasso[coef_lasso!=0]

vars_lasso <- train[which(colnames(train) %in% names(coef_lasso))]

saveRDS(colnames(vars_lasso), file = "vars_lasso.rds")

library(tidyverse)

tscv_results <- tscv_sarimax_2020_01_16_1850 %>%
  group_by(idx) %>%
  summarise(
    nan_count = sum(is.nan(mape)),
    avg_mape = mean(mape),
    avg_mae = mean(mae),
    avg_smape = mean(smape),
    avg_rmse = mean(rmse)
  ) %>%
  filter(nan_count == 0) %>% 
  arrange(avg_mape)

best_model_idx <- tscv_results %>% 
  slice(which.min(avg_mape))

params <- str_extract_all(as.character(best_model_idx$idx), pattern = "\\d+") %>% unlist()
names(params) <- c("p","d","q","P","D","Q","period")

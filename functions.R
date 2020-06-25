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

plot_tscf <- function(x,
                     lag,
                     title,
                     breaks) {
  
  library(astsa)
  
  cf <- acf2(series = x,
               plot = FALSE,
               max.lag = lag) 
  
  # CI parameters
  alpha <- 0.95
  conf_lims <-
    c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(length(x))
  
  # ACF
  ts_acf <- cf[,1]
  
  # PACF 
  ts_pacf <- cf[,2]
  
  # ACF ggplot
  acf_plot <- 
    ts_acf %>%
    as_tibble() %>%
    mutate(lags = 1:n()) %>%
    ggplot(aes(x = lags, y = value)) +
    scale_x_continuous(breaks = seq(0, lag, breaks)) +
    geom_hline(yintercept = conf_lims,
               lty = 2,
               col = 'red') +
    labs(y = "ACF", x = "Lag", title = title) +
    geom_segment(aes(xend = lags, yend = 0)) + geom_point() + theme_setting 
  
  # PACF ggplot
  pacf_plot <- 
    ts_pacf %>%
    as_tibble() %>%
    mutate(lags = 1:n()) %>%
    ggplot(aes(x = lags, y = value)) +
    scale_x_continuous(breaks = seq(0, lag, breaks)) +
    geom_hline(yintercept = conf_lims,
               lty = 2,
               col = 'red') +
    labs(y = "PACF", x = "Lag", title = title) +
    geom_segment(aes(xend = lags, yend = 0)) + geom_point() + theme_setting 
  

  plots <- list(
    acf = acf_plot,
    pacf = pacf_plot
  )
}

adm_plot <- function(date, actual, predicted, mape) {
  plot_tbl <- tibble(
    date = date,
    actual = actual,
    predicted = predicted
  )
  
  plot_melt <- reshape2::melt(plot_tbl, id.vars = "date")
  
  p <- ggplot(data = plot_melt,
              aes(x = date,
                  y = value,
                  colour = variable)) +
    geom_line() +
    geom_point() +
    theme_light() +
    xlab("Date") +
    ylab("Admissions") +
    labs(colour = paste0("MAPE = ", round(mape, 2), "%")) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_date(date_breaks = "1 day")
  
  return(p)
}

# tscv_sarima_test <- tscv_sarima_test %>% 
#   mutate(forecast_plot = 
#            pmap(list(date = date,
#                      actual = test_actual,
#                      predicted = test_predicted,
#                      mape = mape),
#                 adm_plot))
# 
# saveRDS(tscv_sarima_test, 
#         file = str_c("tscv_sarima_test_tidy", ".rds"))

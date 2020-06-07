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

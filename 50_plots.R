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

tscv_sarima_test <- tscv_sarima_test %>% 
  mutate(forecast_plot = 
  pmap(list(date = date,
            actual = test_actual,
            predicted = test_predicted,
            mape = mape),
       adm_plot))

saveRDS(tscv_sarima_test, 
        file = str_c("tscv_sarima_test_tidy", ".rds"))

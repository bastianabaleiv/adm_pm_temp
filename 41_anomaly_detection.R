# Packages ----------------------------------------------------------------
library(tidyverse)
library(anomalize)
library(tibbletime)

source("functions.R")

# Load data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

# STL Decomposition -------------------------------------------------------

anomalized_stl <- adm_pm_temp %>% 
  time_decompose(adm,
                 method = "stl",
                 frequency = "1 weeks",
                 trend = "auto") %>% 
  anomalize(remainder,
            alpha = 0.05) 

recomposed_stl <- anomalized_stl %>% 
  time_recompose()

recomposed_stl %>% 
  plot_anomaly_decomposition() +
  ggtitle("alpha = 0.05")

cleaned_stl <- anomalized_stl %>% 
  clean_anomalies()

plot_adm_yearly_outliers_stl <- cleaned_stl %>%
  mutate(commondate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j"),
         year = year(date),
         adm_anomaly = ifelse(anomaly == "Yes", observed, NA)) %>%
  ggplot(mapping = aes(x = commondate,
                       y = observed_cleaned)) +
  geom_line() +
  facet_grid(facets = year ~ .) +
  theme_half_open() +
  background_grid() +
  scale_x_date(
    labels = function(x)
      format(x, "%b"),
    breaks = date_breaks("1 months")
  ) +
  ylab("Admissions [person]") +
  xlab("Date") +
  geom_point(aes(x = commondate, y = adm_anomaly),
             size = 2,
             color = "red")

plot_adm_yearly_outliers_stl


# Twitter Decomposition ----------------------------------------------------

anomalized_twitter <- adm_pm_temp %>% 
  time_decompose(adm,
                 method = "twitter",
                 frequency = "1 weeks",
                 trend = "auto") %>% 
  anomalize(remainder,
            alpha = 0.05) 

recomposed_twitter <- anomalized_twitter %>% 
  time_recompose()

recomposed_twitter %>% 
  plot_anomaly_decomposition() +
  ggtitle("alpha = 0.05")

cleaned_twitter <- anomalized_twitter %>% 
  clean_anomalies()

plot_adm_yearly_outliers_twitter <- cleaned_twitter %>%
  mutate(commondate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j"),
         year = year(date),
         adm_anomaly = ifelse(anomaly == "Yes", observed, NA)) %>%
  ggplot(mapping = aes(x = commondate,
                       y = observed_cleaned)) +
  geom_line() +
  facet_grid(facets = year ~ .) +
  theme_half_open() +
  background_grid() +
  scale_x_date(
    labels = function(x)
      format(x, "%b"),
    breaks = date_breaks("1 months")
  ) +
  ylab("Admissions [person]") +
  xlab("Date") +
  geom_point(aes(x = commondate, y = adm_anomaly),
             size = 2,
             color = "red")

plot_adm_yearly_outliers_twitter

# Anomalies ---------------------------------------------------------------
adm_pm_temp$anomaly <- as.numeric(cleaned_twitter$anomaly == "Yes")

# Save file ---------------------------------------------------------------
write_csv(x = adm_pm_temp,
          path = "data/adm_pm_temp.csv")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(anomalize)
library(tibbletime)

setwd("/home/bastian/Dropbox/paper_hospital_pm")

load('data/adm_pm_temp.RData')

anomalized <- adm_pm_temp %>% 
  select(date, adm) %>% 
  time_decompose(adm,
                 method = "stl",
                 frequency = "1 weeks",
                 trend = "1 month") %>% 
  anomalize(remainder)

outliers <- anomalized %>% 
  select(date, anomaly) %>%
  filter(anomaly == "Yes") %>% 
  select(date)

p1 <- adm_pm_temp %>%
  select(date, adm) %>%
  time_decompose(adm,
                 method = "stl",
                 frequency = "1 week",
                 trend = "3 months") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition()

p1

adm_pm_temp %>% 
  select(date, adm) %>% 
  time_frequency()

adm_pm_temp %>% 
  select(date, adm) %>% 
  time_trend()

cleaned <- adm_pm_temp %>% 
  select(date, adm) %>% 
  time_decompose(adm,
                 method = "stl",
                 frequency = "1 week",
                 trend = "3 months") %>% 
  anomalize(remainder) %>% 
  clean_anomalies()

anomaly <- which(cleaned$anomaly == "Yes")

ggplot(cleaned ,aes(x = date, y = observed_cleaned)) +
geom_line() + 
geom_point(data = cleaned[anomaly,],
           aes(x = date, y = observed))

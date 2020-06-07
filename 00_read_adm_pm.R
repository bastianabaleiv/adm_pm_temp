# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)

# Setup -------------------------------------------------------------------

#setwd("/home/bastian/Dropbox/paper_hospital_pm")
setwd("/home/baballay/adm_pm_temp")

# Loading RAW data --------------------------------------------------------

# Hospital Admissions & P.M 2.5 -------------------------------------------

# Import Data
adm_pm_tbl <- read_excel("data/01. Serie_Sarimax.xlsx",
                         sheet = "Admissions",
                         col_names = FALSE)

# Column names
colnames(adm_pm_tbl) <- c("year", "month", "day", "adm", "pm")

# Any column with NAs?
sapply(adm_pm_tbl, function(x)
  any(is.na(x)))

# Set date
adm_pm_tbl <-
  tibble(
    date = seq(
      ymd("2014-1-1"),
      by = "day",
      length.out = nrow(adm_pm_tbl)
    ),
    adm = adm_pm_tbl$adm,
    pm = adm_pm_tbl$pm
  ) %>%
  as_tsibble(index = date)

# Save file ---------------------------------------------------------------

write_csv(x = adm_pm_tbl,
          path = "data/adm_pm.csv")

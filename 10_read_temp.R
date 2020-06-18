# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)

# Setup -------------------------------------------------------------------

setwd("/home/baballay/adm_pm_temp")

# Load RAW data -----------------------------------------------------------

# Temperature -------------------------------------------------------------

temp_ohig_tbl <- read_delim(
  "data/datos_parque_ohiggins_140101_190929.csv",
  ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
)

temp_indep_tbl <- read_delim(
  "data/datos_independencia_140101_190929.csv",
  ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
)

temp_cnav_tbl <- read_delim(
  "data/datos_cerro_navia_140101_190929.csv",
  ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
)

# Useful Cols
temp_ohig_tbl <- temp_ohig_tbl[1:3]
temp_indep_tbl <- temp_indep_tbl[1:3]
temp_cnav_tbl <- temp_cnav_tbl[1:3]

# Colnames 
colnames(temp_ohig_tbl) <- c("yymmdd","hrmm", "temp")
colnames(temp_indep_tbl) <- c("yymmdd","hrmm", "temp")
colnames(temp_cnav_tbl) <- c("yymmdd","hrmm", "temp")

# Set date
temp_tbl <-  tibble(
  datetime = seq(
    ymd_hm("2014-1-1 01-00"),
    by = "1 hour",
    length.out = nrow(temp_ohig_tbl)
  ),
  temp_ohig = temp_ohig_tbl$temp,
  temp_indep = temp_indep_tbl$temp,
  temp_cnav = temp_cnav_tbl$temp
) %>%
  as_tsibble(index = datetime) %>%
  filter(datetime <= "2018-12-31")

rm(temp_ohig_tbl,temp_cnav_tbl,temp_indep_tbl)

# Save file ---------------------------------------------------------------

write_csv(x = temp_tbl,
          path = "data/temp_RAW.csv")
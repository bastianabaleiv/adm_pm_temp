# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(imputeTS)
library(forecast)

# Working directory -------------------------------------------------------

setwd("/home/bastian/Dropbox/paper_hospital_pm")

# Loading raw data --------------------------------------------------------

adm_pm_tbl <- read_excel("data/01. Serie_Sarimax.xlsx",
                         sheet = "Admissions",
                         col_names = FALSE)

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

temp_ohig_tbl <- temp_ohig_tbl[1:3]
temp_indep_tbl <- temp_indep_tbl[1:3]
temp_cnav_tbl <- temp_cnav_tbl[1:3]
# Headers -----------------------------------------------------------------

colnames(adm_pm_tbl) <- c("year","month","day","adm","pm")
colnames(temp_ohig_tbl) <- c("yymmdd","hrmm", "temp")
colnames(temp_indep_tbl) <- c("yymmdd","hrmm", "temp")
colnames(temp_cnav_tbl) <- c("yymmdd","hrmm", "temp")

# Avg. & Min. Temperature -------------------------------------------------

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

summary(temp_tbl)

temp_ohig_na <- temp_tbl %>% 
  filter(is.na(temp_ohig)) %>% 
  mutate(temp_ohig = rowMeans(.[3:4],na.rm = TRUE))

temp_tbl[is.na(temp_tbl$temp_ohig),]$temp_ohig <- temp_ohig_na$temp_ohig

temp_tbl <- temp_tbl %>% select(datetime, temp = temp_ohig)

rm(temp_ohig_na)

# source 01 imputation.R --------------------------------------------------

summary(is.na(temp_tbl))

temp_tbl[is.na(temp_tbl$temp),]$temp <- NA

print(temp_tbl[is.na(temp_tbl$temp),])

which(is.na(temp_tbl$temp))

plotNA.gapsize(temp_tbl$temp)
statsNA(temp_tbl$temp)


# STL decomposition -------------------------------------------------------

temp_msts <- msts(temp_tbl$temp,
                  seasonal.periods = c(24,8766))

temp_msts %>% mstl() %>% seasadj() %>% autoplot()

# Missing value imputation by Kalman Smoothing  ---------------------------

temp_imp <- na_kalman(temp_tbl$temp)

plotNA.imputations(temp_tbl$temp[17130:17144], 
                   temp_imp[17130:17144],
                   pch = '')

plotNA.imputations(temp_tbl$temp[18290:18680], 
                   temp_imp[18290:18680],
                   pch = '')

plotNA.imputations(temp_tbl$temp[19000:19520], 
                   temp_imp[19000:19520],
                   pch = '')

plotNA.imputations(temp_tbl$temp[25860:25900], 
                   temp_imp[25800:25900],
                   pch = '')

plotNA.imputations(temp_tbl$temp[41690:41760], 
                   temp_imp[41690:41760],
                   pch = '')

temp_tbl$temp <- temp_imp

# Temperature consolidation -----------------------------------------------

temp_tbl <- temp_tbl %>% 
  index_by(date = date(datetime)) %>% 
  summarize(temp_min = min(temp, na.rm = TRUE),
            temp_avg = mean(temp, na.rm = TRUE),
            temp_max = max(temp, na.rm = TRUE))

# Set date properly to adm_pm_tbl -----------------------------------------

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

# Consolidate dataset -----------------------------------------------------

adm_pm_temp <- left_join(adm_pm_tbl, temp_tbl, by = "date") %>% 
  mutate(month = month(date),
         day = day(date),
         weekday = wday(date, 
                        label = FALSE, 
                        abbr = FALSE, 
                        week_start = getOption("lubridate.week.start", 
                                               1)),
         weekend = if_else(weekday %in% 
                             c(6, 7), "weekend", "weekday"),
         yearday = yday(date)
  )

rm(list=setdiff(ls(), "adm_pm_temp"))


# Save file ---------------------------------------------------------------

save(list = "adm_pm_temp", file = "data/adm_pm_temp.RData")
write_csv(x = adm_pm_temp,
          path = "data/adm_pm_temp.csv")

# Sun Sep 29 22:41:12 2019 ------------------------------
# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
# library(tsibble)

# Setup -------------------------------------------------------------------

setwd("/home/baballay/adm_pm_temp")

# Load Data ---------------------------------------------------------------

adm_pm_tbl <- read_csv("data/adm_pm.csv")
temp_tbl <- read_csv("data/temp.csv")

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

# Save file ---------------------------------------------------------------

write_csv(x = adm_pm_temp,
          path = "data/adm_pm_temp.csv")

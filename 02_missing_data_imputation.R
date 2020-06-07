# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(tsibble)
library(imputeTS)

# Setup -------------------------------------------------------------------

setwd("/home/baballay/adm_pm_temp")

# Load Data ---------------------------------------------------------------

temp_tbl <- read_csv("data/temp_RAW.csv")

# Temperature Imputation --------------------------------------------------

# NAs
summary(temp_tbl)

# Any column with NAs?
sapply(temp_tbl[,-1], function(x) any(is.na(x)))

# Which rows are NAs
temp_na <- sapply(temp_tbl[,-1], function(x) which(is.na(x)))

# How many NAs?
sapply(temp_na,length)

# Rows with NAs
row_na <- unlist(temp_na,use.names = FALSE) %>% unique()

# Check Rows
#temp_tbl[row_na,] %>% View()

# Temperature average across stations
temp <- rowMeans(temp_tbl[,-1],na.rm = TRUE)
which(is.na(temp))

# NAs plot & Stats
plotNA.gapsize(temp)
plotNA.distribution(
  temp,
  pch = '')
statsNA(temp)

temp_imp <- na_kalman(temp)

# # Imputation examples
# plotNA.imputations(temp[18310:18319], 
#                    temp_imp[18310:18319],
#                    pch = '')
# 
# plotNA.imputations(temp[18325:18340], 
#                    temp_imp[18325:18340],
#                    pch = '')
# 
# plotNA.imputations(temp[19040:19060], 
#                    temp_imp[19040:19060],
#                    pch = '')
# 
# # 14 NA
# plotNA.imputations(temp[19160:19180], 
#                    temp_imp[19160:19180],
#                    pch = '')
# 
# # 15 NA
# plotNA.imputations(temp[25880:25900], 
#                    temp_imp[25880:25900],
#                    pch = '')
# 
# # 17 NA
# plotNA.imputations(temp[41695:41720], 
#                    temp_imp[41695:41720],
#                    pch = '')

temp_tbl$temp <- temp_imp

temp_tbl <- temp_tbl %>% 
  select(datetime, temp)

# Daily summary
temp_tsbl <- as_tsibble(temp_tbl,
                        index = datetime) %>%
  index_by(date = date(datetime)) %>% 
  summarize(
    temp_min = min(temp, na.rm = TRUE),
    temp_avg = mean(temp, na.rm = TRUE),
    temp_max = max(temp, na.rm = TRUE)
  )

# Save file ---------------------------------------------------------------

temp_tsbl %>% 
  select(date,
         temp = temp_avg) %>% 
  write_csv(path = "data/temp.csv")

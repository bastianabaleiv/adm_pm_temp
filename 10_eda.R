# Packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(lubridate)

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

# Load data ---------------------------------------------------------------

load('data/adm_pm_temp.RData')

# Time Series Plots -------------------------------------------------------

adm_plot <- ggplot(adm_pm_temp, aes(date, adm)) +
  geom_line() +
  theme_half_open() +
  background_grid() +
  geom_vline(
    xintercept = as.numeric(ymd("2017-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) +
  geom_vline(
    xintercept = as.numeric(ymd("2018-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) 

pm_plot <- ggplot(adm_pm_temp, aes(date, pm)) +
  geom_line() +
  theme_half_open() +
  background_grid() +
  geom_vline(
    xintercept = as.numeric(ymd("2017-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) +
  geom_vline(
    xintercept = as.numeric(ymd("2018-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) 

temp_plot <- ggplot(adm_pm_temp, aes(date, temp_avg)) +
  geom_line() +
  theme_half_open() +
  background_grid() +
  geom_vline(
    xintercept = as.numeric(ymd("2017-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) +
  geom_vline(
    xintercept = as.numeric(ymd("2018-01-01")),
    linetype = 'dashed',
    colour = cbbPalette[3],
    size = 0.6
  ) 

legend <- get_legend(adm_plot)

plot_grid(
  adm_plot + theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) + scale_x_date(date_breaks = "1 year"),
  pm_plot + theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) + scale_x_date(date_breaks = "1 year"),
  temp_plot + scale_x_date(date_breaks = "1 year", date_labels = "%Y"),
  ncol = 1, align = "v"
)

# Adm vs Temp -------------------------------------------------------------

adm_pm_temp %>%
  as.data.frame() %>%
  ggplot(aes(x = temp_min, y = adm)) + geom_point() +
  xlab("Min. Temperature (degrees Celsius)") +
  ylab("Admissions (person)") +
  theme_half_open() +
  background_grid() 

# Adm vs PM 2.5 -----------------------------------------------------------

adm_pm_temp %>%
  as.data.frame() %>%
  ggplot(aes(x = pm, y = adm)) + geom_point() +
  xlab("PM 2.5 (ug/m3)") +
  ylab("Admissions (person)") +
  theme_half_open() +
  background_grid() 

# PM 2.5 vs Temp ----------------------------------------------------------

adm_pm_temp %>%
  as.data.frame() %>%
  ggplot(aes(x = temp_min, y = pm)) + geom_point() +
  xlab("Min. Temperature (degrees Celsius)") +
  ylab("PM 2.5 (ug/m3)") +
  theme_half_open() +
  background_grid() 




# library(GGally)
# 
# ggpairs(adm_pm_temp[c("adm","pm","temp_min")],
#         columns = 1:3,
#         title = "",
#         axisLabels = "show")
# Thu Sep 26 23:19:33 2019 ------------------------------


# load_tbl <- load_tbl %>% mutate(set = if_else((year == 2013 | year ==  2014), 'train',
#                                     if_else(year == 2015, 'validation', 'test')))
#                                  
# load_melt <- melt(load_tbl, 
#              id.vars = c('datetime','year','set'), 
#              measure.vars = 'load')
# 
# load_plot <- adm_pm_temp %>%
#     ggplot(aes(x = date, y = value, colour = as.factor(set))) +
#     geom_line() +
#     background_grid(minor = 'xy') +
#     scale_y_continuous(
#         labels = function(x)
#             format(
#                 x,
#                 big.mark = '.',
#                 decimal.mark = ',',
#                 scientific = FALSE
#             )
#     ) +
#     geom_vline(
#         xintercept = as.numeric(ymd_hm('2015-01-01 00:00')),
#         linetype = 'dashed',
#         colour = 'red',
#         size = 0.8
#     ) +
#     geom_vline(
#         xintercept = as.numeric(ymd_hm('2016-01-01 00:00')),
#         linetype = 'dashed',
#         colour = 'red',
#         size = 0.8
#     ) +
#     scale_color_grey() +
#     labs(y = 'Electricity load (MW)',
#          x = 'Date') +
#     theme(legend.position = 'none') +
#     annotate(
#         geom = 'text',
#         x = ymd_hm('2014-01-01 00:00'),
#         y = 87000,
#         label = 'Training set',
#         color = 'black'
#     ) +
#     annotate(
#         geom = 'text',
#         x = ymd_hm('2015-07-01 00:00'),
#         y = 87000,
#         label = 'Validation set',
#         color = 'black'
#     ) +
#     annotate(
#         geom = 'text',
#         x = ymd_hm('2016-07-01 00:00'),
#         y = 87000,
#         label = 'Test set',
#         color = 'black'
#     )
# 
# save_plot('figures/load_plot.png', load_plot, base_aspect_ratio = 2)



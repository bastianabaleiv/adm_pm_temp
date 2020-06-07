# Packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(lubridate)

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

theme_setting <- theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color="grey90", size=0.5),
  panel.grid.major.x = element_line(color="grey90", size=0.5),
  panel.border = element_rect(fill=NA, color="grey20")#,
 # axis.text = element_text(family="Times"),
 # axis.title = element_text(family="Times"),
 # plot.title = element_text(size=10, hjust=0.5, family="Times")
 )

source("functions.R")

# Load data ---------------------------------------------------------------

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

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

temp_plot <- ggplot(adm_pm_temp, aes(date, temp)) +
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
  ggplot(aes(x = temp, y = adm)) + geom_point() +
  xlab("Temperature [°C]") +
  ylab("Admissions [person]") +
  theme_half_open() +
  background_grid() 

# Adm vs PM 2.5 -----------------------------------------------------------

adm_pm_temp %>%
  as.data.frame() %>%
  ggplot(aes(x = pm, y = adm)) + geom_point() +
  xlab(expression(paste("PM 2.5 [ug/",m^{3},"]"))) +
  ylab("Admissions [person]") +
  theme_half_open() +
  background_grid() 

# PM 2.5 vs Temp ----------------------------------------------------------

adm_pm_temp %>%
  ggplot(aes(x = temp, y = pm, color = adm)) + geom_point() +
  xlab("Temperature [°C]") +
  ylab(expression(paste("PM 2.5 [ug/",m^{3},"]"))) +
  theme_half_open() +
  background_grid() +
  scale_color_gradient(low = "green", high = "red")

# ACF/PACF ----------------------------------------------------------------

# Adm

adm_tscf <- plot_tscf(adm_pm_temp$adm,
         lag = 91,
         title = "Admissions",
         breaks = 7)

adm_tscf$acf
adm_tscf$pacf

# Temp

temp_tscf <- plot_tscf(adm_pm_temp$temp,
                       lag = 91,
                       title = "Temperature",
                       breaks = 7)

temp_tscf$acf
temp_tscf$pacf

# PM

pm_tscf <- plot_tscf(adm_pm_temp$pm,
                       lag = 91,
                       title = "PM 2.5",
                       breaks = 7)

pm_tscf$acf
pm_tscf$pacf

# Calendar HeatMap
library("sugrrants") # [!]
library(viridis)

adm_heatmap <- adm_pm_temp %>% 
  frame_calendar(x = 1, y = 1, date = date, calendar = "monthly", ncol = 12) %>% 
  ggplot(aes(x = .x, y = .y)) +
  geom_tile(aes(fill = adm), colour = "grey50") +
  scale_fill_viridis()

prettify(adm_heatmap, 
         label = c("label","text"), 
         label.padding = unit(0.1, "lines"),
         week_start = -1)

# adm_pm_temp %>%
#   ggplot(aes(x = weekday, y = adm, color = temp)) + geom_point() +
#  # xlab("Temperature [°C]") +
#  # ylab(expression(paste("PM 2.5 [ug/",m^{3},"]"))) +
#   theme_half_open() +
#   background_grid() +
#   scale_color_gradient(low = "green", high = "red")

ggplot(adm_pm_temp, aes(x = weekday, y = adm, group = weekday)) +
  geom_boxplot(fill = "gray") +
  labs(title = "", x = "Weekday", y = "Admission") +
  theme_classic()

ggplot(adm_pm_temp, aes(x = month, y = adm, group = month)) +
  geom_boxplot(fill = "gray") +
  labs(title = "", x = "Month", y = "Admission") +
  theme_classic()

ggplot(adm_pm_temp, aes(x = weekend, y = adm, group = weekend)) +
  geom_boxplot(fill = "gray") +
  labs(title = "", x = "Month", y = "Admission") +
  theme_classic()

ggplot(adm_pm_temp, aes(x = day, y = adm, group = day)) +
  geom_boxplot(fill = "gray") +
  labs(title = "", x = "Day", y = "Admission") +
  theme_classic()


library(GGally)

ggpairs(adm_pm_temp[c("adm","pm","temp")],
        columns = 1:3,
        title = "",
        axisLabels = "show")

# 
# #############3
# ###
# library(latticeExtra) 
# 
# # create data
# set.seed(1) 
# data <- data.frame(x = rnorm(100), y = rnorm(100)) 
# data$z <- with(data, x * y + rnorm(100, sd = 1)) 
# 
# # showing data points on the same color scale 
# levelplot(adm ~ pm * temp, adm_pm_temp,
#           panel = panel.levelplot.points, cex = 1.2) 
# 
# 
# ###
# 
# library("scatterplot3d")
# scatterplot3d(adm_pm_temp[,c("pm","temp","adm")],
#               pch = 16,
#               type = "h")
# 
# ggplot(adm_pm_temp,
#        aes(temp, pm, z = adm)) +
#   geom_contour_filled()
# 



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



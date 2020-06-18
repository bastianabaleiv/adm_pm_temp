# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(fpp3)

adm_pm_temp <- read_csv('data/adm_pm_temp.csv')

train <- adm_pm_temp %>% 
  filter(date < "2017-01-01") %>% 
  as_tsibble()

fitted_tslm <- tsibble(train) %>%
  model(
    tslm = TSLM(log(adm) ~ pm + temp + temp^2 + weekend + weekday + month)
  )

report(fitted_tslm)

augment(fitted_tslm) %>% features(.resid, ljung_box, lag = 10, dof = 5)

augment(fitted_tslm) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = adm, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  xlab("Year") + ylab(NULL) +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=NULL))

augment(fitted_tslm) %>%
  ggplot(aes(x=adm, y=.fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

fitted_tslm %>% gg_tsresiduals()

df <- left_join(train, residuals(fitted_tslm, by = "Day"))
p1 <- ggplot(df, aes(x=adm, y=.resid)) +
  geom_point() + ylab("Residuals")
p2 <- ggplot(df, aes(x=pm, y=.resid)) +
  geom_point() + ylab("Residuals")
p3 <- ggplot(df, aes(x=temp, y=.resid)) +
  geom_point() + ylab("Residuals")
p4 <- ggplot(df, aes(x=weekday, y=.resid)) +
  geom_point() + ylab("Residuals")
p1
p2
p3
p4 # Boxplot

augment(fitted_tslm) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

###### 
# fit <- tsibble(train) %>% 
#   model(ARIMA(adm ~ pm + temp))
# 
# fit <- tsibble(train) %>%
#   model(ARIMA(log(adm) ~ pm + temp + trend() + season() + factor(weekday) + lag(temp,1) + lag(temp,2) + lag(pm,2)))


fit <- tsibble(train) %>%
  model(ARIMA(log(adm) ~ pm + temp + trend() + season() + lag(temp,1) + lag(temp,2) + lag(pm,2)))

fit %>% 
  forecast(h = "1 month", temp = 28, pm = 2)

report(fit)

augment(fit) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

fit %>% gg_tsresiduals()

augment(fit) %>% features(.resid, ljung_box, lag = 10, dof = 5)

augment(fit) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = adm, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  xlab("Year") + ylab(NULL) +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=NULL))

dcmp <- train %>%
  model(STL(adm ~ season(window = Inf)))
components(dcmp)
components(dcmp) %>% autoplot()

library(tidyverse)
library(inspectdf)
library(dplyr)
library(lubridate)
library(skimr)
library(timetk)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)
library(forecast)
library(tidymodels)
library(highcharter)
library(data.table)
library(mice)

#Review data
temp <- fread('daily-minimum-temperatures-in-me (1).csv')
temp %>% view()
temp %>% dim()
temp %>% skim
temp %>% glimpse()
temp %>% inspect_na()
temp[! complete.cases(temp),] %>% view()
temp <-temp %>% mice(method='rf', seed=123)
temp <- temp %>% complete()
temp %>% inspect_na()


colnames(temp) <- c('Date','Daily_temp')
lapply(temp,class)

temp$Date <- temp$Date %>% as.Date('%m/%d/%Y')

temp$Daily_temp <- temp$Daily_temp %>% as.integer()


temp %>% plot_time_series(Date,Daily_temp,
                          .color_var = lubridate::year(Date),
                          .color_lab = "Year",
                          .interactive = T,
                          .plotly_slider = T)


temp %>%
  plot_seasonal_diagnostics(
    Date, Daily_temp, .interactive = T)


temp %>%
  plot_acf_diagnostics(
    Date, Daily_temp, .lags = "1 year", .interactive = T)

# 1.Building h2o::automl().

# • preparing data using tk_augment_timeseries_signature()


all_time <- temp %>% tk_augment_timeseries_signature()

all_time %>%  skim()


temp <- all_time %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)




#• setting stopping metric to “RMSE”
#• setting exclude_algos = c("DRF", "GBM","GLM",'XGBoost')
h2o.init()

train_temp <- temp %>% filter(year < 1988) %>% as.h2o()
test_temp <- temp %>% filter(year >= 1988) %>% as.h2o()

y <- 'Daily_temp'
x <- temp %>% select(-Daily_temp) %>% names()

model_temp<- h2o.automl(
  x = x, y = y, 
  training_frame = train_temp, 
  validation_frame = test_temp,
  leaderboard_frame = test_temp,
  stopping_metric = "RMSE",
  seed = 123, nfolds = 10,
  exclude_algos = c("DRF", "GBM","GLM",'XGBoost'),
  max_runtime_secs = 240) 

model_temp@leaderboard %>% as.data.frame()
leader <- model_temp@leader

pred_temp <- leader %>% h2o.predict(test_temp)

leader %>% h2o.rmse(train=T,
                    valid = T,
                    xval=T)

error_tbl <- temp %>% 
  filter(lubridate::year(Date) >= 1988) %>% 
  add_column(pred = pred_temp %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = Daily_temp) %>% 
  select(Date,actual,pred)

highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='yellow', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')


#----------------------------------------------
#2. Build modeltime::arima_reg(). and setting engine to “auto_arima”

train <- temp %>% filter(Date < "1988-01-01") %>%  as.data.frame()
test <- temp%>% filter(Date >= "1988-01-01") %>%  as.data.frame()

#Auto ARIMA

model_fit_arima<- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Daily_temp ~ Date, train)

calibration <- modeltime_table(
  model_fit_arima) %>%
  modeltime_calibrate(test)


calibration %>% 
    modeltime_forecast(actual_data = temp) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)


# Accuracy ----
calibration %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)








#RMSE for H2o leader model is 2.927
#RMSE for arima model is 4.04(accuracy table)

#It means we forecast temperatures according to h2o model


forecast_data <- seq(as.Date("1991-01-01"), as.Date("1991-12-01"), "months") %>%
  as_tibble() %>% 
  add_column(Daily_temp=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

forecast_h2o <- forecast_data %>% as.h2o()

new_predictions <- leader %>% 
  h2o.predict(forecast_h2o) %>% 
  as_tibble() %>%
  add_column(Date=forecast_data$Date) %>% 
  select(Date,predict) %>% 
  rename(Daily_temp=predict)

temp %>% dim()

forecast_h2o %>% dim()


temp %>% 
  bind_rows(new_predictions) %>% 
  mutate(colors=c(rep('Actual',3650),rep('Predicted',12))) %>% 
  hchart("line", hcaes(Date, Daily_temp, group = colors)) %>% 
  hc_title(text='Forecast') %>% 
  hc_colors(colors = c('red','blue'))






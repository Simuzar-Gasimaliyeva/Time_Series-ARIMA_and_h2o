library(tidyverse)
library(data.table)
library(lubridate)
library(timetk)
library(h2o)
library(tidymodels)
library(modeltime)
library(inspectdf)
library(forecast)
library(rsample)

data <- fread("daily-minimum-temperatures-in-me.csv")
data %>%  glimpse()

data %>% inspect_na()

names(data) <- data %>% names() %>% gsub("Daily minimum temperatures","Min_temp", .)

data$Min_temp <- data$Min_temp %>% parse_number()
data$Date <- data$Date %>% as.Date(., "%m/%d/%Y")


#  Building h2o.automl()-----
h2o.init()

data <- data %>% tk_augment_timeseries_signature(Date) %>% select(Min_temp,everything())

data %>% inspect_na()
data$diff %>% unique()
data$diff %>% table()
data[is.na(data$diff),]$diff<- 86400

data %>% dim()
data %>% glimpse()

data$month.lbl <- data$month.lbl %>% as.character()
data$wday.lbl <- data$wday.lbl %>% as.character()

splits <- initial_time_split(data,prop = 0.8)

train <- training(splits) %>% as.h2o()
test <- testing(splits) %>% as.h2o()

target <- data[,1] %>% names()
features <- data[,-1] %>% names()


model <- h2o.automl(
  x = features, y = target, 
  training_frame = train, 
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  exclude_algos = c("DRF", "GBM","GLM","XGBoost"),
  nfolds = 10, seed = 123, 
  max_runtime_secs = 180
  ) 

model@leaderboard %>% as.data.frame() 
model <- model@leader

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

# Model evaluation----
test_set <- test %>% as.data.frame()
residuals=test_set$Min_temp-y_pred$predict

comparison <-cbind(actual=test_set$Min_temp,predicted=y_pred$predict) 

# RMSE----
RMSE=sqrt(mean(residuals^2))
RMSE

# Adjusted R squared----
y_test_mean=mean(test_set$Min_temp)

tss=sum((test_set$Min_temp-y_test_mean)^2)
rss=sum(residuals^2)

R2=1-(rss/tss)
R2

n <- test_set %>% nrow()
k <- features %>% length()

Adjusted_R2=1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE=round(RMSE),R2,Adjusted_R2)

#  Building model arima_reg()----
interactive <- FALSE

data <- fread("daily-minimum-temperatures-in-me.csv")
data %>%  glimpse()

data %>% inspect_na()

names(data) <- data %>% names() %>% gsub("Daily minimum temperatures","Min_temp", .)

data$Min_temp <- data$Min_temp %>% parse_number()
data$Date <- data$Date %>% as.Date(., "%m/%d/%Y")

data %>% glimpse()

splits <- initial_time_split(data,prop = 0.8)

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Min_temp ~ Date, data=training(splits))

model_table <- modeltime_table(model_fit_arima_no_boost)

# Calibration---
calibration_table <- model_table %>% modeltime_calibrate(new_data = testing(splits))

calibration_table %>% modeltime_forecast(
  new_data = testing(splits),
  actual_data = data
) %>% plot_modeltime_forecast(
  .legend_max_width = 25,
  .interactive = interactive
)

calibration_table %>% 
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(
    .interactive = interactive
  )

# Forecasting temperatures for next year with model which has lower RMSE---
calibration_table <- model_fit_arima_no_boost %>%
modeltime_calibrate(new_data = testing(splits))

calibration_table %>% modeltime_forecast(h="1 year",
                                         new_data = testing(splits),
                                         actual_data = data
) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive = interactive
  )



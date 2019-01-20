# sessions first 

train_meta <- read_csv("./data/otb_interview_task__train__meta_time_series.csv") 

# tidy
train_meta$Date <- dmy(train_meta$Date)

# join datasets together 
data_raw <- train_meta %>% 
  mutate(weather = 1) %>%
  spread(`Weather Index`, weather, fill = 0)

# let's build the datset to what it was before and restric the dates so we have only the test date range
date_from <- dmy("01-01-2018") 
date_to <- max(data_raw$Date) 

dates <- data_raw$Date

movav_k <- 10
adstock_rate <- 0.8

data_all_time <- data_raw %>% 
  transmute( 
    # --------INPUT VARIABLE NAMES HERE -------   
    DayFriday = ifelse(weekdays(Date) == "Wednesday", 1, 0),
    DaySaturday = ifelse(weekdays(Date) == "Saturday", 1, 0),
    DaySunday = ifelse(weekdays(Date) == "Sunday", 1, 0),
    TV_adstock = log(adstock(`TV Ad Reach`, rate = adstock_rate) +1),
    `Consumer Confidence Index`,
    exchange_rate_moving_av = rollmean(`Exchange Rate`, movav_k, fill =  "extend", align = "right"),
    `Online Visibility`,
    online_visibility_exp = `Online Visibility`^4,
    better_than_average_weather_days = `better than average`,
    worse_than_Average_weather_days = `worse than average`
  ) %>% 
  scale() %>% 
  data.frame() %>% 
  bind_cols(Date = dates)

data_test <- data_all_time %>% filter(Date >= date_from, Date <= date_to)

# now retrieve the models 
sessions_models <-  output %>% 
  transpose() %>% 
  pluck('result') %>% 
  transpose() %>% 
  pluck('model') 

sessions_predictions <- sessions_models %>%
  map_df(predict, newdata = data_test) %>% 
  bind_cols(Date = data_test$Date) %>% 
  gather(product_id, sessions, -Date) %>%
  select(product_id, date = Date, sessions)

# test to see if it looks sensible.

product <- 91

train_product %>% 
  select(-bookings) %>%
  bind_rows(sessions_predictions) %>% 
  filter(product_id == product) %>%
  ggplot(aes(x = date, y = sessions)) + geom_line()

# sessions seem to be down - let's see why this is. It looks liek exchange rate is responsable 
data_all_time %>%
  select(-DayFriday, -DaySaturday, -DaySunday) %>%
  gather(variable, value, -Date) %>% 
  ggplot(aes(x = Date, y = value, col = variable)) + geom_line()

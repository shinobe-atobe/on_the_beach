library(broom)

build_mmm  <-  function(dep_var_sess_book = "sessions", product = 0, adstock_rate = 0.8, movav_k = 7) {
  
data <- data_raw %>% 
  mutate(Date = ymd(date)) %>%
  group_by(product_id) %>% mutate(
    
    #-----INPUT NEW VARIABLES HERE---------
    TV_adstock = adstock(`TV Ad Reach`, rate = adstock_rate), #Adstockrates <- tibble(Country = list_of_markets, Adstock = c(0.4, 0.8, 0.8, 0.92, 0.8)) 
    #Day = ifelse(weekdays(Date) == "Monday", 0, weekdays(Date)),
    DayFriday = ifelse(weekdays(Date) == "Wednesday", 1, 0),
    DaySaturday = ifelse(weekdays(Date) == "Saturday", 1, 0),
    DaySunday = ifelse(weekdays(Date) == "Sunday", 1, 0),
    xmas = ifelse(Date %in% dmy(c('25/12/2017', '25/12/2016', '25/12/2015', '25/12/2014')), 1 ,0), #dummy variable
    day_number = day(Date),
    month = month(Date),
    #Seasonality = predict(loess(Visibility ~ day_number, span=0.15), day_number)
    #Seasonality = rollmean(x = Temperature.Fahrenheit, 30, align = "right", fill = NA)
    exchange_rate_lag1 = lag(`Exchange Rate`),
    exchange_rate_moving_av = rollmean(`Exchange Rate`, movav_k, fill =  "extend", align = "right")
    
  ) %>% 
  ungroup() %>%
  filter(Date >= date_from, Date <= date_to) %>%
  select( # try transmute here
    # --------INPUT VARIABLE NAMES HERE -------   
    Date,
    product_id,
    Dep_Var = one_of(dep_var_sess_book),
    #bookings,
    #Day,
    DayFriday,
    DaySaturday,
    DaySunday,
    month,
    TV_adstock,
    #xmas,
    `Consumer Confidence Index`,
    #`Exchange Rate`,
    exchange_rate_moving_av,
    `Online Visibility`,
    #average_weather_days = average,
    better_than_average_weather_days = `better than average`,
    worse_than_Average_weather_days = `worse than average`
    
  ) %>%
  filter(product_id %in% product) %>% select(-product_id)

#estimate / remove NAs if nececary - exclude any categorical variables. Should check data first in case there are lots of NAs
names(data); head(data); nrow(data)
data[4:ncol(data)] <- na.approx(data[4:ncol(data)], rule = 2)

#run model
fit <- lm(select(data, Dep_Var, everything(), -Date))

#contribution of each variable
variables <- data %>%
  mutate(Intercept = 1) %>%
  select( Intercept, everything(), -Date, -Dep_Var)

Contribution <- bind_cols(select(data, Date, Dep_Var), data.frame(mapply(`*`,variables,fit$coefficients)))

# Smooth variables
cont_chart_data <- Contribution %>% 
  select(-Dep_Var) %>%
  gather(Var_Name, value, 2:ncol(.)) %>%
  left_join(variable_groupings, by = 'Var_Name') %>% 
  group_by(Date, 
           Grouping) %>%
  summarise(value = sum(value)) %>%
  spread(Grouping, value) %>%
  data.frame() %>% 
  mutate_at(3:8, rollmean, k = 7, allign = 'centre', fill = "extend") 

# find the mins of each continuous variable
min_seas <-  min(cont_chart_data$Seasonality)
min_conf <-  min(cont_chart_data$Consumer_Confidence)
min_fx <-  min(cont_chart_data$Exchange_Rate)
min_onl <-  min(cont_chart_data$Online_Visibility)

# normalise and re-level the base
contributions <- cont_chart_data %>% 
  mutate(Base_Sales = Base_Sales + min(Online_Visibility) + min(Exchange_Rate) + min(Consumer_Confidence), # max(.$Seasonality)
         Exchange_Rate = Exchange_Rate - min(Exchange_Rate),
         Online_Visibility = Online_Visibility - min(Online_Visibility),
         #Seasonality = Seasonality - min(Seasonality),
         Consumer_Confidence = Consumer_Confidence - min(Consumer_Confidence)
  ) %>% 
  select(-Date) %>% 
  colSums()

# create table to output
prcnt_TV <- fit %>%
  tidy() %>% 
  filter(term == 'TV_adstock') %>% 
  select(estimate) %>% 
  mutate(estimate = estimate * sum(data$TV_adstock)) %>% pull()

tibble(total_metric = sum(data$Dep_Var), 
       prcent_driven_by_TV = prcnt_TV,
       rsquared = summary(fit)$r.squared)
       

}

map_df(list_of_products, safely(build_mmm))

names(list_of_products) <- paste0("product_", list_of_products)

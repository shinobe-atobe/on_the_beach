library(tidyverse);library(shiny);library(zoo); library(lubridate)
source("C:/Users/Phil/Google Drive/SumUp/Sumup TV Attribution/Functions.R") #load functions

train_meta <- read_csv("./data/otb_interview_task__train__meta_time_series.csv") 
train_product <- read_csv("./data/otb_interview_task__train__product_time_series.csv")

# tidy
train_meta$Date <- dmy(train_meta$Date)
train_product$product_id = as.character(train_product$product_id)

# create a total product
total_product <- train_product %>% 
  group_by(date) %>% 
  summarise(sessions = sum(sessions),
          bookings = sum(bookings)) %>% 
  transmute(product_id = "Total", date, sessions, bookings)

# join datasets together 
data_raw <- train_product %>% 
  bind_rows(total_product) %>%
  left_join(train_meta, by = c('date' = 'Date') ) %>%
  mutate(weather = 1) %>%
  spread(`Weather Index`, weather, fill = 0)

# restrict model parameters if neccecary
date_from <- min(train_product$date)  
date_to <- max(train_product$date) 
list_of_products <- unique(train_product$product_id)

###########################################################
#---------------Code for building MMM----------------------
###########################################################

product <- "Total"  # choose product
movav_k <- 10
adstock_rate <- 0.8

dates <- data_raw %>% 
  filter(product_id == product) %>%
  select(date) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= date_from, date <= date_to) %>%
  as.vector()

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
#month = month(Date, label = T, abbr = F),
#Seasonality = predict(loess(Visibility ~ day_number, span=0.15), day_number)
#Seasonality = rollmean(x = Temperature.Fahrenheit, 30, align = "right", fill = NA)
exchange_rate_lag1 = lag(`Exchange Rate`),
exchange_rate_moving_av = rollmean(`Exchange Rate`, movav_k, fill =  "extend", align = "right")

) %>% 
ungroup() %>%
  filter(Date >= date_from, Date <= date_to) %>%
  transmute( 
# --------INPUT VARIABLE NAMES HERE -------   
Date,
product_id,
Dep_Var = sessions,
#bookings,
#Day,
DayFriday,
DaySaturday,
DaySunday,
#month,
TV_adstock = log(TV_adstock +1),
#xmas,
`Consumer Confidence Index`,
#`Exchange Rate`,
exchange_rate_moving_av,
`Online Visibility`,
#trend,
#average_weather_days = average,
better_than_average_weather_days = `better than average`,
worse_than_Average_weather_days = `worse than average`

) %>% 
  filter(product_id %in% product) %>% select(-product_id) 

# optional scaling
data <- bind_cols(data %>% select(Date, Dep_Var, TV_adstock), 
                  data %>% select(-Date, -Dep_Var, -TV_adstock) %>% 
                    scale() %>% data.frame())

#estimate / remove NAs if nececary - exclude any categorical variables. Should check data first in case there are lots of NAs
names(data); head(data); nrow(data)
#data[4:ncol(data)] <- na.approx(data[4:ncol(data)], rule = 2)

#run model
fit <- lm(select(data, Dep_Var, everything(), -Date))
summary(fit)

#RMSE
sqrt( c(crossprod(fit$residuals)) / length(fit$residuals) )

#Build Actual vs Model (AVM)
AVM <- data.frame(cbind(Actual = data[["Dep_Var"]], Fitted = fitted(fit)))
AVM <- cbind(Date = dates, AVM)
AVM <- mutate(AVM, Residual = Actual - Fitted)

#AVM plot
ggplot(AVM, aes(x = date)) + 
  geom_line(aes(y = Actual, colour = "Actual")) + 
  geom_line(aes(y = Fitted, colour = "Fitted")) +
  geom_line(aes(y = Residual, colour = "Resid")) 

#interactive AVM plot
ui <- fluidPage(
  plotlyOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({ggplot(AVM, aes(date)) + 
      geom_line(aes(y = Actual, colour = "Actual")) + 
      geom_line(aes(y = Fitted, colour = "Fitted")) +
      geom_line(aes(y = Residual, colour = "Resid")) 
  })
}

shinyApp(ui = ui, server = server)



###############################
#---------Contribution---------
###############################


variable_groupings <- read_csv("Variable Groupings.csv")

variables <- data %>%
  mutate(Intercept = 1) %>%
  select( Intercept, everything(), -Date, -Dep_Var)

Contribution <- bind_cols(select(data, Date, Dep_Var), data.frame(mapply(`*`,variables,fit$coefficients)))

# generate sand diagram
cont_chart_data <- Contribution %>% 
  #mutate(Week_commencing = ymd('0000-01-01') + years(year(Date)) + weeks(week(Date) - 1) ) %>% 
  #group_by(Week_commencing) %>% 
  select( -Dep_Var) %>%
  #summarise_all(funs(sum)) %>% 
  gather(Var_Name, value, 2:ncol(.)) %>%
  left_join(variable_groupings, by = 'Var_Name') %>% 
  group_by(Date, 
           Grouping) %>%
  summarise(value = sum(value)) %>%
  spread(Grouping, value) %>%
  data.frame() %>% 
  mutate_at(3:8, rollmean, k = 7, allign = 'centre', fill = "extend")

min_seas <-  min(cont_chart_data$Seasonality)
min_conf <-  min(cont_chart_data$Consumer_Confidence)
min_fx <-  min(cont_chart_data$Exchange_Rate)
min_onl <-  min(cont_chart_data$Online_Visibility)

cont_chart_data <- cont_chart_data %>% 
  mutate(Base_Sales = Base_Sales + min(Online_Visibility) + min(Exchange_Rate) + min(Consumer_Confidence), # max(.$Seasonality)
          Exchange_Rate = Exchange_Rate - min(Exchange_Rate),
          Online_Visibility = Online_Visibility - min(Online_Visibility),
          #Seasonality = Seasonality - min(Seasonality),
          Consumer_Confidence = Consumer_Confidence - min(Consumer_Confidence)
  ) 

# daily contribution chart
cont_chart_data %>% 
  gather(Grouping, value, 2:ncol(.)) %>% 
  mutate(Grouping = factor(Grouping, levels = c('TV', 'Online_Visibility', 'Exchange_Rate', 'Weather',  'Seasonality', 'Consumer_Confidence', 'Base_Sales'))) %>%
  ggplot(aes(x = Date, y = value)) + geom_area(aes(fill = Grouping), position = 'Stack') +ggtitle(paste(product, "- Sessions"))

# annual stacked contribution chart 
cont_chart_data %>% 
  group_by(year = year(Date)) %>%
  summarise_all(funs(sum)) %>%
  select(-Date) %>% 
  gather(Grouping, value, 2:ncol(.)) %>% 
  mutate(Grouping = factor(Grouping, levels = c('TV', 'Online_Visibility', 'Exchange_Rate', 'Weather',  'Seasonality', 'Consumer_Confidence', 'Base_Sales'))) %>%
  ggplot(aes(x = year, y = value)) + geom_bar(aes(fill = Grouping), position = 'stack', stat = 'identity') +ggtitle(paste(product, "- Sessions"))

 
##########################################
#------MMM function for map loop -----####
##########################################



product <- "Total"  # choose product
movav_k <- 10
adstock_rate <- 0.8

build_mmm <- function(product = "Total") {

dates <- data_raw %>% 
  filter(product_id == product) %>%
  select(date) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= date_from, date <= date_to) %>%
  as.vector()

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
    #month = month(Date, label = T, abbr = F),
    #Seasonality = predict(loess(Visibility ~ day_number, span=0.15), day_number)
    #Seasonality = rollmean(x = Temperature.Fahrenheit, 30, align = "right", fill = NA)
    exchange_rate_lag1 = lag(`Exchange Rate`),
    exchange_rate_moving_av = rollmean(`Exchange Rate`, movav_k, fill =  "extend", align = "right")
    
  ) %>% 
  ungroup() %>%
  filter(Date >= date_from, Date <= date_to) %>%
  transmute( 
    # --------INPUT VARIABLE NAMES HERE -------   
    Date,
    product_id,
    Dep_Var = sessions,
    #bookings,
    #Day,
    DayFriday,
    DaySaturday,
    DaySunday,
    #month,
    TV_adstock = log(TV_adstock +1),
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


# optional scaling
data <- bind_cols(data %>% select(Date, Dep_Var, TV_adstock), data %>% select(-Date, -Dep_Var, -TV_adstock) %>% scale() %>% data.frame())

#estimate / remove NAs if nececary - exclude any categorical variables. Should check data first in case there are lots of NAs
names(data); head(data); nrow(data)
#data[4:ncol(data)] <- na.approx(data[4:ncol(data)], rule = 2)

#run model
fit <- lm(select(data, Dep_Var, everything(), -Date))
#tidy(summary(fit))

variables <- data %>%
  mutate(Intercept = 1) %>%
  select( Intercept, everything(), -Date, -Dep_Var)

Contribution <- bind_cols(select(data, Date, Dep_Var), data.frame(mapply(`*`,variables,fit$coefficients)))

#generate sand diagram
cont_chart_data <- Contribution %>% 
  #mutate(Week_commencing = ymd('0000-01-01') + years(year(Date)) + weeks(week(Date) - 1) ) %>% 
  #group_by(Week_commencing) %>% 
  select( -Dep_Var) %>%
  #summarise_all(funs(sum)) %>% 
  gather(Var_Name, value, 2:ncol(.)) %>%
  left_join(variable_groupings, by = 'Var_Name') %>% 
  group_by(Date, 
           Grouping) %>%
  summarise(value = sum(value)) %>%
  spread(Grouping, value) %>%
  #filter(Week_commencing != '2016-12-30') %>%  
  data.frame() %>% 
  mutate_at(3:8, rollmean, k = 7, allign = 'centre', fill = "extend")

min_seas <-  min(cont_chart_data$Seasonality)
min_conf <-  min(cont_chart_data$Consumer_Confidence)
min_fx <-  min(cont_chart_data$Exchange_Rate)
min_onl <-  min(cont_chart_data$Online_Visibility)

contribution <- cont_chart_data %>% 
  mutate(Base_Sales = Base_Sales + min(Online_Visibility) + min(Exchange_Rate) + min(Consumer_Confidence), # max(.$Seasonality)
         Exchange_Rate = Exchange_Rate - min(Exchange_Rate),
         Online_Visibility = Online_Visibility - min(Online_Visibility),
         #Seasonality = Seasonality - min(Seasonality),
         Consumer_Confidence = Consumer_Confidence - min(Consumer_Confidence)
  ) %>% select(-Date) %>% colSums() 

list(contribition = contribution, model = fit)

}

# unique product IDs
list_of_products <- unique(train_product$product_id)
#names(list_of_products) <- paste0("product_", list_of_products)
names(list_of_products) <- list_of_products

# apply MMM to each one and save output 
output <- map(list_of_products, safely(build_mmm))

# chart 
  output %>% 
  transpose() %>% 
  pluck('result') %>% 
  transpose() %>% 
  pluck('contribition') %>% 
  bind_rows() %>% 
  mutate(variable = c("Base_Sales",
                      "Consumer_Confidence",
                      "Exchange_Rate",
                      "Online_Visibility",
                      "Seasonality",        
                      "TV",
                      "Weather")) %>% 
  select(variable,  everything()) %>% 
  mutate(variable = factor(variable,  levels = c('TV', 'Online_Visibility', 'Exchange_Rate', 'Weather',  'Seasonality', 'Consumer_Confidence', 'Base_Sales'))) %>% 
  gather(product, value, 2:ncol(.)) %>% 
  ggplot(aes(x = product, y = value, fill = variable)) + geom_bar(position = 'fill', stat = 'identity') + 
   coord_cartesian(ylim = c(-0.01 , 1)) + theme(text = element_text(size=10))

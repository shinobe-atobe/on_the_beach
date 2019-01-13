library(tidyverse);library(shiny);library(zoo); library(lubridate)
source("C:/Users/Phil/Google Drive/SumUp/Sumup TV Attribution/Functions.R") #load functions

train_meta <- read_csv("./data/otb_interview_task__train__meta_time_series.csv") 
train_product <- read_csv("./data/otb_interview_task__train__product_time_series.csv")

train_meta$Date <- dmy(train_meta$Date)

data_raw <- train_product %>% 
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

product <- 0  # choose product
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
month = month(Date),
#Seasonality = predict(loess(Visibility ~ day_number, span=0.15), day_number)
#Seasonality = rollmean(x = Temperature.Fahrenheit, 30, align = "right", fill = NA)
exchange_rate_lag1 = lag(`Exchange Rate`),
exchange_rate_moving_av = rollmean(`Exchange Rate`, 10, fill =  "extend", align = "right")

) %>% 
ungroup() %>%
  filter(Date >= date_from, Date <= date_to) %>%
  select( # try transmute here
# --------INPUT VARIABLE NAMES HERE -------   
Date,
product_id,
Dep_Var = sessions,
#bookings,
#Day,
DayFriday,
DaySaturday,
DaySunday,
month,
TV_adstock,
xmas,
`Consumer Confidence Index`,
`Exchange Rate`,
exchange_rate_lag1,
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





#---------Contribution---------
variable_groupings <- read.csv("Variable Groupings.csv")

variables <- data %>%
  mutate(Intercept = 1) %>%
  select( Intercept, everything(), -Date, -Dep_Var)

Contribution <- bind_cols(select(data, Date, Dep_Var), data.frame(mapply(`*`,variables,fit$coefficients)))

#Contribution <- filter(Contribution, Date >= dmy('01/01/2018'))

#generate weekly sand diagram
Contribution %>% 
  mutate(Week_commencing = ymd('0000-01-01') + years(year(Date)) + weeks(week(Date) - 1) ) %>% 
  group_by(Week_commencing) %>% 
  select(-Date, -Dep_Var) %>%
  summarise_all(funs(sum)) %>%
  gather(Var_Name, value, 2:ncol(.)) %>%
  left_join(variable_groupings, by = 'Var_Name') %>% 
  group_by(Week_commencing, Grouping) %>%
  summarise(value = sum(value)) %>%
  spread(Grouping, value) %>%
  #mutate(Base_Sales = Base_Sales - 130 + 210, # max(.$Seasonality)
  #       Seasonality = Seasonality + 130,
  #       Product = Product - 210 ) %>% # max(.$Seasonality
  #select(-Paid_Ads) %>%
  gather(Grouping, value, 2:ncol(.)) %>% 
  mutate(Grouping = factor(Grouping, levels = c('TV', 'Market/Competition', 'Other_Marketing', 'Paid_Ads',  'Seasonality', 'Product', 'Social', 'SEM', 'Price_And_Subsidy', 'Base_Sales'))) %>%
  ggplot(aes(x = Week_commencing, y = value)) + geom_area(aes(fill = Grouping), position = 'Stack') +ggtitle(paste(Market, "- Card Reader Owners"))


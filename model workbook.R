library(tidyverse);library(shiny);library(zoo); library(lubridate)
source("C:/Users/Phil/Google Drive/SumUp/Sumup TV Attribution/Functions.R") #load functions

train_meta <- read_csv("./data/otb_interview_task__train__meta_time_series.csv") 
train_product <- read_csv("./data/otb_interview_task__train__product_time_series.csv")

train_meta$Date <- dmy(train_meta$Date)

data_raw <- train_product %>% 
  left_join(train_meta, by = c('date' = 'Date') ) %>%
  mutate(weather = 1) %>%
  spread(`Weather Index`, weather, fill = 0)

date_from <- min(train_product$date)  
date_to <- max(train_product$date) 
list_of_products <- unique(train_product$product_id)

###########################################################
#---------------Code for building MMM----------------------
###########################################################

product <- 0  # choose market

dates <- data_raw %>% 
  filter(product_id == product) %>%
  select(Date) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= date_from, Date <= date_to) %>%
  as.vector()

data <- data_raw %>% 
  mutate(Date = ymd(Date)) %>%
  group_by(country) %>% mutate(

#-----INPUT NEW VARIABLES HERE---------
TV.costs_Adstock = adstock(TV.costs, rate = 0.1), #Adstockrates <- tibble(Country = list_of_markets, Adstock = c(0.4, 0.8, 0.8, 0.92, 0.8)) 
#Day = ifelse(weekdays(Date) == "Monday", 0, weekdays(Date)),
DayWednesday = ifelse(weekdays(Date) == "Wednesday", 1, 0),
DaySaturday = ifelse(weekdays(Date) == "Saturday", 1, 0),
DaySunday = ifelse(weekdays(Date) == "Sunday", 1, 0),
Adwords_CTR =  adwords.clicks / adwords.impressions,
Marketing_costs_excl_TV = Marketing.costs - TV.costs,
Marketing_Costs_Adstock = adstock(Marketing.costs, rate = 0.3),
xmas = ifelse(Date %in% dmy(c('25/12/2017', '25/12/2016')), 1 ,0), #dummy variable
subsidy_per_unit = subsidy.cost / card.reade.owner,
day_number = day(Date),
Facebook_CTR = facebook.clicks / facebook.impressions,
CRO_to_signups_Ratio = card.reade.owner / signups,
#Seasonality = predict(loess(Visibility ~ day_number, span=0.15), day_number)
Seasonality = rollmean(x = Temperature.Fahrenheit, 30, align = "right", fill = NA),
Subsidy_per_unit_squared = subsidy_per_unit ^ 2


) %>% 
ungroup() %>%
  filter(Date >= date_from, Date <= date_to) %>%
  select(
# --------INPUT VARIABLE NAMES HERE -------   
Date,
#Dep_Var = card.reade.owner,
Dep_Var = signups,
#card.reade.owner,
#Day,
DayWednesday,
DaySaturday,
DaySunday,
country,
#CRO_to_signups_Ratio,
#Dep_Var = visitors,
#Paid.ads.Visitors,
#signups,
#subsidy.cost,
#subsidy_per_unit,
Subsidy_per_unit_squared,
#TV.costs,
#Marketing.costs, 
Marketing_Costs_Adstock,
#Total.Costs,       
#tpv,
xmas,
#adwords.clicks,
#adwords.impressions, 
#facebook.clicks,
#facebook.impressions,
trends.iZettle ,     
trends.paypal.here,
Seasonality,
trends.squareup,
#trends.sumup,
trends.Worldpay,
Temperature.Fahrenheit,
#Rainfall,
Visibility,
Humidity,
Cloud.Coverage,
#Avg..position,
Adwords_CTR,
Facebook_CTR,
#Marketing_costs_excl_TV,
TV.costs_Adstock

) %>%
  filter(country %in% Market) %>% select(-country)
  
#estimate / remove NAs if nececary - exclude any categorical variables. Should check data first in case there are lots of NAs
names(data); head(data)
data[4:ncol(data)] <- na.approx(data[4:ncol(data)], rule = 2)

#run model
fit <- lm(select(data, Dep_Var, everything(), -Date))
summary(fit)

#RMSE
sqrt( c(crossprod(fit$residuals)) / length(fit$residuals) )

#Build Actual vs Model (AVM)
AVM <- data.frame(cbind(Actual = data[[2]], Fitted = fitted(fit)))
AVM <- cbind(Date = dates, AVM)
AVM <- mutate(AVM, Residual = Actual - Fitted)

#AVM plot
ggplot(AVM, aes(x = Date)) + 
  geom_line(aes(y = Actual, colour = "Actual")) + 
  geom_line(aes(y = Fitted, colour = "Fitted")) +
  geom_line(aes(y = Residual, colour = "Resid")) 

#interactive AVM plot
ui <- fluidPage(
  plotlyOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({ggplot(AVM, aes(Date)) + 
      geom_line(aes(y = Actual, colour = "Actual")) + 
      geom_line(aes(y = Fitted, colour = "Fitted")) +
      geom_line(aes(y = Residual, colour = "Resid")) + ggtitle("Netherlands")
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


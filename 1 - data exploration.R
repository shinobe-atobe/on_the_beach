library(tidyverse)

train_meta <- read_csv("./data/otb_interview_task__train__meta_time_series.csv") 
train_product <- read_csv("./data/otb_interview_task__train__product_time_series.csv") 

# 140 individual products
n_distinct(train_product$product_id)

# sessions looks to be rising slowe than bookings
train_product %>%
  group_by(date) %>% 
  summarise_at(vars(sessions, bookings), funs(sum)) %>% 
  gather(metric, value, 2:3) %>% 
  ggplot(aes(x = date, y = value)) + geom_line(aes(col = metric)) + facet_grid(rows = vars(metric), scales = "free_y")

# sessions looks to be rising slowe than bookings - adding conversion rate confirms this. I expect that there may be another factor
# playing a part that is not in the data - changing ratio of existing to new customers (returning customers more likely to convert)
train_product %>%
  group_by(date) %>% 
  summarise_at(vars(sessions, bookings), funs(sum)) %>% 
  mutate(conversion_rate = bookings / sessions) %>% 
  gather(metric, value, 2:4) %>% 
  ggplot(aes(x = date, y = value)) + geom_line(aes(col = metric)) + facet_grid(rows = vars(metric), scales = "free_y")

# surprised theres so much noise, is this all products? - yes. also doesnt seem like any products are particular small in volumes
# there does sttm to be a group of products that increase their sessions in the 2nd half of 2016 and on to 2018 - could be interesting for later
train_product %>%
  gather(metric, value, 3:4) %>% 
  ggplot(aes(x = date, y = value, group = factor(product_id))) + geom_line(size = 0.05, alpha = 0.1) + facet_grid(rows = vars(metric), scales = "free_y")

# each product has enough bookings & sessions to model 
train_product %>%
  group_by(product_id) %>%
  summarise(total_bookings = sum(bookings),
            total_session = sum(sessions)) %>% 
  arrange(desc(total_bookings)) %>% 
  ggplot(aes(x = factor(product_id), y = total_bookings)) + geom_bar(stat = 'identity')
  
# should be ok to model with fixed effects & adstock

# use broom to map over each of the 140 models

# need to find a driver of the upwards trend

# perhaps if bookings are modelled we will have to group by week

# charts showing variable transformations


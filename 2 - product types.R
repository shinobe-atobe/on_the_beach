# create data frame with product and everything we know about that product
# n sold
# up in 2017
# reactiveness to variables
# volatility index
# seasonal
# of each in sessions
# % of each var in bookings
# Conv Rate
# correlation with each other
# https://stats.stackexchange.com/questions/2976/clustering-variables-based-on-correlations-between-them


names <- as.character(sessions_output$variable)

# proportion driven by each variable 
prod_details <- sessions_output %>%
 select(-variable) %>% 
  t() %>%
  as.data.frame() %>% 
  rownames_to_column("product_id") %>%
  set_names(c("product_id", names)) %>% 
  mutate_at(3:8, funs(. / Base_Sales)) %>%
  select(-Base_Sales)

number_of_products <- train_product  %>% 
  mutate(conv_rate = bookings / sessions) %>% 
  group_by(product_id) %>% 
  summarise(n_total_sessions = sum(sessions), 
            n_total_bookings = sum(bookings),
            sd_total_sessions = sd(sessions),
            sd_total_bookings = sd(bookings),
            av_conv_rate = mean(conv_rate),
            uplift_2017 = sum(sessions[year(date) == 2017]) / sum(sessions[year(date) == 2015]))

prod_details <- prod_details %>% 
  left_join(number_of_products, by = 'product_id')

prod_details_scaled <- prod_details %>%
  select(-product_id) %>% 
  scale() %>%
  data.frame()

# try from 1 to 15 clusters to see what the optimal number is 
kmeans_fun <-  function(x) {
  obj <- kmeans(na.omit(prod_details_scaled), x)
  return(obj$tot.withinss)
}

plot(sapply(1:15, kmeans_fun))

# found there were 3 clusters
obj <- kmeans(prod_details_scaled, 3)

prods_with_cluster <- bind_cols(prod_details_scaled, cluster = obj$cluster)

# summarise each of the cluster
av <- prods_with_cluster %>% 
  group_by(cluster) %>% 
  summarise_all(mean) %>%
  gather(var, val, -cluster)


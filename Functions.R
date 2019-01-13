#---------------Define Functions------------------

#adstock
adstock <- function(x, rate) {
  
  adstocked_variable <- x 
  
  for (i in 1:(length(x)-1) ) {
    adstocked_variable[i+1] <-  x[i + 1] + (rate * adstocked_variable[i])
  }
  adstocked_variable
}

#theme for charts
phil_theme <- theme(#panel.grid.major.x = element_blank(), 
  #panel.grid.major.y = element_line(colour='#d4d4d4'), 
  #panel.grid.minor = element_blank(), 
  #panel.background = element_rect(fill='#efefef'), 
  #plot.background = element_rect(fill='#efefef'), 
  #strip.background =element_rect(fill='#efefef'), 
  #text = element_text(family="Helvetica Neue"), 
  #axis.line = element_line(color="black", size = 2),
  strip.text.x = element_text(hjust = 1, size = 20),
  #axis.title.y = element_text(margin = margin(r=15), size = 20),
  axis.title.y = element_text(size = 20),
  axis.title.x = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20), size = 20)
  #legend.key=element_blank(),
  #plot.margin = unit(c(1,1,1,1), "cm")
) 

#generate dummy variables on the fly
gen_dummy <- function(dummy_name, dateDDMMYYYY, country_code = list_of_markets) {
  dummy <- tibble(Date = dmy(dateDDMMYYYY)) 
  dummy[dummy_name] <- 1 #create tables with date and number 1
  dummy <- bind_rows(replicate(length(country_code), dummy, simplify = FALSE), .id = 'Position') #repeat for all countries
  dummy$country <- country_code[as.numeric(dummy$Position)] #repeat for all countries
  dummy$Position <- NULL #tidy up
  data %>% left_join(dummy, by = c("Date", "country")) #merge with total dataset
  data[dummy_name][is.na(data[dummy_name])] <<- 0 #tidy up
  return(dummy)
}

#Var Vs Dep
Variables_VS_Sales <- function(Dep_Var, variable) {
  data %>% 
    select(1, Dep_Var, variable) %>% 
    mutate_at(.funs = funs(scale), .vars = c(Dep_Var, variable)) %>%
    ggplot(aes(x = Date)) +geom_line(aes_string(y = Dep_Var)) + geom_line(aes_string(y = variable), stat = 'identity', col = 'red')
}

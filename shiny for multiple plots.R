library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(reshape2)
library(readxl)




# organise dates
dates <- select(data_raw, order_date)
dates <- unique(as.Date(dates$order_date, format="%d/%m/%Y"))

#interactive AVM plot
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Plot", 
             selectInput("select", label = h3("Zip Code"), choices = c('Volkspark' = 10119,'North Mitte' = 10115,'South Friedrichshein' = 10245,'Kolwitzplatz'= 10435,'Mitte' = 10117,'Friedrichshein' = 10243,'G?rlitzer Park' = 10999,'WrangelKeiz' = 10997,10407,'Urbanstrasse' = 10967, 'Sonnenallee' = 12047, 'Total Berlin' = 'Total Berlin')), 
             radioButtons("radio", label = h3("Dependant Variable"), choices = list("Re-orders" = 'Num_Orderss', "Acquisitions" = 'Num_Acquisitions'), selected = 'Num_Orderss'),
             #radioButtons("radio2", label = h3("Media in"), choices = list("yes" = 'yes', "no" = 'no'), selected = 'no'),
             radioButtons("Period", label = "Period", choices = list("Days" = "Days", "Weeks" = "Weeks"), selected = 'Days') ,
             plotlyOutput("AVMPlot"),
             plotlyOutput("MediaPlot")),
    
    tabPanel("Summary", 
             verbatimTextOutput("fit"))
  )
)

server <- function(input, output) {
  
  data <- reactive({
    if(input$radio == 'Num_Orderss') {
      data_raw %>% filter(customer_zip == input$select)  %>% mutate(Cumulative_Aqs = cumsum(Num_Acquisitions)) %>% select(-order_date, -customer_zip, -UniqueID, -Num_Acquisitions, -Foodora_Rel_to_market)
    } else {
      data_raw %>% filter(customer_zip == input$select)  %>% mutate(Cumulative_Aqs = cumsum(Num_Acquisitions)) %>% select(-order_date, -customer_zip, -UniqueID, -Num_Orderss, -Cumulative_Aqs, -Foodora_Rel_to_market)  
    } 
  })
  
  
  #    data <- reactive({
  #     if(input$Period == 'Weeks') {
  #      data() %>% group_by(Week) %>% summarise_each(funs(sum)) 
  #       }
  #       })
  # 
  #   data <- reactive({
  #     if(input$radio2 == 'no') {
  #       select(data, -Coloumns, -Panels, -Posters)
  #     } 
  #   })
  
  
  fit <- reactive({lm(data())})
  output$fit <- renderPrint({summary(fit())})
  
  #Build Actual vs Model (AVM)
  AVM <- reactive({mutate(cbind(Date = dates, data.frame(cbind(Actual = data()[[1]], Fitted = fitted(fit())))), Residual = Actual - Fitted)}) 
  
  #Advertsing plot
  #Advertising_data <- reactive({Select})
  Advertising <- reactive({melt(cbind(Date = dates, data()), id.vars = "Date", measure.vars = c("Columns", "Posters", "Panels"))})
  
  output$AVMPlot <- renderPlotly({ggplot(AVM(), aes(Date)) + 
      geom_line(aes(y = Actual, colour = "Actual")) + 
      geom_line(aes(y = Fitted, colour = "Fitted")) +
      geom_line(aes(y = Residual, colour = "Resid")) +
      theme(legend.background = element_rect(), legend.position = c(0.1, 0.9), legend.key = element_rect(fill = "transparent", colour = "transparent"))
  })
  
  output$MediaPlot <- renderPlotly({ggplot(Advertising(), aes(x = Date, y = value, fill = variable)) + 
      geom_bar(position="stack", stat="identity", width=1) + scale_fill_manual(values =  c("#468C3C", "#078AB9", "#343B46","#32132B", "#B7B5BA", "#AE1B1F","#FFAA00")) +
      theme(legend.background = element_rect(), legend.position = c(0.8, 0.8), legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.text = element_text(size = 10))
  })
  
  output$Variables <- renderText({paste(names(fit()$coefficients), sep = "", collapse=", \n")})
  
}

shinyApp(ui = ui, server = server)

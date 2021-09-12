warning = FALSE
results="hide"
message=FALSE

if (!require("highcharter")) install.packages('highcharter')
if (!require("shiny")) install.packages('shiny')
if (!require("dplyr")) install.packages('dplyr')
if (!require("ggplot2")) install.packages('ggplot2')
if (!require("plotly")) install.packages('plotly')
if (!require("leaflet")) install.packages('leaflet')
if (!require("DT")) install.packages('DT')
if (!require("jpeg")) install.packages('jpeg')
if (!require("datasets")) install.packages('datasets')
if (!require("forecast")) install.packages('forecast')
if (!require("lubridate")) install.packages('lubridate')
if (!require("thematic")) install.packages('thematic')
if (!require("bslib")) install.packages('bslib')
if (!require("shinyWidgets")) install.packages('shinyWidgets')

library(highcharter)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(jpeg)
library(datasets)
library(forecast)
library(lubridate)
library(thematic)
library(bslib)
library(shinyWidgets)



# Load the Data from the Markdown into Shiny App
load("Final_Data_Group_03.RData")
#Due to possible interference from each task save the data into different variables for each task
#a
a <- final_dataset

#b
b <- final_dataset

#In task c there is a seperation into cities so make a vector with the neccessary cities
#c
c <- final_dataset
city <- c("AUGSBURG", "INGOLSTADT","REGENSBURG", "WUERZBURG", "BAMBERG", 
          "BAYREUTH", "ASCHAFFENBURG", "ERLANGEN", "ROSENHEIM", "LANDSHUT")

#Shiny Ui
ui <- fluidPage(
#The Shiny App is divided into tabs and a navigation bar. The Logo is inside a folder called WWW because of some internal directions from the R application. 
#The overall theme and logo is set here
  titlePanel(div(img(height="100px",width="100px",src = "Logo.jpg"))),
  setBackgroundColor(
    color = "#406596",
    shinydashboard = FALSE
  ),
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  tags$head(tags$style(HTML("#table1 {background-color:white}"))),
  navbarPage(title = "Shiny App",
  #a
             tabPanel("Defective Parts per Location",
                      sidebarPanel(# City Dropdown Menu
                        selectizeInput("cityInput", 
                                       label = "Select City", 
                                       choices = unique(a$Stadt), 
                                       selected = unique(a$Stadt)[1]),
                        
                        # Year Dropdown
                        selectInput("yearInput", 
                                    label = "Select Year",
                                    choices = unique(a$Fehlerhaft_Jahr),
                                    selected = unique(a$Fehlerhaft_Jahr)[1]),
                        
                        # Einzelpart Dropdown
                        selectInput("einzelpartInput", 
                                    label = "Select Einzelpart",
                                    choices = unique(a$Einzelpart),
                                    selected = unique(a$Einzelpart)[1])
                        ),
                      mainPanel(plotOutput("plot1"))
             ),
  #b
             tabPanel("Forecast",
                      sidebarPanel(
                        # Einzelpart Dropdown
                        selectInput("partb", 
                                    label = "Select Einzelpart",
                                    choices = unique(b$Einzelpart),
                                    selected = unique(b$Einzelpart)[1]),
                        selectizeInput("cityb", 
                                       label = "Select City", 
                                       choices = unique(b$Stadt), 
                                       selected = unique(b$Stadt)[1]),
                        tags$div(class="header", checked=NA,tags$p("The first plot shows the total Outage. The second shows the Outage per City")),
                      ),
                      mainPanel(plotOutput("plotb1"),
                                plotOutput("plotb2")),
                      
             ),
  #c
             tabPanel("Interactive Map",
                      sidebarPanel(selectInput("partc", 
                                               label = "Select Einzelpart",
                                               choices = unique(c$Einzelpart),
                                               selected = unique(c$Einzelpart)[1]),
                                   tags$div(class="header", checked=NA,
                                            tags$p("Parts needed 1st quarter 2017")
                                     
                                   )
                                   ),
                      mainPanel(#dataTableOutput("tablec"),
                                leafletOutput("map"), p()
                                )
             ),
  #d
             tabPanel("Data Table",
                      mainPanel(dataTableOutput("table1"))
             )
  ))

#Shiny Server
server <- function(input, output, session){
  #reactivea filters the inputs in ui-a in the data table and output$plot1 renders the filtered data in a bar plot
  #a
  # Output City
  reactivea <- reactive({
    if(input$cityInput == "" & input$yearInput == "" & input$einzelpartInput == ""){
      return(a)
    }
    if(input$cityInput != ""){
      a <- a %>%
        filter(Stadt == input$cityInput)
    }
    if(input$yearInput != ""){
      a <- a %>%
        filter(Fehlerhaft_Jahr == input$yearInput)
    }
    if(input$einzelpartInput != ""){
      a <- a %>%
        filter(Einzelpart == input$einzelpartInput)
    }
    a <- aggregate(Anzahl_Fehler ~ Fehlerhaft_Monat, data = a, sum) 
    return(a)
  })
  
  output$plot1<-renderPlot({
    ggplot(reactivea(),
           aes(x = Fehlerhaft_Monat, y= Anzahl_Fehler))+
      geom_bar(stat = "identity", width = 0.8)+
      scale_x_continuous(breaks=c(1, 2, 3, 4,5,6,7,8,9,10,11,12),labels = c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sept", "Okt", "Nov", "Dez"))+
      labs(title="Number of failures for each part for each month",x="Month", y= "Count of Defective parts")
  })
  #reactiveb1 filters the parts and uses the forecast. The time periods are months starting from Jan2014 = 1 and March2017 = 39
  #b
  reactiveb1 <- reactive({
    dfforecast <- data.frame(b) %>%
      filter(Einzelpart==input$partb) %>%
      select(c("Fehlerhaft_Jahr", "Fehlerhaft_Monat", "Anzahl_Fehler"))
    dfforecast <- aggregate(Anzahl_Fehler ~ 
                              Fehlerhaft_Jahr + Fehlerhaft_Monat, 
                            data = dfforecast, sum) 
    # 1.) Exponential Smoothing Forecasting
    fit <- ets(dfforecast$Anzahl_Fehler)
    fcast_EG = forecast(fit, 12) # Forecast will be 12 time periods long
    return(fit)
  })
  #reactiveb2 is equal to reactiveb1 with the exception that it is also filtered by city to show the forecast not only for the part but for the city
  reactiveb2 <- reactive({
    dfforecast2 <- data.frame(b) %>%
      filter(Einzelpart==input$partb) %>%
      filter(Stadt==input$cityb) %>%
      select(c("Fehlerhaft_Jahr", "Fehlerhaft_Monat", "Anzahl_Fehler"))
    dfforecast2 <- aggregate(Anzahl_Fehler ~ 
                              Fehlerhaft_Jahr + Fehlerhaft_Monat, 
                            data = dfforecast2, sum) 
    # 1.) Exponential Smoothing Forecasting
    fit2 <- ets(dfforecast2$Anzahl_Fehler)
    fcast_EG2 = forecast(fit2, 12) # Forecast will be 12 time periods long
    return(fit2)
  })
  #Plot the forecasts
  output$plotb1 <- renderPlot({
    plot(reactiveb1())
  })
  output$plotb2 <- renderPlot({
    plot(reactiveb2())
  })
  #reactivec uses the forecast and the map location from the data table to make a data frame for the interactive map
  #c
  reactivec <- reactive({
    
    names_map_data <- c("Stadt","Anzahl")
    map_data <- data.frame(Stadt=character(), Anzahl=integer())
    
    map_locations <- data.frame(Stadt=character(), Laengengrad=integer(), Breitengrad=integer())
    names_map_location <- c("Stadt", "Laengengrad", "Breitengrad")
    
    #using the forecast for each part input for each city
    for (i in 1:10) {
      dfforecast <- data.frame(final_dataset) %>%
        filter(Einzelpart==input$partc) %>%
        filter(Stadt==city[i]) %>%
        select(c("Fehlerhaft_Jahr", "Fehlerhaft_Monat", "Anzahl_Fehler"))
      dfforecast <- aggregate(Anzahl_Fehler ~ Fehlerhaft_Jahr + Fehlerhaft_Monat, 
                              data = dfforecast, sum) 
      
      ds_ts <- ts(dfforecast$Anzahl_Fehler, frequency = 12,  start=c(2014,1), end=c(2017,3))
      ds_ts_x <- decompose(ds_ts, "additive") 
      temp_data <- data.frame(ds_ts_x$x)
      rowx <- data.frame(city[i],sum(temp_data[37,], temp_data[38,], temp_data[39,]))
      names(rowx) <- names_map_data
      map_data <- rbind(map_data,rowx)
      names(map_data) <- names_map_data
    }
    
    #getting the map locations lat und lng for the interactive map
    for (j in 1:10){
      c <- final_dataset %>%
        filter(Stadt==city[j])
      rowy <- data.frame(city[j],c[1,6],c[1,7])
      names(rowy) <- names_map_location 
      map_locations <- rbind(map_locations,rowy)
      names(map_locations) <- names_map_location
    }
    map_full <- left_join(map_data,map_locations)
    return(map_full)
  })
  #test data table if the function works. Can be shwon in the Shiny app but isnt asked for in the task
  output$tablec = DT::renderDataTable({
    reactivec()
  },filter ="top")
  #render the Map with the data from reactivec
  output$map <- renderLeaflet({
    leaflet(reactivec()) %>% 
      addTiles() %>%
      addMarkers(lng = ~Laengengrad, lat = ~Breitengrad, label = paste(reactivec()$Stadt, reactivec()$Anzahl) )
  })
  
  #Put out the final data set 
  #d
  output$table1 = DT::renderDataTable({
    final_dataset
  },filter ="top")

  
} 
#Execute Shiny App
shinyApp(ui, server)


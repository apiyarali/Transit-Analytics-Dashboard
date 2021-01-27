#Transit Analytics

################################################################################
# 
# Creator: Ali PiyarAli
# 
# Link to Published App: https://alipiyarali.shinyapps.io/TransitAnalytics/
# 
################################################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# install.packages("testthat")
# install.packages("pkgload")

# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap")

library(ggmap)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(pryr)
library(imputeTS)

#source("jaxmat.R")   #for displaying mathematics

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    
    .sidebar-menu li a{
      font-size: 20px;
      font-weight: bold;
      font-color: #ffffff;
      margin: 30px;
    }
    
    #sidebar{
      background-color: #dec4de;  
    }
    
    .skin-red .main-header .navebar {
      background-color: #da251d;
    }
    
    .skin-red .main-header .logo {
      background-color: #da251d;
    }
    
    .skin-red .main-sidebar{
      background-color: #222d32;
    }
    
    h3{
      text-align:center;
    }
    
    h2{
      text-align:center;
    }
    
    h4{
      text-align:center;
      font-style: italic;
    }
    
    h5{
      margin-left: 20px;
      font-style: italic;
    }
    
    #transit{
      display: block;
      margin-left: auto;
      margin-right: auto;
    }
    
    #note{
      font-weight: bold;
      font-size: 16px;
    }
  ')
))


#The user interface
header <- dashboardHeader(title = "Transit Analytics",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 350,
  sidebarMenu(id="menu",
              h5("Select from options below:"),
              menuItem("Stops with Most Trips", tabName = "heat"),
              menuItem("Stops Size - Trip Frequency", tabName = "stop"),
              menuItem("Trips per hour by Line", tabName = "trips",
                       menuSubItem("Bar Chart", tabName = "bar"),
                       menuSubItem("Line Chart", tabName = "line")
                       ),
              menuItem("Network Map", tabName = "map"),
              hr(),
              h5(id="note","PLESE BE PATIENT, FOR THE MAP PLOT"),
              h5(id="note","TO GENERATE, READING OVER 500,000"),
              h5(id="note","DATA POINTS."),
              hr(),
              h5("Info:"),
              h5("Toronto Transit Commission data analytics"),
              h5("Data Source is", a(href = "https://transitfeeds.com/p/ttc/33", "GTFS.")),
              h5("Data feed is from", a(href = "https://transitfeeds.com/p/ttc/33/latest","11 October 2020 - 21 November 2020.")),
              h5("Data is publicly avaialable"),
              hr()
  )   
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=12,
           box(id="map",
             width = 800,
             h2("Toronto Transit Commission"),
             h3(uiOutput("title")),
             h4(uiOutput("titleExtra")),
             br(),
             plotOutput("transit", width = "800px", height = "700px")  
           )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "red") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  
  # fetch the map
  register_google(key = "YOUR GOOGLE API KEY")
  getOption("ggmap")
  tor <- c(-79.380910,43.722477)
  tor_map <- get_map(location = tor, maptype = "roadmap", zoom = 11)
  
  #Initialization
  
  # read GTFS data
  
  # For Map
  shapes <- read.csv("shapes.txt")
  
  # For Heat Map
  stops <- read.csv("stops.txt")
  stop_times <- read.csv("stop_times.txt") %>% sample_n(100000) # use a data sample of 100,000 instead of the whole dataset
  trips <- read.csv("trips.txt")
  calendar <- read.csv("calendar.txt")
  
  # For Trip Frequency
  stop_times_trip <- read.csv("stop_times.txt")
  
  # For Trip per hour by Line
  routes <- read_csv("routes.txt")
  stop_times_hour <- read_csv("stop_times.txt", col_types= cols(arrival_time = col_character(), departure_time = col_character()))
  
  
  #Functions that respond to events in the input
  
  observeEvent(input$menu,{
    
    # HEATMAP OF STOPS WITH MOST TRIPS
    if(input$menu == "heat"){
      
      #join all stop times with stop info and trips
      stops_freq = 
        inner_join(stop_times,stops,by=c("stop_id")) %>% 
        inner_join(trips,by=c("trip_id")) %>%
        inner_join(calendar,by=c("service_id")) %>%
        select(stop_id,stop_name,stop_lat,stop_lon) # %>%
      
      print (stops_freq)
      
      output$title <- renderUI(paste("Heat Map of Stops with Most Trips"))
      output$titleExtra <- renderUI(paste("(sample of 100,000 data points)"))
      
      # plot the map with a density/heatmap trips/stops
      output$transit <- renderPlot(
        ggmap(tor_map, extent = "device") +
          stat_density2d(data = stops_freq, aes(x = stop_lon, y = stop_lat, alpha=..level..), # variable transparency according to number of trips
                         size = .5, color='black', bins=5, geom = "polygon", fill='blue') # use 5 bins(transparency levels) to represent different densities
      ) 
      
    }
    
    # PLOT STOPS WITH SIZE BASED ON TRIP FREQUENCY
    if(input$menu == "stop"){
      output$title <- renderUI(paste("Stops with Size based on Trip Frequency"))
      output$titleExtra <- renderUI(paste(""))

      #join all data and count number of services grouped by stop
      stops_freq_trip =
        inner_join(stop_times_trip,stops,by=c("stop_id")) %>%
        inner_join(trips,by=c("trip_id")) %>%
        inner_join(calendar,by=c("service_id")) %>%
        select(stop_id,stop_name,stop_lat,stop_lon) %>%
        group_by(stop_id,stop_name,stop_lat,stop_lon) %>%
        summarize(count=n()) %>%
        filter(count>=150) # filter out least used stops

      # plot the map with stop data
      output$transit <- renderPlot(ggmap(tor_map, extent = "device") +
                                     geom_point(data = stops_freq_trip,aes(x=stop_lon, y=stop_lat, size=count, fill=count), shape=21, alpha=0.8, colour = "blue")+ #plot stops with blue color
                                     scale_size_continuous(range = c(0, 9), guide = FALSE) + # size proportional to number of trips
                                     scale_fill_distiller() # circle fill proportional to number of trips
                                   )
    }

    # Getting the number of trips per hour for each line

    # Joining the data frames
    # From routs.txt: route_id -> trips.txt: trip_id -> stip_times.txt
    stop_times_hour <- stop_times_hour %>%
      left_join(trips) %>%
      left_join(routes) %>%
      select(route_id, route_short_name, trip_id, stop_id, service_id, arrival_time, departure_time, direction_id, shape_id, stop_sequence)

    # Service id with most trips
    trips %>%
      group_by(service_id) %>%
      count(service_id) %>%
      arrange(desc(n))

    # Biggest service
    bigger_service <- trips %>%
      group_by(service_id) %>%
      count(service_id) %>%
      arrange(desc(n)) %>%
      head(1)

    # Filtering by service_id, stop_sequence and direction_id
    stop_times_hour <- stop_times_hour %>%
      filter(
        stop_sequence == 1 &
          direction_id == 0 &
          service_id == bigger_service$service_id)

    # Transforming characters into numbers
    stop_times_hour <- stop_times_hour %>%
      mutate(
        arrival_time = ifelse(
          as.integer(substr(arrival_time, 1, 2)) < 24,
          as.integer(substr(arrival_time, 1, 2)),
          as.integer(substr(arrival_time, 1, 2)) - 24),
        departure_time = ifelse(
          as.integer(substr(departure_time, 1, 2)) < 24,
          as.integer(substr(departure_time, 1, 2)),
          as.integer(substr(departure_time, 1, 2)) -24)
      )

    # what is the number of trips per hour for each of these lines

    output_data <- stop_times_hour %>%
      group_by_at(vars(route_id, route_short_name, arrival_time)) %>%
      count(arrival_time)

    # Above data format changes
    output_data <- stop_times_hour %>%
      group_by_at(vars(route_id, route_short_name, arrival_time)) %>%
      count(arrival_time) %>%
      mutate(time_window = paste(arrival_time, '00', sep = ':')) %>%
      select(route_id, route_short_name, arrival_time, time_window, n)

    # charts with the number of trips per hour
    line <- output_data %>%
      filter(route_id == '61268')

    line$time_window <- factor(line$time_window, levels = unique(line$time_window))

    # bar chart for number of trips per hour
    if(input$menu == "bar"){
      g_bar <- ggplot(data = line,
                      aes(x = time_window, y = n)) +
        geom_bar(stat = 'identity', fill = 'dodgerblue2', color = 'dodgerblue2') +
        geom_text(aes(label = n),
                  vjust = -0.3,
                  color = "black",
                  size = 3) +
        scale_fill_brewer(palette="Dark2")+
        labs(title = paste('Trips by hour for route', line$route_short_name, sep = ' '),
             x = "Time window",
             y = '') +
        theme(panel.grid = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(colour = "grey"),
              axis.line.y = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.text.y = element_blank(),
              axis.ticks.x = element_line(colour = "grey"),
              axis.ticks.y = element_blank(),
              plot.title = element_text(hjust = 0.5)
        )

      output$title <- renderUI(paste("Number of Trips per hour for Service ID with Most Trips"))
      output$titleExtra <- renderUI(paste(""))

      output$transit <- renderPlot(g_bar)

    }

    # bar chart for number of trips per hour
    if(input$menu == "line"){
      g_line <- ggplot(data = line,
                       aes(x = time_window, y = n, group = 1)) +
        geom_line(color = 'steelblue') +
        geom_point(color = 'steelblue') +
        geom_text(aes(label = n),
                  vjust = -0.8,
                  color = "black",
                  size = 3) +
        scale_fill_brewer(palette="Dark2")+
        labs(title = paste('Trips by hour for route', line$route_short_name, sep = ' '),
             x = "Time window",
             y = '') +
        theme(panel.grid = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(colour = "grey"),
              axis.line.y = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.text.y = element_blank(),
              axis.ticks.x = element_line(colour = "grey"),
              axis.ticks.y = element_blank(),
              plot.title = element_text(hjust = 0.5)
        )

      output$title <- renderUI(paste("Number of Trips per hour for Service ID with Most Trips"))
      output$titleExtra <- renderUI(paste(""))

      output$transit <- renderPlot(g_line)

    }


    # PLOT THE TRANSPORT NETWORK
    if(input$menu == "map"){
      output$title <- renderUI(paste("Transport Network Map"))
      output$titleExtra <- renderUI(paste(""))

      output$transit <- renderPlot(ggmap(tor_map, extent = "device") +
                                     geom_path(data = shapes, aes(shape_pt_lon, shape_pt_lat, group = shape_id), size = 1, alpha = .5, color='blue') +
                                     coord_equal() + theme_map()
                                   )
    }
    
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)
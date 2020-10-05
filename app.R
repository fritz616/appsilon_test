library(shiny)
library(shiny.semantic)
library(feather)
library(geosphere)
library(dplyr)
library(leaflet)


#Load data and assign variables
path <- "my_data.feather"
dt<-read_feather(path)
shipType<-unique(dt$ship_type)
shipName<-unique(dt$SHIPNAME)


#UI module for dropdown list
dropdownUI<-function(id){
  ns <- NS(id)
  tagList(h1("Vessel Type:"),
          dropdown_input(ns("vesselType"), shipType, value = NULL),
          h1("Vessel Name:"),
          dropdown_input(ns("vesselName"), shipName, value = NULL),
          h1("Summary:"),

  )
}

#return value with vessel name
setup<-function(input,output,session){
  vesselName<-reactive({input$vesselName})
  return(vesselName)
}

#Server module for dropdown list
counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      setShipType<-reactive({
        x<-input$vesselType
      })
      shipNameType<-reactive({
        req(input$vesselType)
        shipSetType<-subset(dt, ship_type==setShipType())
        y<-c(unique(shipSetType$SHIPNAME))
      })
      
      observeEvent(input$vesselType, {
        update_dropdown_input(session, "vesselName", shipNameType())
      })
      
      bins<-reactive({ input$vesselName })
    }
  )
}


# Define UI for application
ui <- semanticPage(
  titlePanel("Hello Appsilon!"),
  sidebar_layout(
    sidebar_panel(
      dropdownUI("id"),
      textOutput("test"),
      width = 2
    ),
    main_panel(
      leafletOutput("map"),
      width = 3
    ),
    mirrored = FALSE
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  counterServer("id")
  #Retrieve vessel name 
  bins <- callModule(setup,"id")
  #Make df with ships of interest
  df<-reactive({
    req(bins())
    shipSet<-subset(dt, SHIPNAME==bins())
    name<-c(shipSet$SHIPNAME)
    LAT<-c(shipSet$LAT)
    LON<-c(shipSet$LON)
    dtime<-c(shipSet$DATETIME)
    df<-data.frame(name,LAT,LON,dtime)
  })
  #Find the longest distance
  timeDistance<-reactive({
    df<-df()
    distance<-c()
    tableLength<-1:length(df$LAT)
    
    for (i in tableLength){
      metry<-distm(c(df$LON[i], df$LAT[i]), c(df$LON[i+1], df$LAT[i+1]), fun = distHaversine)
      distance<-append(distance, metry)
    }
    distance<-distance[1:length(distance)-1]
    
    
    timeDistance=c()
    for (i in tableLength){
      czas<-difftime(df$dtime[i], df$dtime[i+1], units = "secs")
      timeDistance<-append(timeDistance, abs(czas[[1]]))
    }
    timeDistance<-timeDistance[1:length(timeDistance)-1]
    
    constitutivePoints<-300
    result<-data.frame("distance"=distance, "time"=timeDistance)
    result<-subset(result, result$time<=constitutivePoints)
    result 
    })
  
  #Find index of the longest distance row
  maxDist<-reactive({  
    
    result<-timeDistance()
    df<-clearTable()
    timeDiff=c()
    currTime<-format(Sys.time(), "%F %X")
    maxDist<-which(result$distance %in% max(result$distance))
    if (length(maxDist)>1){
      for (i in maxDist){
        difference<-difftime(df$dtime[i], currTime, units = "secs")
        timeDiff<-append(timeDiff, abs(difference[[1]]))
      }
      maxDist<-maxDist[which(timeDiff %in% min(timeDiff))]
    }
    maxDist
  })
  
  #remove inconsecutive rows from the original df 
  clearTable<-reactive({
    req(bins())
    shipSet<-df()
    dtime<-c(shipSet$dtime)
    outlier<-c()
    loop<-1:length(dtime)
    for (i in loop){
      czas<-difftime(dtime[i], dtime[i+1], units = "secs")
      if (abs(czas[[1]])<=300 || is.na(czas[[1]])){
        outlier <- append(outlier, FALSE)}
      else {
          outlier <- append(outlier, TRUE)
      }
    }
    shipSet<-cbind(shipSet, outlier)
    shipSet<-subset(shipSet, outlier==FALSE)
    
  })
  
  #output note
  output$test<-renderText({
    result<-timeDistance()
    xD<-clearTable()
    x<-result[maxDist(),]
    y<-paste("Ship",xD$name[maxDist()],"has traveled", round(x$distance), "meters. Starting at", xD$LON[maxDist()],"LON and",
             xD$LAT[maxDist()], "LAT and finished", xD$LON[maxDist()+1], "LON and", xD$LAT[maxDist()+1], "LAT.")
  })  
  
  

  #map
  
  output$map<-renderLeaflet({
    df<-clearTable()
    max<-maxDist()
    max1<-maxDist()+1
    df<-df[max:max1,]
    beginning<-paste("Beginning:",df$LAT[1], "LAT and", df$LON[1],"LON")
    end<-paste("End:",df$LAT[2], "LAT and", df$LON[2],"LON")
    m <- leaflet(data = df) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng= df$LON[1],lat = df$LAT[1], zoom = 12)%>%
      addMarkers(~LON[1], ~LAT[1], popup=beginning) %>%
      addMarkers(~LON[2], ~LAT[2], popup=end) %>%
      addPolylines(lng = ~LON, lat = ~LAT)
        
    m
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)



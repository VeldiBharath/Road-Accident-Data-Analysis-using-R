#Initialize Libs
library(sf)
library(ggplot2)
library(shiny)
library(dplyr)

shp_file <- r"(C:\Users\akhil\OneDrive - dsatm.edu.in\Documents\Akhil\R\2003_BBMP.kml)"

{
    acc_data <- read.csv(r"(C:\Users\akhil\OneDrive - dsatm.edu.in\Documents\Akhil\R\AccidentData.csv)")
    colnames(acc_data) <- c("device","lat","long","location","alarmType","speed","time")
    acc_data <- distinct(acc_data,device,lat,long,location,alarmType,speed,time)
    
    overall <- as.data.frame(table(acc_data$location))
    class(overall)
    colnames(overall) <- c("Name","accidents")
}

{
nc <- st_read(shp_file)

t_overall <- merge(nc,overall, by = "Name" , all.x = TRUE)
t_overall$accidents[is.na(t_overall$accidents)] <- 1

for (i in 1:nrow(t_overall)){
  if(t_overall[i,]$accidents == 1){
   t_overall[i,]$accidents <- r <- sample(1000:16000,1) 
  }
}

plot <- ggplot() +
  geom_sf(data= t_overall, aes(fill = accidents)) +
  scale_fill_gradient(low = "blue", high = "red")+
  labs(fill = "No of Accidents") +
  xlab("Latitude")+
  ylab("Longitude")+
  theme_minimal()

print(plot)

}

{
  library(shiny)
  library(leaflet)

  
  data <- data.frame(
    Place = overall$Name
  )
  
  ui <- fluidPage(
    titlePanel("Accidents in Bangalore"), 
    mainPanel(
      plotOutput("plot"),
      selectInput("Place", "Select Ward:", choices = unique(data$Place)),  
      actionButton("Submit","Submit"),
      br(),
      textOutput("result"),
      br(),
      leafletOutput("mymap"),
      br(),
      br(),
      br()
    )
  )
  

  server <- function(input, output) {

    output$plot <- renderPlot({
      print(plot)
    })
    
    
    observeEvent(input$Submit, {
      
      print("clicked..")
      
      result_message <- paste("Place selected: ",input$Place)
      Place <- input$Place
      
      output$result <- renderText({
        result_message
      })
      
      output$mymap <- renderLeaflet({
        leaflet(data= subset(acc_data, location == Place)) %>%
          addTiles() %>%
          addMarkers(lng= ~long, lat = ~lat, clusterOptions = markerClusterOptions())
        
      })
      
    })
    
    
  }
  
  shinyApp(ui = ui, server = server)
}


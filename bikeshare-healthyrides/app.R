library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(tidyverse)

#Loading Data
ridesummary<- fread("ridesummary.csv")
rawdata <- fread("bikes.csv")
ridesummary$Month <- factor(ridesummary$Month, levels = month.abb) 
tripbmonth <- fread("tripbymonth .csv")
stations <- fread("most_used_st.csv")

# UI for the bikeshare application 
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  
  # Application title -----------------------------------------------
  titlePanel("Healthy Ride - Pittsburgh Bike Share"),
  
  
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      
      # Select variable for y-axis ----------------------------------
      img(src = "hride.jpg", height = 140, width = 250),
      
      selectInput(inputId = "z",
                  label = "Type of User",
                  choices = c("total","subscriber","customer"),
                  selected = "total"),
      
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show Summary Data",
                    value = TRUE),
      
      div((HTML("<strong>The following datasets were used for producing these visualizations and can be downloaded
               for your own use</strong>")),style = "color:green"),
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("ridesummary", "rawdata","stationdata")),
      
      
      #Adding a Download Button
      downloadButton("downloadData", "Download"),
      
      
    
      #Select which months the user wants to plots
      checkboxGroupInput(inputId = "selected_type",
                         label= "Select Months",
                         choices = c("Jan","Feb","Mar","Apr","May","Jun","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         selected = "Jan"
        
      )
      
      
    
),


    
  
    
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      
  
      # Output: Histogram ----
      plotOutput(outputId = "tripboxplot"),
      
      #barplot of stations
      plotOutput(outputId = "stbarplot"),
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "datasummary")  
    )
  )   
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  months_subset <- reactive({
    req(input$selected_type)
    filter(tripbmonth,Month %in% input$selected_type)
    
    
  })
  
  station_subset <- reactive({
    req(input$selected_type)
    filter(stations,Month %in% input$selected_type)
    
    
  })
  
  
  #Create Boxplot
  output$tripboxplot <- renderPlot({
    ggplot(data = months_subset(),aes(x = Month,y = Tripduration,color=Usertype))+
    xlab("Months")+ylab("Trip Duration in Minutes")+
    geom_boxplot()+coord_flip()+ggtitle("Trip Duration Between Customers and Subscribers")+theme(title = element_text(face = "bold"))+
    scale_color_manual(values=c("#8fad93", "#009cb5"))
    
    
  })
  
  
  #Create Barchart
  output$stbarplot <- renderPlot({
    ggplot(data = station_subset(),aes(x=`To station name`,y = n))+
    geom_bar(stat="identity",fill="#ae7c5e")+xlab("Destination Station")+ylab("Number of rides")+
    coord_flip()+
    ggtitle("Busiest Destination Stations by Month")+theme(title = element_text(face = "bold"))
    
    
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
   ggplot(data = ridesummary, aes_string(x = "Month", y = input$z,group=1)) +
    geom_point(size=1.8)+geom_line(color="blue",size=0.6)+ylab("Number of Rides")+
    ggtitle("Healthy-Ride Rental Usage in 2020")+theme(title = element_text(face = "bold"))
   
      
    
  })
  
 
  
  
  
  
  
  
  # Print data table if checked -------------------------------------
  output$datasummary <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = rawdata[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "ridesummary" = ridesummary,
           "rawdata" = rawdata,
           "stationdata"=stations)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset,".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)






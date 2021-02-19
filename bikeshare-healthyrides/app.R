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


# UI for the bikeshare application 
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("Healthy Ride"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      
      # Select variable for y-axis ----------------------------------

  
      selectInput(inputId = "z",
                  label = "Type of User",
                  choices = c("total","subscriber","customer"),
                  selected = "total"),
      
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show Summary Data",
                    value = TRUE),
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("ridesummary", "rawdata")),
      
      
      #Adding a Download Button
      downloadButton("downloadData", "Download"),
      
      
      # Adding a Slider
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 10,
                  value = 5)
      
    
),
    
  
    
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      
      #boxplot
#      plotOutput(outputId = "boxplot"),

      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "datasummary")  
    )
  )   
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  

  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
   ggplot(data = ridesummary, aes_string(x = "Month", y = input$z,group=1)) +
    geom_point(size=1.5)+geom_line(color="red",size=0.8)+ylab("Number of Rides")+
    ggtitle("Healthy-Ride Rental Usage in 2020")+theme(title = element_text(face = "bold"))
  
      
    
  })
  
  output$distPlot <- renderPlot({
    
    x    <- rawdata$Tripduration
    bins <- seq(min(x), max(x), length.out = input$bins)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Trip Durations in minutes",
         main = "Histogram of waiting times")
  
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
           "rawdata" = rawdata)
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






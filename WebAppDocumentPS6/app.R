library(shiny)
library(tidyverse)
getwd()

UAH <- read_delim("UAH-lower-troposphere-long.csv.bz2")

view(UAH)

ui <- fluidPage(
  tabsetPanel(type = "tab",
              tabPanel("Opening Page",
                       h1("UAH Lower Troposphere Data"),
                       p("The University of Alabama Huntsville has been collecting data on the
                          temperature of the", strong("lower troposphere"), "depending on month and region
                          since 1978."), 
                       p("This data contains", nrow(UAH), "observations with 4 variables." ),
                       p("Below is a small random sample of the data."),
                       dataTableOutput("small")
                       ),
              tabPanel("Plot Panel",
                sidebarLayout(
                  sidebarPanel( 
                   
                    fluidRow(h1("Select Month Below"),
                             p("Lower Troposphere temperatures by year,
                               which can be filtered by the month"),
                      uiOutput("selectinput"),
                      radioButtons("color", "choose color", 
                                   choices = c("skyblue", "lawngreen", "orangered",
                                                        "purple", "gold"))
                    )
                    ),
                mainPanel(
                  plotOutput("plot"),
                  textOutput("monthchange")
                )
                )
              ),
              tabPanel("Table Panel",
              sidebarLayout(
                sidebarPanel(
                  fluidRow(uiOutput("checkboxregion"))
                  ),
                mainPanel(
                  textOutput("range_text"),
                  tableOutput("table")
                )
              )
        ),
  
 )
)
 

server <- function(input, output) {
  output$small <- renderDataTable({
      UAH %>% 
        sample_n(10)
    })
  
output$selectinput <- renderUI({
  selectInput("select", "Choose:",
              choices = unique(UAH$month))
})
  
samplemon <- reactive({
  UAH %>% 
    filter(month %in% input$select)
})

  output$plot <- renderPlot({
  samplemon() %>%
      ggplot(aes(year, temp))+
      geom_point(color = input$color) +
      labs(x = "Year",
           y = "Temperature in Celsius",
           title = "Temperature of Lower Troposphere by Year")
  })
  
  output$monthchange <- renderText({
    paste("This scatterplot shows the Lower Troposphere temperatures 
          over the years for month", input$select[1])
  })
  
  output$table <- renderTable({
    
    sampletwo()

  })
  output$checkboxregion <- renderUI({
    checkboxGroupInput("region", "Choose Regions",
                       choices = unique(UAH$region))
    
  })
  
  sampletwo <- reactive({
    UAH %>% 
      filter(region %in% input$region) 
  })

range_text <- reactive({
  selected <- input$region
  filtered <- UAH %>% 
    filter(region %in% input$region)
  data_range <- range(filtered$temp)
  paste0("Data Range:", data_range[1], "to", data_range[2])
})
  
  
  output$range_text <- renderText({
    range_text()

  })
}



shinyApp(ui = ui, server = server)


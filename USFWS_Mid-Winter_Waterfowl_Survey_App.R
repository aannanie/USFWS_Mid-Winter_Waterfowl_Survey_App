library(dplyr)
library(ggplot2)
library(vroom)
library(shiny)


PF <- read.csv("Pacific_Flyway.csv")

ui <- fluidPage(
  h1("USFWS Mid-Winter Waterfowl Survey Data App"),
  tags$head(tags$style('h1 {font-style: italic}')),
  p("Please select a flyway and year for which you would like to see the data 
    dispalyed"),
  column(8, radioButtons("Flyway", "Flyway", c("Pacific Flyway" = "PF", 
                                               "Central Flyway" = "CF", 
                                               "Mississippi Flyway" = "MF", 
                                               "Atlantic Flyway" = "AF"),
                         inline = TRUE, selected = character(0))),
  fluidRow(
    column(8,
           # This allows the user to select from the list of products based
           # on the product codes.
           selectInput("Year", "Year",
                       choices = PF$Year,
                       width = "100%"
           )
    )),
  column(12, tableOutput("Year")),
  )

server <- function(input, output) {
  output$Year <- renderTable(PF[input$Year,])
}

shinyApp(ui = ui, server = server)


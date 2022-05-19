library(dplyr)
library(ggplot2)
library(shiny)
library(janitor)


PF <- read.csv("PF.csv")
CF <- read.csv("CF.csv")
MF <- read.csv("MF.csv")
AF <- read.csv("AF.csv")

ui <- fluidPage(
  h1("USFWS Mid-Winter Waterfowl Survey Data App"),
  tags$head(tags$style('h1 {font-style: italic}')),
  p("Please select a flyway and year for which you would like to see the data
    displayed"),
  column(8, radioButtons(inputId = "Flyway",
                         label = "Flyway",
                         choices = c("Pacific Flyway" = "PF",
                                     "Central Flyway" = "CF",
                                     "Mississippi Flyway" = "MF",
                                     "Atlantic Flyway" = "AF"),
                         inline = TRUE, selected = character(0))),
  fluidRow(
    column(8,
           selectInput(inputId = "year2get",
                       label = "Year",
                       choices = c(""), # start empty
                       selected = NULL,
                       width = "100%")
    )),
  fluidRow(
    # placeholder -- summary text verbatim on app
    tableOutput("spitOutTable")
  ),
  fluidRow(
    column(8,
           selectInput(inputId = "zone2get",
                       label = "Zone",
                       choices = c(""), # start empty
                       selected = NULL,
                       width = "100%")
     )),
  fluidRow(
    column(8,
           selectInput(inputId = "species2get",
                       label = "Species",
                       choices = c(""), # start empty
                       selected = NULL,
                       width = "100%")
    )),
)

server <- function(input, output, session) {
  flyway2use <- reactive({
    req(input$Flyway)
    fname <- paste0(input$Flyway,".csv")
    # read the csv and clean the names up
    dat <- read.csv(fname) %>% clean_names()
    # go populate the year dropdown
    updateSelectInput(session,"year2get",choices=unique(dat$i_year))
    # and return the data
    dat
  })
  
  # simple output of data table
  output$spitOutTable <- renderTable({
    # don't run until the flyway has been selected
    req(input$Flyway)
    # get the data from the reactive and then filter to get the year
    res <- flyway2use() %>% filter(i_year == input$year2get)
    # return a summary that will get printed
    colnames(res) <- c("Year", "State", "Flyway", "Zone", "American Black Duck",
                       "American Coot", "American Wigeon", "Blue-Winged & Cinnamon Teal 
    (not speciated)", "Brant", "Bufflehead", "Canada Goose (not speciated)", 
                       "Canvasback", "Eider (not speciated)", "Emperor Goose", "Gadwall", 
                       "Goldeneye (not speciated)", "Greater White-Fronted Goose", 
                       "Green-Winged Teal", "Harlequin Duck", "Light Geese (not differentiated by 
    color phase)", "Long-Tailed Duck", "Mallard", "Merganser (not speciated)", 
                       "Mexican-Like Duck", "Mottled Duck", "Northern Pintail", "Northern Shoveler",
                       "Redhead", "Ring-Necked Duck", "Ruddy Duck", "Sandhill Crane", "Scaup (not 
    speciated)", "Scoters (not speciated)", "Swan (not speciated)",
                       "Unidentified Ducks", "Whistling Duck (not speciated)", "Wood Duck", 
                       "Miscellaneous Ducks")
    res
  })
}

shinyApp(ui = ui, server = server)


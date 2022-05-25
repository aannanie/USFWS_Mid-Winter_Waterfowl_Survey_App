library(dplyr)
library(ggplot2)
library(shiny)
library(janitor)


PF <- read.csv("PF.csv")
CF <- read.csv("CF.csv")
MF <- read.csv("MF.csv")
AF <- read.csv("AF.csv")

ui <- fluidPage(
  h1("USFWS Mid-Winter Waterfowl Survey App"),
  tags$head(tags$style('h1 {font-style: italic}')),
  p("Please select a flyway and year for which you would like to see the data
    displayed"),
  
  fluidRow(
    column(12, 
           imageOutput("Image1", width = "800px", height = "600px")
    )),
  
  column(12, radioButtons(inputId = "Flyway",
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
    column(4, 
           downloadButton("downloadData", "Download")
    )),
  
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
  
  fluidRow(
    plotOutput("spitOutPlot")
  )
  
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
    
  zone2use <- reactive({
    req(input$Flyway)
    fname2 <- paste0(input$Flyway,".csv")
    # read the csv and clean the names up
    dat2 <- read.csv(fname2) %>% clean_names()
    updateSelectInput(session,"zone2get",
                              choices=unique(dat2$zone))
    dat2
  })
  
  species2use <- reactive({
    req(zone2use())
    dat3 <- zone2use() %>% filter(zone == input$zone2get)
    updateSelectInput(session,"species2get", 
                      choices= colnames(dat3[, 5:38]))
  })
  
  output$spitOutPlot <- renderPlot({
    req(species2use())
    res2 <- zone2use() %>% filter(zone == input$zone2get)
    ggplot(res2, aes(x = unlist(i_year), y= unlist(res2 %>% select(input$species2get)))) +
      geom_line() +
      xlab("Year") +
      ylab("Number of Birds Observed")
    
  })
  
  output$Image1 <- renderImage({
    list(src="MWS_AllFlyways_Map.jpg",
         width=800,
         height=600)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Flyway, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(zone2use(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

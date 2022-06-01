library(dplyr)
library(ggplot2)
library(shiny)
library(janitor)
library(shinyWidgets)
library(shinydashboard)

ui <- fluidPage(
  h1("USFWS Mid-Winter Waterfowl Survey App"),
  tags$head(tags$style('h1 {font-style: italic}')),
  p("The USFWS Mid-Winter Waterfowl Survey is a coordinated annual survey 
    conducted in collaboration with state and federal wildlife management 
    agencies from across the country. Established in 1935 primarily as a means 
    of informing the development of waterfowl hunting regulations, the surveys 
    are currently used to document winter distribution patterns, habitat use,
    and long term population trends. They also serve as an important 
    supplement to the annual Waterfowl Breeding Population and Habitat Surveys 
    by documenting species that are unaccounted for due to incompatibilities 
    with the timing and spatial extent of the breeding surveys. For more detailed
    information on the surveys and to access additional data not available
    through this app, visit the", a("Mid-Winter Waterfowl Survey Homepage", 
    href='https://migbirdapps.fws.gov/mbdc/databases/mwi/mwidb.asp?opt=mwidb')),
  
  fluidRow(
    column(12, 
           img(src = "MWS_AllFlyways_Map.jpg", width = "800px", 
               height = "600px", style="display: block; margin-left: auto; 
               margin-right: auto;")
    )),
  
  p("Please select a flyway and year for which you would like to see the data
    displayed"),
  
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
    column(width = 12,
           DT::dataTableOutput("trace_table"),style = "height:500px; 
           overflow-y: scroll;overflow-x: scroll; background-color:white" ,
    # placeholder -- summary text verbatim on app
    tableOutput("spitOutTable")
  )),
  
  fluidRow(
    column(4, 
           downloadButton("downloadData", "Download")
    )),
  
  p("Please select a zone and a species for which you would like to see a
    plot of all available data (Note: Not all zones are represented in the
    map above)"),
  
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
  ),
  
  mainPanel(
    setBackgroundImage(src = "Ducks3.JPG")
  )
)

server <- function(input, output, session) {
  flyway2use <- reactive({
    req(input$Flyway)
    fname <- paste0(input$Flyway,".csv")
    # read the csv and clean the names up
    dat <- read.csv(fname) %>% clean_names()
    # This sets the name of the column so it is consistent across different
    # computers (clean_names can generate different names depending on the 
    # software version of the computer, this standardizes it)
    names(dat)[1] <- "year"
    # go populate the year dropdown
    updateSelectInput(session,"year2get",choices=unique(dat$year))
    # and return the data
    dat
  })
  
  # simple output of data table
  output$spitOutTable <- renderTable({
    # don't run until the flyway has been selected
    req(input$Flyway)
    # get the data from the reactive and then filter to get the year
    res <- flyway2use() %>% filter(year == input$year2get)
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
    names(dat2)[4] <- "zone"
    names(dat2)[1] <- "year"
    updateSelectInput(session,"zone2get",
                              choices=unique(dat2$zone))
    dat2
  })
  
  species2use <- reactive({
    req(zone2use())
    dat3 <- zone2use() %>% filter(zone == input$zone2get)
    dat3 %>% clean_names()
    # Get rid of species where there are zeros so only species with observations
    # are selectable
    yesDucks <- colSums(dat3[,-c(1:4)],na.rm = TRUE) > 0
    allSpp <- names(dat3[,-c(1:4)])
    possibleSpp <- allSpp[yesDucks]
    updateSelectInput(session,"species2get",
                      choices = possibleSpp)
  })
  
  output$spitOutPlot <- renderPlot({
    req(species2use())
    res2 <- zone2use() %>% filter(zone == input$zone2get)
    ggplot(res2, aes(x = unlist(year), y= unlist(res2 %>% select(input$species2get)))) +
      geom_line() +
      xlab("Year") +
      ylab("Number of Birds Observed")
    
  })
  
  # Downloadable csv of selected dataset
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

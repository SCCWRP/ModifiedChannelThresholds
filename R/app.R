ModifiedChannelThresholds <- function(...) {
  ui <- fluidPage(
    titlePanel("[Draft] Biointegrity and Eutrophication Thresholds for Modified Channels and Other Classes of Streams"),
    "Last updated 2024-06-19",
    tabsetPanel(
      tabPanel("Background", backgroundUI("bg")),
      tabPanel("Eutrophication Response Models", responseModelsUI("rs")),
      tabPanel("Threshold Query and Synthesis", identificationUI("id")),
      tabPanel("Resources", resourcesUI("re"))
    )
  )
    
  
  server <- function(input, output, session) {
    backgroundServer("bg")
    responseModelsServer("rs")
    identificationServer("id")
    resourcesServer("re")
  }
  shinyApp(ui = ui, server = server)
}




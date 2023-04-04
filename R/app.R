library(shiny)

ModifiedChannelThresholds <- function(...) {
  ui <- fluidPage(
    titlePanel("Biointegrity and Biostimulatory Thresholds for Modified Channels and Other Classes of Streams"),
    tabsetPanel(
      tabPanel("Background", backgroundUI("bg")),
      tabPanel("Biostimulatory Response Models", responseModelsUI("rs")),
      tabPanel("Threshold Query and Synthesis", identificationUI("id"))
    )
  )
    
  
  server <- function(input, output, session) {
    backgroundServer("bg")
    responseModelsServer("rs")
    identificationServer("id")
  }
  shinyApp(ui = ui, server = server)
}




#' Main Shiny app function call
#'
#' @param ... not used
#'
#' @return shiny app object
#' @export
#'
#' @import bslib
ModifiedChannelThresholds <- function(...) {
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 16))
  thematic::thematic_shiny(font = "auto")
  
  ui <- page_navbar(
    title = "ModifiedChannelThresholds",
    nav_panel("Background", backgroundUI("bg")),
    nav_panel("Eutrophication Response Models", responseModelsUI("rs")),
    nav_panel("Threshold Query and Synthesis", identificationUI("id")),
    nav_panel("Resources", resourcesUI("re"))
  )
  
  server <- function(input, output, session) {
    backgroundServer("bg")
    responseModelsServer("rs")
    identificationServer("id")
    resourcesServer("re")
  }
  shinyApp(ui = ui, server = server)
}




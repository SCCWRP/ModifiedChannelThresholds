#' Main Shiny app function call
#'
#' @param ... not used
#'
#' @return shiny app object
#' @export
#'
#' @import bslib
ModifiedChannelThresholds <- function(...) {
  ui <- page_navbar(
    title = '[Draft] Modified channel thresholds',
    theme = bs_theme(
      preset = 'cosmo',
      'headings-color' = '#2f5496'
    ),
    header = tags$head(tags$style(
      HTML('
        .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
          right: var(--bslib-full-screen-enter-right);
        }
        table td.withPlaceholder:empty:before {
          content: "Double click to input";
          color: gray;
        }
      '),
      shinyjs::useShinyjs()
    )),
    nav_panel('Background', backgroundUI('bg')),
    nav_panel('Eutrophication Response Models', responseModelsUI('rs')),
    nav_panel('Threshold Query and Synthesis', identificationUI('id')),
    nav_panel('Resources', resourcesUI('re'))
  )
  
  server <- function(input, output, session) {
    backgroundServer('bg')
    responseModelsServer('rs')
    identificationServer('id')
    resourcesServer('re')
  }
  shinyApp(ui = ui, server = server)
}




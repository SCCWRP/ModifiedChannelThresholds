responseModelsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$some_text <- renderText("some text")
  })
}
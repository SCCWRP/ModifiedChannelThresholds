resourcesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$thresh_table <- downloadHandler(
      "Thresholds.csv",
      content = function(file) write.csv(thresholds, file, row.names = F)
    )
  })
}
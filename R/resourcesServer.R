resourcesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$thresh_table <- downloadHandler(
      "Thresholds.csv",
      content = function(file) write.csv(thresholds, file, row.names = F)
    )
    
    output$data_sets <- downloadHandler(
      'Data_Sets.xlsx',
      content = function(file) {
        file.copy(
          system.file("extdata", "Data sets used in each analysis_final.xlsx", package = "ModifiedChannelThresholds"),
          file
        )
      }
    )
  })
}
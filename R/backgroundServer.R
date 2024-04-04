backgroundServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$csci <- renderImage({
      list(src = "figures/csci.jpg", width = "137px", height = "175px")
    }, deleteFile = FALSE)
    output$asci <- renderImage({
      list(src = "figures/asci.jpg", width = "175px", height = "175px")
    }, deleteFile = FALSE)
    output$perc_cover <- renderImage({
      list(src = "figures/percent_cover.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$ws <- renderImage({
      list(src = "figures/ws.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$rfi <- renderImage({
      list(src = "figures/rfi.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$sfi <- renderImage({
      list(src = "figures/sfi.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$cvf <- renderImage({
      list(src = "figures/cvf.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$hb <- renderImage({
      list(src = "figures/hb.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$sb2 <- renderImage({
      list(src = "figures/sb2.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$sb1 <- renderImage({
      list(src = "figures/sb1.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$sb0 <- renderImage({
      list(src = "figures/sb0.jpg", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$ref_thresholds <- renderImage({
      list(src = "figures/new_ref_thresholds.png", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$res_thresholds <- renderImage({
      list(src = "figures/res_thresholds.png", width = "100%", height = "auto")
    }, deleteFile = FALSE)
    output$best_thresholds <- renderImage({
      list(src = "figures/new_best_thresholds.png", width = "100%", height = "auto")
    }, deleteFile = FALSE)
  })
}

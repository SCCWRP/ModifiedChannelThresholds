#' helpers
#'
#' @description A utils function to fix the height of a shinycssloaders spinner within a bslib::card
#'
#' @return shiny output object with corrected spinner div height
#'
#' @noRd
adjust_spinner_height <- function(x) {
  x[[4]] <- x[[4]] |>
    bslib::as_fill_carrier()
  x
}


make_placeholder_plot <- function(msg) {
  placeholder <- ggplot() +
    geom_text(
      aes(x = 0.5, y = 0.80, label = msg),
      size = 14, size.unit = "pt"
    ) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  placeholder
}
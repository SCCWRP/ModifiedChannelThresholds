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

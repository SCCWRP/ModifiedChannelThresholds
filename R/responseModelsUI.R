#' Response models tab
#'
#' @param id module id 
#'
#' @return shiny ui object
#'
#' @import bslib
responseModelsUI <- function(id) {
  tagList(
    tags$head(tags$style(
      HTML('
        .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
        }
      ')
    )),
    h3('Eutrophication Response Models'),
    p(
      'Response models can help managers identify stress levels above which poor biological conditions are likely to occur. We calibrated a type of general additive model (shape-constrained additive models with monotonic negative relationships) between biointegrity index scores and eutrophication indicator concentrations. Managers can use these models to identify candidate eutrophication thresholds for a wide range of biointegrity goals for the CSCI, ASCI_D, or ASCI_H.'
    ),
    p(
      'After entering the biointegrity goals and selecting the desired eutrophication indicators, the dashboard will provide a table of the resulting eutrophication thresholds, along with plots showing the model responses. Plots and table entries will not be generated for indices with blank values in the Goal column. Enter or change threshold goal values by double-clicking each cell in the Goal column. Click the Submit button below the table to submit.'
    ),
    layout_columns(
      col_widths = c(2, 10),
      card(
        card_body(
          fill = FALSE,
          'Biointegrity Goals', 
          DT::dataTableOutput(NS(id, 'user_input_table'), fill = FALSE)
        ),
        'Eutrophication Analytes',
        shinyWidgets::pickerInput(
          inputId = NS(id, 'biostim'),
          label = NULL,
          choices = unique(raw_dat$Stressor),
          selected = c('Total N', 'Total P', 'Chl-a', 'AFDM', '% cover'),
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        ),
        card_body(
          fill = FALSE,
          actionButton(NS(id, 'submit'), 'Submit')          
        )
      ),
      card(
        card_header(
          "Response Models"
        ),
        layout_column_wrap(
          width = "500px",
          plotOutput(NS(id, 'plots')) |>
            shinycssloaders::withSpinner() |>
            adjust_spinner_height(),
          DT::dataTableOutput(NS(id, 'table')) |>
            shinycssloaders::withSpinner() |>
            adjust_spinner_height()
        )
      )
    )
  )
}
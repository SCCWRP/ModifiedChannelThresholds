responseModelsUI <- function(id) {
  fluidPage(
    HTML(
      '
        <h3 style="color:#2f5496">Eutrophication Response Models</h3>
        <p> Response models can help managers identify stress levels above which poor biological conditions are likely to occur. We calibrated a type of general additive model (shape-constrained additive models with monotonic negative relationships) between biointegrity index scores and eutrophication indicator concentrations. Managers can use these models to identify candidate eutrophication thresholds for a wide range of biointegrity goals for the CSCI, ASCI_D, or ASCI_H. </p>
        <p>Enter or change threshold goal values by double-clicking each cell in the Goal column. Click the Submit button below the table to submit. After entering the biointegrity goals and selecting the desired biointegrity and eutrophication indicators, the dashboard will provide a table of the resulting eutrophication thresholds, along with plots showing the model responses. Plots and table entries will not be generated for indices with blank values in the Goal column.</p>
      '
    ),
    fluidRow(
      column(2,
             DT::dataTableOutput(NS(id, "user_input_table")),
             br(),
             shinyWidgets::pickerInput(inputId = NS(id, "biostim"),
                         label = "Eutrophication Analyte",
                         choices = unique(raw_dat$Stressor),
                         selected = c("Total N", "Total P", "Chl-a", "AFDM", "% cover"),
                         options = list(`actions-box` = TRUE),
                         multiple = TRUE),
             div(style = "display:block; float:right", actionButton(NS(id, "submit"), "Submit"))
      ), #End column
      column(
        10,
        plotOutput(NS(id, "plots"))
      )
    ), #End Fluid Row
    DT::dataTableOutput(NS(id, "table"))
  )
}
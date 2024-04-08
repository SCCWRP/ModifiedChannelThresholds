responseModelsUI <- function(id) {
  fluidPage(
    HTML(
      '
        <h3 style="color:#2f5496">Eutrophication Response Models</h3>
        <p> Response models can help managers identify stress levels above which poor biological conditions are likely to occur. We calibrated a type of general additive model (shape-constrained additive models with monotonic negative relationships) between biointegrity index scores and eutrophication indicator concentrations. Managers can use these models to identify candidate eutrophication thresholds for a wide range of biointegrity goals for the CSCI, ASCI_D, or ASCI_H. </p>
        <p>After entering the biointegrity goal and selecting the desired biointegrity and eutrophication indicators, the dashboard will provide a table of the resulting eutrophication thresholds, along with a plot showing the model response.</p>
      '
    ),
    fluidRow(
      column(2,
             numericInput(NS(id, "goal"), "Threshold goal", 
                          value = 0.75, 
                          min = 0, 
                          max = 300, step = 0.05),
             shinyWidgets::pickerInput(inputId = NS(id, "index"),
                         label = "Index",
                         choices = unique(raw_dat$Index),
                         selected = c("CSCI", "ASCI_H", "ASCI_D"),
                         options = list(`actions-box` = TRUE),
                         multiple = TRUE),
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
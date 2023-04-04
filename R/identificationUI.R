identificationUI <- function(id) {
  fluidPage(
    HTML('<h3 style="color:#2f5496">Threshold query and synthesis</h3>'),
    fluidRow(
      column(
        12,
        "This dashboard allows users to query biointegrity and biostimulatory thresholds presented in SCCWRP TR####. In addition, it allows users to compare observed data to these thresholds, facilitating assessments of modified channels and other special classes of streams in California.",
        br(),
        conditionalPanel(
          "input.submit",
          div(style = "display:block; float:left", downloadButton(NS(id, "download_graphic"), "Download Graphic"), downloadButton(NS(id, "download_table"), "Download Table")),
          ns = NS(id)
        )
      )
    ),
    fluidRow(
      column(
        3,
        br(),
        HTML('<strong style="font-size:16px;color=#2f5496">Identify Thresholds</strong>'),
        br(),
        "Identify the thresholds you wish to evaluate. Under “Class”, select the appropriate stream types (traditionally-derived thresholds for wadeable streams will be included with results). Multiple options may be selected. Under “Stringency” select whether you want to evaluate high-, intermediate-, or low-stringency thresholds (only one stringency may be selected). Under “Indicator”, select which biointegrity or biostimulatory thresholds you want to evaluate.",
        shinyWidgets::pickerInput(
          NS(id, "Class_fullname"), 
          label = "Class", 
          choices = class_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE)
        ),
        shinyWidgets::pickerInput(
          NS(id, "Stringency"),
          label = "Stringency", 
          choices = stringency_choices,
          selected = stringency_choices |> dplyr::first()
        ),
        shinyWidgets::pickerInput(
          NS(id, "Indicator"),
          label = "Indicator", 
          choices = indicator_choices,
          selected = indicator_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE)
        )
      ),
      column(
        4,
        br(),
        HTML('<strong style="font-size:16px;color=#2f5496">Enter Observed Values</strong>'),
        br(),
        "Enter values observed at a site to evaluate with the thresholds identified above by double-clicking each cell in the Observed Value column. If data are missing, leave the observed value blank. Click the Submit button below the table to submit.",
        DT::dataTableOutput(NS(id, "user_input_table")),
        br(),
        div(style = "display:block; float:right",actionButton(NS(id, "clear"), "Clear"), actionButton(NS(id, "submit"), "Submit")),
        br(),
        br()
      ),
      column(
        5,
        conditionalPanel(
          "input.submit",
          br(),
          HTML('<strong style="font-size:16px;color=#2f5496">Comparison of observed values with selected thresholds</strong>'),
          br(),
          "Numbers in cells are the selected thresholds. Numbers in parentheses in the x-axis labels are the observed values. Color of the cell indicates whether the observed values passed or failed the threshold.",
          plotOutput(NS(id, "assessment_plot"), width = "6.5in", height = "7.5in"),
          ns = NS(id)
        )
      )
    ),
    fluidRow(
      column(
        12,
        conditionalPanel(
          "input.submit",
          DT::dataTableOutput(NS(id, "threshold_table")),
          ns = NS(id)
        )
      )
    )
  )
}
  
  

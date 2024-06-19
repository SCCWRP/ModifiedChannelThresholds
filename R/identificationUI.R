#' Indentification tab
#'
#' @param id module id
#'
#' @return shiny ui output
#'
#' @import bslib
identificationUI <- function(id) {
  page_fluid(
    theme = bs_theme(preset = "cosmo"),
    h3('Threshold query and synthesis'),
    fluidRow(
      column(
        12,
        "This dashboard allows users to query biointegrity and eutrophication thresholds presented in SCCWRP TR1367. In addition, it allows users to compare observed data to these thresholds, facilitating assessments of modified channels and other special classes of streams in California.",
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
        HTML("Under <strong>Region</strong>, select the region of interest. <br> Under <strong>Flow Duration</strong>, select the appropriate class. <br> Under <strong>Modification Type</strong>, select the appropriate class. <br> Under <strong>Stringency</strong>, select the desired level of stringency (only one may be selected). <br> Under <strong>Indicator</strong>, select which biointegrity or eutrophication indicators you wish to evaluate.<br>"),
        shinyWidgets::pickerInput(
          NS(id, "Region"), 
          label = "Region", 
          choices = region_choices,
          selected = region_choices[1]
        ),
        shinyWidgets::pickerInput(
          NS(id, "Flow_Dur"),
          label = "Flow Duration", 
          choices = flow_duration_choices,
          selected = flow_duration_choices[1]
        ),
        shinyWidgets::pickerInput(
          NS(id, "Mod_Status"),
          label = "Modification Type", 
          choices = modification_type_choices
        ),
        shinyWidgets::pickerInput(
          NS(id, "Stringency"),
          label = "Stringency", 
          choices = stringency_choices,
          selected = stringency_choices[1]
        ),
        shinyWidgets::pickerInput(
          NS(id, "Indicator"),
          label = "Indicator", 
          choices = indicator_choices,
          selected = indicator_choices,
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(actionsBox = TRUE)
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
          tabsetPanel(
            tabPanel(title = "Summary", plotOutput(NS(id, "assessment_plot"), width = "6.5in", height = "7.5in")),
            tabPanel(title = "Detail", plotOutput(NS(id, "assessment_plot_detail"), width = "6.5in", height = "7.5in"))
          ),
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
  
  

#' Indentification tab
#'
#' @param id module id
#'
#' @return shiny ui output
#'
#' @import bslib
identificationUI <- function(id) {
  tagList(
    h3('Threshold query and synthesis'),
    'This dashboard allows users to query biointegrity and eutrophication thresholds presented in SCCWRP TR1367. In addition, it allows users to compare observed data to these thresholds, facilitating assessments of modified channels and other special classes of streams in California.',
    card(
      full_screen = TRUE,
      style = "border: 0px",
      layout_columns(
        col_widths = c(2, 5, 5),
        card(
          card_header(strong('Identify thresholds')),
          shinyWidgets::pickerInput(
            NS(id, 'Region'), 
            label = tooltip(
              trigger = span('Region', bsicons::bs_icon('info-circle')),
              'Select the region of interest'
            ), 
            choices = region_choices,
            selected = region_choices[1]
          ),
          shinyWidgets::pickerInput(
            NS(id, 'Flow_Dur'),
            label = tooltip(
              trigger = span('Flow duration', bsicons::bs_icon('info-circle')),
              'Select the appropriate class'
            ),
            choices = flow_duration_choices,
            selected = flow_duration_choices[1]
          ),
          shinyWidgets::pickerInput(
            NS(id, 'Mod_Status'),
            label = tooltip(
              trigger = span('Modification type', bsicons::bs_icon('info-circle')),
              'Select the appropriate class'
            ),
            choices = modification_type_choices
          ),
          shinyWidgets::pickerInput(
            NS(id, 'Stringency'),
            label = tooltip(
              trigger = span('Stringency', bsicons::bs_icon('info-circle')),
              'Select the desired level of stringency. Only one may be selected'
            ), 
            choices = stringency_choices,
            selected = stringency_choices[1]
          ),
          shinyWidgets::pickerInput(
            NS(id, 'Indicator'),
            label = tooltip(
              trigger = span('Indicator', bsicons::bs_icon('info-circle')),
              'Select which biointegrity or eutrophication indicators you wish to evaluate'
            ),
            choices = indicator_choices,
            selected = indicator_choices,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )
        ),
        card(
          card_header(
            tooltip(
              trigger = span(strong('Enter observed values'), bsicons::bs_icon('info-circle')),
              'Enter values observed at a site to evaluate with the identified thresholds by double-clicking each cell in the Observed Value column. If data are missing, leave the observed value blank. Click the Submit button below the table to submit'
            )
          ),
          DT::dataTableOutput(NS(id, 'user_input_table')),
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            actionButton(NS(id, 'clear'), 'Clear'),
            actionButton(NS(id, 'submit'), 'Submit')
          )
        ),
        navset_card_underline(
          title = tooltip(
            trigger = span(strong('Comparison of observed values with selected thresholds'), bsicons::bs_icon('info-circle')),
            'Numbers in cells are the selected thresholds. Numbers in parentheses in the x-axis labels are the observed values. Color of the cell indicates whether the observed values passed or failed the threshold'
          ),
          nav_panel(
            'Summary',
            card_body(
              card_body(
                class = 'p-0',
                min_height = "400px",
                plotOutput(NS(id, 'assessment_plot')) |>
                  shinycssloaders::withSpinner() |>
                  adjust_spinner_height(),
              )
            ),
            card_body(
              fill = FALSE,
              downloadButton(NS(id, 'download_graphic_summary'), 'Download Graphic')
            )
          ),
          nav_panel(
            'Detail',
            card_body(
              card_body(
                class = 'p-0',
                min_height = "400px",
                plotOutput(NS(id, 'assessment_plot_detail')) |>
                  shinycssloaders::withSpinner() |>
                  adjust_spinner_height()
              )            
            ),
            card_body(
              fill = FALSE,
              downloadButton(NS(id, 'download_graphic_detail'), 'Download Graphic')
            )
          ),
          nav_panel(
            'Table',
            DT::dataTableOutput(NS(id, 'threshold_table')) |>
              shinycssloaders::withSpinner() |>
              adjust_spinner_height(),
            downloadButton(NS(id, 'download_table'), 'Download Table')
          )
        )
      )
    )
  )
}
  
  

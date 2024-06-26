responseModelsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # threshold user input
    thresh_goal_table <- reactiveValues(data = {
      data.frame(
        Index = c('ASCI_D', 'ASCI_H', 'CSCI'),
        Goal = c(NA_real_, NA_real_, NA_real_)
      )
    })
    
    output$user_input_table <- DT::renderDataTable({
      thresh_goal_table$data
    }, 
      editable = list(target = 'cell', disable = list(columns = c(0))), 
      options = list(dom = 't', ordering = FALSE, columnDefs = list(list(targets = 1, className = "withPlaceholder"))), 
      selection = 'none',
      rownames = FALSE
    ) |>
      bindEvent(thresh_goal_table$data)
    
    observe({
      thresh_goal_table$data <<- DT::editData(
        thresh_goal_table$data, 
        input$user_input_table_cell_edit, 
        rownames = FALSE
      ) 
      
      thresh_goal_table$data <- thresh_goal_table$data |>
        dplyr::mutate(
          Goal = dplyr::case_when(
            Goal < 0 ~ NA_real_,
            .default = round(Goal, 2)
          )
        )
    }) |>
      bindEvent(input$user_input_table_cell_edit)
    
    selected_indices <- reactive({
      thresh_goal_table$data$Index[!is.na(thresh_goal_table$data$Goal)]
    })
    
    #function to calculate goal
    thresh_dat <- reactive({
      raw_dat |>
        dplyr::rename(`Eutrophication Variable` = BiostimVar) |>
        dplyr::filter(Stressor %in% input$biostim) |> 
        dplyr::filter(Index %in% selected_indices()) |> 
        dplyr::inner_join(thresh_goal_table$data, by = dplyr::join_by(Index, Fit >= Goal)) |> 
        dplyr::group_by(Stressor,Index) |>
        dplyr::slice_max(BiostimValue, n=1) |>
        dplyr::ungroup() |>
        dplyr::rename(`Threshold Candidate`=BiostimValue, 
               IndexScore_predicted = Fit,
               IndexScore_predicted_se = SE) |>
        dplyr::mutate(
          IndexScore_predicted_l95 = round(IndexScore_predicted - IndexScore_predicted_se * 1.96, 2),
          IndexScore_predicted_u95 = round(IndexScore_predicted + IndexScore_predicted_se * 1.96, 2),
          IndexScore_predicted_se = round(IndexScore_predicted_se, 3),
          IndexScore_predicted = round(IndexScore_predicted, 2),
          `Threshold Candidate` = dplyr::case_when(
            Stressor == 'Total N' ~ round(`Threshold Candidate`, 3),
            Stressor == 'Total P' ~ round(`Threshold Candidate`, 3),
            Stressor == 'Chl-a' ~ round(`Threshold Candidate`, 1),
            Stressor == 'AFDM' ~ round(`Threshold Candidate`, 1),
            Stressor == '% cover' ~ round(`Threshold Candidate`)
          ),
          Units = dplyr::case_when(
            Stressor == 'Total N' ~ 'mg/L',
            Stressor == 'Total P' ~ 'mg/L',
            Stressor == 'Chl-a' ~ 'mg/m2',
            Stressor == 'AFDM' ~ 'g/m2',
            Stressor == '% cover' ~ '%'
          )
        ) |>
        dplyr::select(
          Stressor, Units, `Threshold Candidate`, Index, 
          IndexScore_predicted, IndexScore_predicted_se, IndexScore_predicted_l95, IndexScore_predicted_u95
        )
    })
    
    #function to render table using above function 
    output$table <- DT::renderDataTable({
      thresh_dat() |>
        dplyr::mutate(
          Stressor = case_when(
            Stressor == 'Total N' ~ 'Total N (mg/L)',
            Stressor == 'Total P' ~ 'Total P (mg/L)',
            Stressor == 'Chl-a' ~ 'Chl-a (mg/m2)',
            Stressor == 'AFDM' ~ 'AFDM (g/m2)',
            .default = Stressor
          )
        ) |>
       dplyr:: select(-Units)
    }, 
      options = list(searching = FALSE, bLengthChange = FALSE), 
      selection = 'none',
      rownames = FALSE,
      fillContainer = TRUE,
      colnames = c(
        'Stressor', 'Threshold Candidate', 'Index', 
        'Score', 'Score SE', 'Score L95', 'Score U95'
      )
    ) |>
      bindEvent(input$submit, ignoreNULL = FALSE)
    
    # #function to create plots
    plot_data <- reactive({
      plot_raw_dat <- raw_dat |>
        dplyr::filter(Stressor %in% input$biostim, Index %in% selected_indices()) |>
        dplyr::mutate(
          Stressor = case_when(
            Stressor == 'Total N' ~ 'Total N (mg/L)',
            Stressor == 'Total P' ~ 'Total P (mg/L)',
            Stressor == 'Chl-a' ~ 'Chl-a (mg/m2)',
            Stressor == 'AFDM' ~ 'AFDM (g/m2)',
            .default = Stressor
          ),
          Stressor = factor(
            Stressor, 
            levels = c('Total N (mg/L)', 'Total P (mg/L)', 'Chl-a (mg/m2)', 'AFDM (g/m2)', '% cover')
          )
        )
      
      plot_obs_df <- obs_points_df |>
        dplyr::filter(Stressor %in% input$biostim, Index %in% selected_indices()) |>
        dplyr::mutate(
          Stressor = case_when(
            Stressor == 'Total N' ~ 'Total N (mg/L)',
            Stressor == 'Total P' ~ 'Total P (mg/L)',
            Stressor == 'Chl-a' ~ 'Chl-a (mg/m2)',
            Stressor == 'AFDM' ~ 'AFDM (g/m2)',
            .default = Stressor
          ),
          Stressor = factor(
            Stressor, 
            levels = c('Total N (mg/L)', 'Total P (mg/L)', 'Chl-a (mg/m2)', 'AFDM (g/m2)', '% cover')
          )
        )
      
      plot_thresh_dat <- thresh_dat() |>
        dplyr::mutate(
          Stressor = case_when(
            Stressor == 'Total N' ~ 'Total N (mg/L)',
            Stressor == 'Total P' ~ 'Total P (mg/L)',
            Stressor == 'Chl-a' ~ 'Chl-a (mg/m2)',
            Stressor == 'AFDM' ~ 'AFDM (g/m2)',
            .default = Stressor
          ),
          Stressor = factor(
            Stressor, 
            levels = c('Total N (mg/L)', 'Total P (mg/L)', 'Chl-a (mg/m2)', 'AFDM (g/m2)', '% cover')
          )
        )
      if (all(nrow(plot_raw_dat) == 0, nrow(plot_obs_df) == 0, nrow(plot_thresh_dat) == 0)) {
        placeholder <- make_placeholder_plot(
          msg = "Enter threshold goals and analytes\nto view response model plots"
        )
        return(placeholder)
      }

      ggplot(data = plot_raw_dat, aes(x = BiostimValue, y = Fit))+
        geom_point(data = plot_obs_df, aes(y = IndexScore), size = 0.5, color = 'gray') +
        geom_ribbon(
          mapping = aes(ymin = Fit - 1.96 * SE, ymax = Fit + 1.96 * SE), 
          alpha = 0.2, fill = '#39568cff'
        ) +
        geom_path(linewidth = 1, color = '#39568cff') +
        facet_grid(Index ~ Stressor, scales = 'free_x') +
        labs(x = '', y = 'Index score') +
        geom_hline(
          data = thresh_goal_table$data |> dplyr::filter(Index %in% selected_indices()), 
          mapping = aes(yintercept = Goal), 
          color = 'red', linetype = 'dashed', linewidth = 1
        ) +
        geom_vline(
          data = plot_thresh_dat, 
          mapping = aes(xintercept = `Threshold Candidate`), 
          color = 'red', linetype = 'dashed', linewidth = 1
        ) +
        geom_label(
          data = plot_thresh_dat,
          mapping = aes(x = Inf, y = Inf, label = `Threshold Candidate`),
          label.size = NA, hjust = 1, vjust = 1
        )
    }) |>
      bindEvent(input$submit, ignoreNULL = FALSE)
    
    #function to render plot using above function
    output$plots <- renderPlot({
      plot_data()
    }, res = 96)
  })
}
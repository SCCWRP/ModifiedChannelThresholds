responseModelsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #function to calculate goal
    thresh_dat <- reactive({
      raw_dat |>
        dplyr::rename(`Eutrophication Variable` = BiostimVar) |>
        dplyr::filter(Stressor %in% input$biostim) |> 
        dplyr::filter(Index %in% input$index) |> 
        dplyr::filter(Fit>=input$goal) |> 
        dplyr::group_by(Stressor,Index) |>
        dplyr::slice_max(BiostimValue, n=1) |>
        dplyr::ungroup() |>
        dplyr::rename(`Threshold Candidate`=BiostimValue, 
               IndexScore_predicted=Fit,
               IndexScore_predicted_se=SE) |>
        dplyr::mutate(IndexScore_predicted_l95=IndexScore_predicted-IndexScore_predicted_se*1.96,
               IndexScore_predicted_u95=IndexScore_predicted+IndexScore_predicted_se*1.96)
    })
    #function to render table using above function 
    output$table <- DT::renderDataTable({
      thresh_dat()
    }) |>
      bindEvent(input$submit)
    # #function to create plots
    plot_data <- reactive({
      ggplot(data=raw_dat |>
               dplyr::filter(Stressor %in% input$biostim, 
                             Index %in% input$index), aes(x=BiostimValue, y=Fit))+
        geom_point(data=obs_points_df |>
                     dplyr::filter(Stressor %in% input$biostim, 
                                   Index %in% input$index),
                   size=.5, aes(y=IndexScore), color="gray") +
        geom_ribbon(aes(ymin=Fit-1.96*SE, ymax=Fit+1.96*SE), alpha=.2, fill="#39568cff")+
        geom_path(linewidth=1, color="#39568cff")+
        facet_grid(Index~Stressor, scales="free_x")+
        xlab("")+ylab("Index score")+
        geom_hline(data=thresh_dat(), aes(yintercept=IndexScore_predicted), color="red", linetype="dashed", linewidth=1)+
        geom_vline(data=thresh_dat(), aes(xintercept=Threshold_candidate), color="red", linetype="dashed", linewidth=1)+
        theme_bw()+
        geom_label(data = thresh_dat(),
                   label.size=NA,
                   mapping = aes(x = Inf, y = Inf, label = Threshold_candidate),
                   hjust   = 1,
                   vjust   = 1
        )
      
    })
    #function to render plot using above function
    output$plots <- renderPlot({
      plot_data()
    }) |>
      bindEvent(input$submit)
  })
}
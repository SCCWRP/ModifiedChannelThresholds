library(ggplot2)

identificationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    synthesize_thresholds_plotdata <- function(
      classes,
      thresholds_df,
      indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
      stringency="Intermediate") {
      
      
      thresh_dat <- thresholds_df |>
        dplyr::filter(Stringency==stringency) |>
        dplyr::filter(Class %in% c("Wadeable streams",classes)) |>
        dplyr::filter(Indicator %in% indicators) |>
        dplyr::mutate(Index = dplyr::case_when(Indicator_Type=="Biointegrity"~Indicator,
                                 Approach=="Response"~Response_model_index,
                                 T~"Not applicable"))
      thresh_dat_sum<-thresh_dat |>
        dplyr::filter(!Flagged) |>
        dplyr::group_by(Indicator) |>
        dplyr::summarise(Threshold_mean= mean(Threshold_value, na.rm=T))
      thresholds_plotdata <- list(thresh_dat, thresh_dat_sum)
      thresholds_plotdata
    }
    
    
    synthesize_thresholds_obs_plot<-function(
      classes,
      thresholds_df,
      obs_data,
      indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
      stringency="Intermediate") {
      
      obs_data <- obs_data |>
        dplyr::select(Indicator, Observed_value) |>
        dplyr::filter(Indicator %in%  indicators) |>
        dplyr::mutate(Indicator=factor(Indicator, levels=c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover")))
      
      thresholds_plotdata <- synthesize_thresholds_plotdata(classes=classes, thresholds_df=thresholds_df, indicators=indicators, stringency = stringency) 
      xdf <- obs_data |>
        dplyr::rename(Value=Observed_value) |>
        dplyr::mutate(DataType="Observed value") |>
        dplyr::bind_rows(
          thresholds_plotdata[[2]] |>
            dplyr::rename(Value=Threshold_mean) |>
            dplyr::mutate(DataType="Mean of thresholds")
        )
      multiple_thresholds<-thresholds_plotdata[[1]] |>
        dplyr::filter(!Flagged) |>
        dplyr::mutate(ClassIndicator = paste0(Class, Indicator)) |>
        dplyr::group_by(ClassIndicator) |>
        dplyr::tally() |>
        dplyr::ungroup()|>
        dplyr::filter(n>1) 
      
      ydf<-thresholds_plotdata[[1]] |>
        dplyr::mutate(ClassIndicator = paste0(Class, Indicator))
      ggplot(data=ydf, 
             aes(x=Class, y=Threshold_value))+
        geom_point(aes(fill=Approach4, shape=Index, 
                       size=Flagged),
                   position=position_dodge(width=0))+
        stat_summary(data= ydf |> 
                       dplyr::filter(!Flagged) |>
                       dplyr::filter(ClassIndicator %in% multiple_thresholds$ClassIndicator), fun.y="mean", geom="crossbar", size=.25)+
        geom_hline(data=xdf, aes(yintercept=Value, color=DataType))+
        scale_color_manual(values=c("black","violet"), name="", labels=c("Mean of unflagged thresholds\n(Within or across classes)","Observed value"))+
        facet_wrap(~Indicator, scales="free", ncol=2)+
        scale_shape_manual(values=c(24,25,22, 21), name="Response model index")+
        scale_fill_manual(values=c("#e41a1c","#377eb8","#33a02c","#b2df8a"), name="Approach")+
        scale_size_manual(values=c(2,1), name="Flagged?", labels=c("No","Yes"))+
        theme_bw()+
        coord_flip()+
        guides(shape=guide_legend(override.aes = list(fill="gray", size=2), order=2),
               fill=guide_legend(override.aes = list(shape=21, size=2), order=1),
               size=guide_legend(order=3),
               color=guide_legend(order=4)
        )+
        theme(legend.position = "bottom",
              legend.direction = "vertical")+
        xlab("")+ylab("")
    }

    selected_classes <- reactive({
      channel_cross_walk |>
        dplyr::filter(
          Region == input$Region,
          FlowDuration == input$Flow_Dur,
          ModificationStatus == input$Mod_Status
        ) |>
        dplyr::select(where(~ . == 'Yes')) |>
        names()
    })
    
        
    threshold_data <- reactive({
      threshold_static |>
        dplyr::filter(
          Class %in% selected_classes(),
          Stringency %in% input$Stringency,
          Indicator %in% input$Indicator
        ) |>
        dplyr::mutate(
          Class = as.factor(Class),
          Approach = as.factor(Approach),
          Response_model_form = as.factor(Response_model_form),
          Response_model_index = as.factor(Response_model_index),
          Approach4 = dplyr::case_when(
            Approach != "Response" ~ paste0(Class," - ",Approach),
            Approach == "Response" ~ paste0(Class," - ",Approach," (",Response_model_form,", ", Response_model_index,")"),
            T ~ "OTHER"
          ),
          Approach4 = factor(Approach4, levels = Approach4 |> 
                                                   unique() |>
                                                   rev()),
          Flagged = !is.na(Flag)
        ) |>
        dplyr::arrange(
          Class,
          Approach,
          Response_model_form,
          Response_model_index
        )
    }) |>
      bindEvent(input$Region, input$Mod_Status, input$Flow_Dur, input$Stringency, input$Indicator, input$submit)
    
    
    my_thresh_df <- reactive({
      threshold_data() |>
        dplyr::inner_join(obs_table$data) |>
        dplyr::mutate(Threshold_pass = dplyr::case_when(
          is.na(Observed_value)~"No data",
          is.na(Threshold_value)~"No threshold identified",
          Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & is.na(Flag) ~ "Fails",
          Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & is.na(Flag) ~ "Passes",
          Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & !is.na(Flag) ~ "Fails but flagged",
          Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & !is.na(Flag) ~ "Passes flagged",
          
          Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & is.na(Flag) ~ "Fails",
          Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & is.na(Flag) ~ "Passes",
          Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & !is.na(Flag) ~ "Fails but flagged",
          Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & !is.na(Flag) ~ "Passes flagged",
          
          T~"Other"),
          Threshold_pass = factor(Threshold_pass,
                                  levels=c("Passes","Passes flagged",
                                           "Fails","Fails but flagged",
                                           "No threshold identified",
                                           "No data")),
          obs_label = paste0(Indicator,"\n(",Observed_value,")")
        )
    })
    
    
    assessment_plot <- reactive({
      assessment_plot <- ggplot(data=my_thresh_df(), aes(x=obs_label, y=Approach4)) +
        geom_tile(aes(fill=Threshold_pass), color="white") +
        geom_text(aes(label=Threshold_value))+
        facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
        scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#cab2d6","#ff7f00","#fdbf6f"), name="Threshold", drop=F)+
        ylab("")+
        xlab("Indicator\n(Observed value)")+
        theme_bw()+
        theme(legend.position = "bottom",
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.title.x = element_text(color="gray25"))
      
      
      assessment_plot_cow <- cowplot::plot_grid(assessment_plot +
                                                  theme(legend.position = "none"),
                                                cowplot::get_legend(assessment_plot),
                                                nrow=2, rel_heights=c(1,.2))
      assessment_plot_cow
    })
    
    output$assessment_plot <- renderPlot({
      assessment_plot()
    }) |>
      bindEvent(input$submit, input$clear)
    
    
    assessment_plot_detail <- reactive({
      thresholds_df <- threshold_static |>
        dplyr::mutate(Indicator = factor(Indicator, levels = unique(Indicator)),
               Flagged = !is.na(Flag),
               Approach4 = dplyr::case_when(Approach=="Response"~paste0("Response (",Response_model_form,")"),
                                     T~Approach))
      
      
      synthesize_thresholds_obs_plot(classes = selected_classes(), thresholds_df = thresholds_df, obs_data = obs_table$data, indicators = input$Indicator, stringency = input$Stringency)
    })
    
    output$assessment_plot_detail <- renderPlot({
      assessment_plot_detail()
    }) |>
      bindEvent(input$submit, input$clear)
    
    
    
    
    obs_table <- reactiveValues(data = {
      threshold_static |>
        dplyr::distinct(Indicator_Type, Indicator) |>
        dplyr::arrange(Indicator_Type, match(Indicator, indicator_choices)) |>
        dplyr::mutate(
          Units = dplyr::case_when(
            Indicator == "ASCI_D" ~ "None",
            Indicator == "ASCI_H" ~ "None",
            Indicator == "CSCI" ~ "None",
            Indicator == "TN" ~ "mg/L",
            Indicator == "TP" ~ "mg/L",
            Indicator == "Chl-a" ~ "mg/m²",
            Indicator == "AFDM" ~ "g/m²",
            Indicator == "% cover" ~ "%"
          ),
          Observed_value = NA_real_
        )
    })
    
    
    output$user_input_table <- DT::renderDataTable({
      obs_table$data |>
        dplyr::mutate(
          Indicator = dplyr::case_when(
            Indicator == "ASCI_D" ~ "Algal Stream Condition Index for diatoms (ASCI_D)",
            Indicator == "ASCI_H" ~ "Algal Stream Condition Index for diatoms and soft-bodied algal taxa (ASCI_H)",
            Indicator == "CSCI" ~ "California Stream Condition Index for benthic macroinvertebrates (CSCI)",
            Indicator == "TN" ~ "Total nitrogen (TN)",
            Indicator == "TP" ~ "Total phosphorous (TP)",
            Indicator == "Chl-a" ~ "Benthic chlorophyll-a (Chl-a)",
            Indicator == "AFDM" ~ "Benthic Ash-Free Dry Mass (AFDM)",
            Indicator == "% cover" ~ "Percent algal cover on the streambed (% cover)"
          )
        )
    }, editable = list(target = "cell", disable = list(columns = c(1, 2, 3))), options = list(dom = 't'), selection = 'none') |>
      bindEvent(obs_table$data)
    
    
    observe({
      obs_table$data <<- DT::editData(obs_table$data, input$user_input_table_cell_edit)
    }) |>
      bindEvent(input$user_input_table_cell_edit)
    
    observe({
      obs_table$data$Observed_value <- NA_real_
    }) |>
      bindEvent(input$clear)
    
    output$threshold_table <- DT::renderDataTable({
      threshold_data() |>
        dplyr::select(
          Class,
          Class_fullname, 
          Stringency,
          Approach,
          Response_model_detail,
          Indicator_Type,
          Indicator,
          Threshold_value,
          Flag
        )
    }, selection = 'none') |>
      bindEvent(input$submit)
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("Threshold-Table-", Sys.Date(), ".csv")
      },
      content = function(file) {
        data = threshold_data() |>
          dplyr::select(
            Class,
            Class_fullname, 
            Stringency,
            Approach,
            Response_model_detail,
            Indicator_Type,
            Indicator,
            Threshold_value,
            Flag
          )
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    output$download_graphic <- downloadHandler(
      filename = function() {
        paste0("Threshold-Graphic-", Sys.Date(), ".png")
      },
      content = function(file) {
        cowplot::save_plot(file, plot = assessment_plot() + theme(plot.background = element_rect(fill = "white", color = NA)), base_height = 7.5, base_width = 6.5)
      }
    )
    
  })
}
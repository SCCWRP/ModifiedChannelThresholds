library(ggplot2)
library(dplyr)

identificationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_classes <- reactive({
      channel_cross_walk |>
        filter(
          Region == input$Region,
          FlowDuration == input$Flow_Dur,
          ModificationStatus == input$Mod_Status
        ) |>
        select(where(~ . == 'Yes')) |>
        names()
    })
    
    assessment_summary_plot <- function(thresh_df, obs_df) {
      my_thresh_df <- thresh_df |>
        inner_join(obs_df) |>
        mutate(
          Indicator_Type = stringr::str_replace(Indicator_Type, "Biostimulatory", "Eutrophication"),
          Threshold_pass = case_when(
            is.na(Observed_value) ~ "No data",
            is.na(Threshold_value) ~ "No threshold identified",
            Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & is.na(Flag) ~ "Fails",
            Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & is.na(Flag) ~ "Passes",
            Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & !is.na(Flag) ~ "Fails but flagged",
            Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & !is.na(Flag) ~ "Passes flagged",
            Indicator_Type == "Eutrophication" & Threshold_value < Observed_value & is.na(Flag) ~ "Fails",
            Indicator_Type == "Eutrophication" & Threshold_value >= Observed_value & is.na(Flag) ~ "Passes",
            Indicator_Type == "Eutrophication" & Threshold_value < Observed_value & !is.na(Flag) ~ "Fails but flagged",
            Indicator_Type == "Eutrophication" & Threshold_value >= Observed_value & !is.na(Flag) ~ "Passes flagged",
            .default = "Other"
          ),
          Threshold_pass = factor(
            Threshold_pass, 
            levels = c("Passes", "Passes flagged", "Fails", "Fails but flagged", "No threshold identified", "No data")
          ),
          Approach4 = factor(Approach4, levels = rev(unique(Approach4))),
          Threshold_value = case_when(
            Indicator_Type == "Biointegrity" ~ sprintf('%.2f', Threshold_value),
            Indicator == "% cover" ~ sprintf('%.0f', Threshold_value),
            Indicator %in% c("AFDM", "Chl-a") ~ sprintf('%.1f', Threshold_value),
            Indicator %in% c("TN", "TP") ~ sprintf('%.3f', Threshold_value),
            .default = sprintf('%.1f', Threshold_value)
          ),
          Threshold_value = stringr::str_remove_all(Threshold_value, "NA"),
          Observed_value = case_when(
            Indicator %in% c("CSCI", "ASCI_D", "ASCI_H") ~ sprintf('%.2f', Observed_value),
            Indicator == "% cover" ~ sprintf('%.0f', Observed_value),
            Indicator %in% c("AFDM", "Chl-a") ~ sprintf('%.1f', Observed_value),
            Indicator %in% c("TN", "TP") ~ sprintf('%.3f', Observed_value),
            .default = sprintf('%.1f', Observed_value)
          ),
          obs_label = paste0(Indicator, "\n(", Observed_value, ")")
        )
      
      threshold_colors <- c("#1f78b4", "#a6cee3", "#e31a1c", "#cab2d6", "#ff7f00", "#fdbf6f")
      assessment_plot <- ggplot(data = my_thresh_df, aes(x = obs_label, y = Approach4)) +
        geom_tile(aes(fill = Threshold_pass), color = "white", show.legend = TRUE) +
        geom_text(aes(label = Threshold_value)) +
        scale_fill_manual(name = "Threshold", values = threshold_colors, drop = F) +
        facet_wrap(~ Indicator_Type, ncol = 1, scales = "free", drop = T) +
        labs(y = "", x = "Indicator\n(Observed value)") +
        theme_bw() +
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_text(color = "gray25"),
          legend.location = "plot",
          legend.key.spacing.y = unit(0, "cm")
        )
      assessment_plot
    }
    
    assessment_detail_plot <- function(thresh_df, obs_data) {
      thresh_dat <- thresh_df |>
        filter(Indicator %in% obs_data$Indicator) |>
        mutate(
          Class = as.character(Class),
          Index = case_when(
            Indicator_Type == "Biointegrity" ~ Indicator,
            Approach == "Response" ~ Response_model_index,
            .default = "Not applicable"
          )
        )
      
      observed_and_grand_mean <- thresh_dat |>
        filter(!Flagged) |>
        group_by(Indicator) |>
        summarize(Value = mean(Threshold_value, na.rm = T), DataType = "Mean of thresholds") |>
        bind_rows(
          obs_data |>
            mutate(DataType = "Observed value") |>
            rename(Value = Observed_value)
        )
      
      unflagged_thresholds <- thresh_dat |> 
        filter(!Flagged) |>
        group_by(Class, Indicator) |>
        mutate(n = n()) |>
        filter(n > 1)
      
      ggplot(data = thresh_dat, aes(x = Class, y = Threshold_value)) +
        geom_point(aes(fill = Approach5, shape = Index, size = Flagged)) +
        stat_summary(data = unflagged_thresholds, fun = "mean", geom = "crossbar", linewidth = 0.25) +
        geom_hline(data = observed_and_grand_mean, aes(yintercept = Value, color = DataType)) +
        scale_color_manual(
          name = "", values = c("black", "violet"), 
          labels = c("Mean of unflagged thresholds\n(Within or across classes)", "Observed value")
        ) +
        facet_wrap(~ Indicator, scales = "free", ncol = 2) +
        scale_shape_manual(values = c(24, 25, 22, 21), name = "Response model index") +
        scale_fill_manual(values = c("#e41a1c", "#377eb8", "#33a02c", "#b2df8a"), name = "Approach") +
        scale_size_manual(values = c(2, 1), name = "Flagged?", labels = c("No", "Yes")) +
        theme_bw() +
        coord_flip() +
        guides(
          fill = guide_legend(override.aes = list(shape = 21, size = 2), order = 1),
          shape = guide_legend(override.aes = list(fill = "gray", size = 2), order = 2),
          size = guide_legend(order = 3),
          color = guide_legend(order = 4)
        ) +
        theme(legend.position = "bottom", legend.direction = "vertical") +
        labs(x = "", y = "")
    }
    
        
    threshold_data <- reactive({
      thresholds |>
        filter(
          Class %in% selected_classes(),
          Stringency %in% input$Stringency,
          Indicator %in% input$Indicator
        ) |>
        mutate(
          Indicator = factor(Indicator, levels = unique(Indicator)),
          Class = factor(Class, levels = c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC")),
          Flagged = !is.na(Flag),
          Approach = factor(Approach, levels = c("Reference", "Best observed", "Response")),
          Response_model_form = factor(Response_model_form, levels = c("LR", "SCAM")),
          Response_model_index = factor(Response_model_index, levels = c("ASCI_D", "ASCI_H", "CSCI")),
          Approach4 = case_when(
            Approach != "Response" ~ paste0(Class, " - ", Approach),
            Approach == "Response" ~ paste0(Class, " - ", Approach, " (", Response_model_form, ", ", Response_model_index, ")"),
            .default = "OTHER"
          ),
          Approach5 = if_else(Approach == "Response", paste0("Response (", Response_model_form, ")"), Approach)
        ) |>
        filter(!is.na(Class)) |>
        arrange(Class, Approach, Response_model_form, Response_model_index) 
    }) |>
      bindEvent(input$Region, input$Mod_Status, input$Flow_Dur, input$Stringency, input$Indicator, input$submit)

    
    output$assessment_plot <- renderPlot({
      assessment_summary_plot(threshold_data(), obs_table$data) + theme(legend.position = "none")
    }) |>
      bindEvent(input$submit, input$clear)
    
    output$assessment_plot_detail <- renderPlot({
      assessment_detail_plot(threshold_data(), obs_table$data)
    }) |>
      bindEvent(input$submit, input$clear)
    

    obs_table <- reactiveValues(data = {
      thresholds |>
        mutate(Indicator_Type = stringr::str_replace(Indicator_Type, "Biostimulatory", "Eutrophication")) |>
        distinct(Indicator_Type, Indicator) |>
        arrange(Indicator_Type, match(Indicator, indicator_choices)) |>
        mutate(
          Units = case_when(
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
        mutate(
          Indicator = case_when(
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
      }, 
      editable = list(target = "cell", disable = list(columns = c(1, 2, 3))), 
      options = list(dom = 't'), 
      selection = 'none'
    ) |>
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
        select(
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
          select(
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
        write.csv(data, file, row.names = FALSE, na = "")
      }
    )
    
    output$download_graphic <- downloadHandler(
      filename = function() {
        paste0("Threshold-Graphic-", Sys.Date(), ".png")
      },
      content = function(file) {
        cowplot::save_plot(
          file, 
          plot = assessment_summary_plot(threshold_data(), obs_table$data) + 
            theme(plot.background = element_rect(fill = "white", color = NA)), 
          base_height = 7.5, base_width = 6.5
        )
      }
    )
  })
}
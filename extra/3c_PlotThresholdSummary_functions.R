library(tidyverse)
library(ggplot2)

#Load and prep required data
thresholds_df<-read_csv("Data/mod_channel_thresholds.csv") %>%
  mutate(Indicator = factor(Indicator, levels = unique(Indicator)),
         Flagged = !is.na(Flag),
         Approach4 = case_when(Approach=="Response"~paste0("Response (",Response_model_form,")"),
                               T~Approach))


#Get user info

#Get thresholds
synthesize_thresholds_plotdata<-function(
    classes,
    indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
    stringency="Intermediate") {
  
  
  thresh_dat<-thresholds_df %>%
    filter(Stringency==stringency) %>%
    # filter(!Flagged) %>%
    filter(Class %in% c("Wadeable streams",classes)) %>%
    filter(Indicator %in% indicators) %>%
    mutate(Index = case_when(Indicator_Type=="Biointegrity"~Indicator,
                             Approach=="Response"~Response_model_index,
                             T~"Not applicable"))
  thresh_dat_sum<-thresh_dat %>%
    filter(!Flagged) %>%
    group_by(Indicator) %>%
    summarise(Threshold_mean= mean(Threshold_value, na.rm=T))
  thresholds_plotdata <- list(thresh_dat, thresh_dat_sum)
  thresholds_plotdata
}

synthesize_thresholds_plot<-function(
    classes,
    indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
    stringency="Intermediate") {
  
  thresholds_plotdata <- synthesize_thresholds_plotdata(classes=classes, indicators=indicators, stringency = stringency)
  ggplot(data=thresholds_plotdata[[1]], 
         aes(x=Class, y=Threshold_value))+
    # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
    
    geom_point(aes(fill=Approach4, shape=Index, 
                   size=Flagged),
               position=position_dodge(width=.5))+
    # stat_summary(data=. %>% filter(!Flagged), fun.y="mean", shape=8, size=.5)+
    stat_summary(data=. %>% filter(!Flagged), fun.y="mean", geom="crossbar", size=.25)+
    geom_hline(data=thresholds_plotdata[[2]], aes(yintercept=Threshold_mean), linetype="dashed", size=.5)+
    # stat_summary( fun.y="mean", shape=10)+
    facet_wrap(~Indicator, scales="free", ncol=2)+
    scale_shape_manual(values=c(24,25,22, 21), name="Response model index")+
    scale_fill_manual(values=c("#e41a1c","#377eb8","#33a02c","#b2df8a"), name="Approach")+
    scale_size_manual(values=c(2,1), name="Flagged?", labels=c("No","Yes (excluded from means)"))+
    theme_bw()+
    coord_flip()+
    guides(shape=guide_legend(override.aes = list(fill="gray", size=2), order=2),
           fill=guide_legend(override.aes = list(shape=21, size=2), order=1),
           size=guide_legend(order=3)
    )+
    theme(legend.position = "bottom",
          legend.direction = "vertical")+
    xlab("")+ylab("")
}

# synthesize_thresholds_plot(classes=c("RFI-N","CVF","HB"))
synthesize_thresholds_plot(classes=c("CVF","SB0"),
                           indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM", "% cover"))
##

synthesize_thresholds_obs_plot<-function(
    classes,
    indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
    CSCI=NA_real_,ASCI_D=NA_real_,ASCI_H=NA_real_, TN=NA_real_, TP=NA_real_, Chla=NA_real_, AFDM=NA_real_, Cover=NA_real_,
    stringency="Intermediate") {
  
  obs_data=tibble(
    Indicator=c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
    Observed_value=c(CSCI, ASCI_D, ASCI_H, TN, TP, Chla, AFDM, Cover)
  ) %>%
    filter(Indicator %in%  indicators) %>%
    mutate(Indicator=factor(Indicator, levels=c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover")))
  
  thresholds_plotdata <- synthesize_thresholds_plotdata(classes=classes, indicators=indicators, stringency = stringency) 
  xdf <- obs_data %>%
    rename(Value=Observed_value) %>%
    mutate(DataType="Observed value") %>%
    bind_rows(
      thresholds_plotdata[[2]] %>%
        rename(Value=Threshold_mean) %>%
        mutate(DataType="Mean of thresholds")
    )
  multiple_thresholds<-thresholds_plotdata[[1]] %>%
    filter(!Flagged) %>%
    mutate(ClassIndicator = paste0(Class, Indicator)) %>%
    group_by(ClassIndicator) %>%
    tally() %>%
    ungroup()%>%
    filter(n>1) 
  
  ydf<-thresholds_plotdata[[1]] %>%
    mutate(ClassIndicator = paste0(Class, Indicator))
  ggplot(data=ydf, 
         aes(x=Class, y=Threshold_value))+
    # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
    
    geom_point(aes(fill=Approach4, shape=Index, 
                   size=Flagged),
               position=position_dodge(width=0))+
    stat_summary(data=. %>% 
                   filter(!Flagged) %>%
                   filter(ClassIndicator %in% multiple_thresholds$ClassIndicator), fun.y="mean", geom="crossbar", size=.25)+
    # geom_hline(data=obs_data, aes(yintercept=Observed_value, color="Observed value"), linetype="dashed", size=.75)+scale_color_manual(values=c("violet"), name="")+
    # geom_hline(data=thresholds_plotdata[[2]], aes(yintercept=Threshold_mean), linetype="solid")+
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


#Elder
test_plot<-synthesize_thresholds_obs_plot(classes=c("CVF","SB0"),
                                          indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM", "% cover"),
                                          CSCI=0.39, ASCI_D=0.97, ASCI_H=1.02, TN=0.79, TP=1.03, Chla=13.26, AFDM=9.2)

# library(cowplot)
# cowplot::plot_grid(test_plot +
#                      theme(legend.position = "none"),
#                    cowplot::get_legend(test_plot),
#                    nrow=2, rel_heights=c(1,.25),
#                    hjust=0)

ggsave(test_plot, filename="Figures/assessment_plot_alt_ELDER.jpg",
       dpi=300, height=8, width=7.75)

#Magpie
test_plot<-synthesize_thresholds_obs_plot(classes=c("CVF","HB"),
                                          indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM", "% cover"),
                                          CSCI=0.21, ASCI_D=0.92, ASCI_H=.82, TN=.64, TP=.2, Chla=154, AFDM=83, Cover=41)

# library(cowplot)
# cowplot::plot_grid(test_plot +
#                      theme(legend.position = "none"),
#                    cowplot::get_legend(test_plot),
#                    nrow=2, rel_heights=c(1,.25),
#                    hjust=0)

ggsave(test_plot, filename="Figures/assessment_plot_alt_MAGPIE.jpg",
       dpi=300, height=8, width=7.75)

#Pine
test_plot<-synthesize_thresholds_obs_plot(classes=c("CVF","RFI-N"),
                                          indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM", "% cover"),
                                          CSCI=0.83, ASCI_D=0.9, ASCI_H=.94, TN=.07, TP=.013, Chla=33.3, AFDM=11, 
                                          stringency = "High")

# library(cowplot)
# cowplot::plot_grid(test_plot +
#                      theme(legend.position = "none"),
#                    cowplot::get_legend(test_plot),
#                    nrow=2, rel_heights=c(1,.25),
#                    hjust=0)

ggsave(test_plot, filename="Figures/assessment_plot_alt_PINE.jpg",
       dpi=300, height=8, width=7.75)

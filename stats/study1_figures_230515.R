#-----------------------------------SETUP------------------------------

#Study 1 Figures
#HCP, HCPD, and NSD datasets
#Written by M. Peterson under MIT License 2023
#Nielsen Brain and Behavior Lab

#load libraries
library(mosaic)
library(dplyr)
library(ggpubr)
library(broom)
library(tidyverse)
library(rstatix)
library(psych) #ICC, sphericity, KMO
library(circlize) #chord diagrams
library(readxl)
library(gridExtra) #grid of scatterplots
library(reshape)
library(MatchIt) #Creation of HCP DISC and REP datasets
library(gghalves) #raincloud plots
#library(ppcor) #partial correlation
#library(mvnTest) #Doornik-Hansen test for multivariate normality
#library(car) #VIF test for multicollinearity. Masks 'recode' function.
#library(lavaan) #CFA
library(CCA) #Canonical correlation analysis
library(CCP) #Canonical correlation analysis dimensions
#-----------------------------------ALL DEMOS---------------------------------
#Load ALL .csv
study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")

#Formatting for Demos
study1_demos <- subset(study1, NewNetwork=="1")

#FIGURES    
    #1. Age raincloud plot (points to left of boxes)
    #Filter NSD to just one set (both sets=duplication)
    study1_demos_age <- subset(study1_demos, dataset!="TASK")
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
    ggplot(study1_demos_age, aes(x = dataset, y = Age_in_Yrs, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .1
        )
      )+
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(11, 36)), clip = "off")+
      labs(y="Age (Years)", x="")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = .6), axis.text.x =element_text(colour = "black", hjust = 0.5))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AgeRain_230630.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #2. FD_AVG Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = FD_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0.037, .17)), clip = "off")+
      labs(y="Mean FD (mm)", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", hjust = 0.6, angle=90), axis.text.x =element_text(colour = "black", angle=20, hjust = 1, vjust=1))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AvgFD_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    #3. MEAN FD x AGE
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x=Age_in_Yrs, y=FD_avg, color=dataset))+
      labs(x = 'Age (Years)', y = 'Mean FD (mm)')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=Palette,  labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette,  labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      geom_point(aes(fill=dataset), colour="black", pch=21)+
      scale_y_continuous(limits = c(0.03, 0.2))+
      geom_smooth(method=lm, aes(fill=dataset), se=TRUE) +
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position="none", legend.title=element_blank(), legend.text=element_text(colour = "black", size = 10), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_Demos_AvgFDxAge_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #4. DVARS_avg Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = DVARS_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(25, 50)), clip = "off")+
      labs(y="Mean DVARS", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", angle=20, hjust = 0.6, vjust=.9))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AvgDVARS_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #5. Total volumes Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = Sum_Volumes, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(949, 4750)), clip = "off")+
      labs(y="Total Volumes Available", x=" ")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=0, hjust = 0.5, vjust=.9))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_TotVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    #6. Percent available volumes by dataset (raincloud)
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = Percent_Vols, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(50, 100)), clip = "off")+
      labs(y="% Volumes Available", x="")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=20, hjust = 1, vjust=1))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_PercentVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)


#FIGURES: POINTS TO LEFT
    #1. Age raincloud plot (points to left of boxes)
    #Filter NSD to just one set (both sets=duplication)
    study1_demos_age <- subset(study1_demos, dataset!="TASK")
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
    ggplot(study1_demos_age, aes(x = dataset, y = Age_in_Yrs, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.9, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA,
        position= position_nudge(x=.3)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .2
        )
      )+
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(11, 36)), clip = "off")+
      labs(y="Age (Years)", x="")+
      annotate("segment", x = 1.3, xend = 1.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 2.3, xend = 2.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 3.3, xend = 3.3, y = 9.3, yend = 9.7,
               colour = "black")+
      annotate("segment", x = 4.3, xend = 4.3, y = 9.3, yend = 9.7,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = .6), axis.text.x =element_text(colour = "black", hjust = 0.1))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AgeRain_230630.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    #2. FD_AVG Raincloud Plots by dataset
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = FD_avg, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.75, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .2, 
        outlier.shape = NA,
        position= position_nudge(x=.25)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .08, dodge.width=0
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0.037, .17)), clip = "off")+
      labs(y="Mean FD (mm)", x=" ")+
      annotate("segment", x = 1.25, xend = 1.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 2.25, xend = 2.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 3.25, xend = 3.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 4.25, xend = 4.25, y = 0.027, yend = 0.03,
               colour = "black")+
      annotate("segment", x = 5.25, xend = 5.25, y = 0.027, yend = 0.03,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", hjust = 0.6, angle=90), axis.text.x =element_text(colour = "black", angle=20, hjust = 0.3, vjust=.45))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_AvgFD_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    
    #6. Percent available volumes by dataset (raincloud)
    Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#44AA99")
    ggplot(study1_demos, aes(x = dataset, y = Percent_Vols, fill=dataset)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.8, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA, 
        position= position_nudge(x=.28)
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitterdodge(
          seed = 1, jitter.width = .2, dodge.width=0
        )
      ) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(50, 100)), clip = "off")+
      labs(y="% Volumes Available", x="")+
      annotate("segment", x = 1.3, xend = 1.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 2.3, xend = 2.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 3.3, xend = 3.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 4.3, xend = 4.3, y = 46.5, yend = 47.5,
               colour = "black")+
      annotate("segment", x = 5.3, xend = 5.3, y = 46.5, yend = 47.5,
               colour = "black")+
      scale_colour_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+
      scale_fill_manual(values=Palette, labels = c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task"))+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      scale_x_discrete(labels=c("HCP-Disc", "HCP-Rep", "HCPD", "NSD-Rest", "NSD-Task")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black",angle=20, hjust = .5, vjust=.7))+
      theme(panel.background = element_blank())+
      theme(axis.ticks = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    #save the file
    ggsave(filename = paste("Study1_Demos_PercentVols_230620.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
#------------------------------------NSD DEMOS----------------------------------------    
  #Load datasets
    DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/NSD_analysis/NSD_BIDS/participants.csv")
    FD_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/FD_avg_NSD_REST_230515.csv")
    FD_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/FD_avg_NSD_TASK_230515.csv")
    DVARS_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/DVARS_avg_NSD_REST_230515.csv")
    DVARS_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/DVARS_avg_NSD_TASK_230515.csv")
    TOTAL_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/RemainingVols_NSD_REST_230515.csv")
    TOTAL_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/RemainingVols_NSD_TASK_230515.csv")
    
    #Format data
    names(FD_REST)[1] <- "SUBJID"
    names(FD_REST)[2] <- "avg_FD_REST"
    names(FD_TASK)[1] <- "SUBJID"
    names(FD_TASK)[2] <- "avg_FD_TASK"
    names(DVARS_REST)[1] <- "SUBJID"
    names(DVARS_REST)[2] <- "avg_DVARS_REST"
    names(DVARS_TASK)[1] <- "SUBJID"
    names(DVARS_TASK)[2] <- "avg_DVARS_TASK"
    names(TOTAL_REST)[1] <- "SUBJID"
    names(TOTAL_REST)[2] <- "VOLS_REST"
    names(TOTAL_TASK)[1] <- "SUBJID"
    names(TOTAL_TASK)[2] <- "VOLS_TASK"
    
    #Merge datasets
    comb1 <- merge(DEMOS, FD_REST, by =c("SUBJID"), all=FALSE)
    comb2 <- merge(comb1, FD_TASK, by =c("SUBJID"), all=FALSE)
    comb3 <- merge(comb2, DVARS_REST, by =c("SUBJID"), all=FALSE)
    comb4 <- merge(comb3, DVARS_TASK, by =c("SUBJID"), all=FALSE)
    comb5 <- merge(comb4, TOTAL_REST, by =c("SUBJID"), all=FALSE)
    full_df <- merge(comb5, TOTAL_TASK, by =c("SUBJID"), all=FALSE)
    
    #Long format (for plotting)
    NSD_LONG <- full_df %>%
      pivot_longer(cols = -c(SUBJID, age, sex),
                   names_to = c(".value", "data"),
                   names_pattern = "^(avg_DVARS|avg_FD|VOLS)_(REST|TASK)$"
      )
    
    
#PLOTS
    #SET 1: FD, DVARS, TOT VOLS 
    #Quality of data is comparable across task types
    #Fig. 1a: avg FD by task
    ggplot(NSD_LONG, aes(x=data, y=avg_FD))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Mean FD (mm)')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=3)+
      #scale_y_continuous(limits = c(0, .20), expand=c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Fig1a_NSD_Demos_AvgFD_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    #Fig. 1b: avg DVARS by task
    ggplot(NSD_LONG, aes(x=data, y=avg_DVARS))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Mean DVARS')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=3)+
      #scale_y_continuous(limits =c(20,40), expand=c(0,0))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Fig1b_NSD_Demos_AvgDVARS_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    #Fig. 1c: total number of volumes by task
    ggplot(NSD_LONG, aes(x=data, y=VOLS))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Total Volumes')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=3)+
      #scale_y_continuous(limits=c(1500,2210))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Fig1c_NSD_Demos_TotVols_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    
    #Fig. 1d: Total number of volumes by FD
    ggplot(NSD_LONG, aes(x=avg_FD, y=VOLS, fill=SUBJID))+
      geom_line(aes(group=SUBJID))+
      labs(x = 'Mean FD (mm)', y = 'Total Volumes')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=3)+
      #scale_y_continuous(limits=c(1500,2210))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Fig1d_NSD_Demos_TotVolsxFD_230520.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    
#---------------------------------FREQ STATS-----------------------------
#SETUP
    #Load ALL .csv
    study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230620.csv")
    
    #Formatting for Demos
    study1_demos <- subset(study1, NewNetwork=="1")
    
  #HCP-DISC    
    HCPDISC_demos <- subset(study1_demos, dataset=="HCP-DISC")    
    
    #AGE
    favstats(HCPDISC_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPDISC_demos$Sex_Bin)
    #Avg FD
    favstats(HCPDISC_demos$FD_avg)
    #%VOLS
    favstats(HCPDISC_demos$Percent_Vols)
    
  #HCP-REP    
    HCPREP_demos <- subset(study1_demos, dataset=="HCP-REP")    
    
    #AGE
    favstats(HCPREP_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPREP_demos$Sex_Bin)
    #Avg FD
    favstats(HCPREP_demos$FD_avg)
    #%VOLS
    favstats(HCPREP_demos$Percent_Vols)
    
  #HCPD    
    HCPD_demos <- subset(study1_demos, dataset=="HCPD")    
    
    #AGE
    favstats(HCPD_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(HCPD_demos$Sex_Bin)
    #Avg FD
    favstats(HCPD_demos$FD_avg)
    #%VOLS
    favstats(HCPD_demos$Percent_Vols)
    
  #NSD
    #REST
    NSDR_demos <- subset(study1_demos, dataset=="REST")    
    #AGE
    favstats(NSDR_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(NSDR_demos$Sex_Bin)
    #Avg FD
    favstats(NSDR_demos$FD_avg)
    #%VOLS
    favstats(NSDR_demos$Percent_Vols)
    
    #TASK
    NSDT_demos <- subset(study1_demos, dataset=="TASK")    
    #AGE
    favstats(NSDT_demos$Age_in_Yrs)
    #SEX (0=M, 1=F)
    table(NSDT_demos$Sex_Bin)
    #Avg FD
    favstats(NSDT_demos$FD_avg)
    #%VOLS
    favstats(NSDT_demos$Percent_Vols)
    

#------------------------------------DEVELOPMENT ANALYSIS: HCPD----------------------------
    #SETUP: 1. Run ALL DEMOS data code    
    #Load specialization data (SAI/NSAR)
    HCPD_SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/network_sa/REST/NETWORK_SA_SUB_NET_LH_RH_230214.csv")
    HCPD_SAI$SUBJID <- gsub("^.{0,4}", "", HCPD_SAI$SUBJID)
    HCPD_SAI$Network <- gsub("^.{0,8}", "", HCPD_SAI$NETWORK)
    
    #Drop Network 0 (medial wall)
    HCPD_SAI <- subset(HCPD_SAI, Network!=0)
    #Reorder CBIG Networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    HCPD_SAI$NewNetwork <- recode(HCPD_SAI$Network, !!!mapping)
    
    #Create SA LAT variable
    HCPD_SAI$SA_LAT <- (HCPD_SAI$RH_SA - HCPD_SAI$LH_SA) / (HCPD_SAI$LH_SA + HCPD_SAI$RH_SA)
    
    #Merge in SAI with 200, demos
    HCPD_NSAR <- merge(HCPD_data, HCPD_SAI, by =c("SUBJID"), all=FALSE)
    
    
    
    #1. Age effects on specialization? Bonferroni correction = .001
    #Center variables
    HCPD_NSAR$Age_Center <- HCPD_NSAR$Age_in_Yrs - (mean(HCPD_NSAR$Age_in_Yrs))
    HCPD_NSAR$FD_Center <- HCPD_NSAR$FD_avg - (mean(HCPD_NSAR$FD_avg))
    HCPD_NSAR$Sex_Bin <- ifelse(HCPD_NSAR$sex == "M", 0, 1) #recode males (1) as 0 and Females (2) as 1
    
    for (i in 1:17) {
      # Subset the data based on NewNetwork value
      subset_data <- subset(HCPD_NSAR, NewNetwork == i)
      
      # Fit the linear regression model
      subset_data$Sex_Bin <- as.factor(subset_data$Sex_Bin)
      subset_data$Age_Center <- as.numeric(subset_data$Age_Center)
      subset_data$SA_LAT <- as.numeric(subset_data$SA_LAT)
      subset_data$FD_Center <- as.numeric(subset_data$FD_Center)
      
      model <- lm(SA_LAT ~ Age_Center + Sex_Bin + FD_Center, data = subset_data)
      # Create a unique name for each model
      model_name <- paste("HCPD_SA_model", i, sep = "")
      
      # Assign the model to the unique name
      assign(model_name, model)
    }
    
    #Access model results through: summary(HCPD_SA_model1)
    
    
    
    
    #2. Control-C (12) Models centered at different ages: 5, 10, 15, 20      
    #Age5 Model
    HCPD_NSAR$Age_Center5 <- as.numeric(HCPD_NSAR$Age_in_Yrs - 5)
    HCPD_NSAR$FD_Center <- as.numeric(HCPD_NSAR$FD_avg - (mean(HCPD_NSAR$FD_avg)))
    HCPD_NSAR$Sex_Bin <- ifelse(HCPD_NSAR$sex == "M", 0, 1) #recode males (1) as 0 and Females (2) as 1
    subset_data <- subset(HCPD_NSAR, NewNetwork==12)
    model5 <- lm(SA_LAT ~ Age_Center5 + Sex_Bin + FD_Center, data = subset_data)
    summary(model5)
    
    #Age10 Model
    HCPD_NSAR$Age_Center10 <- HCPD_NSAR$Age_in_Yrs - 10
    HCPD_NSAR$FD_Center <- HCPD_NSAR$FD_avg - (mean(HCPD_NSAR$FD_avg))
    HCPD_NSAR$Sex_Bin <- ifelse(HCPD_NSAR$sex == "M", 0, 1) #recode males (1) as 0 and Females (2) as 1
    subset_data <- subset(HCPD_NSAR, NewNetwork==12)
    model10 <- lm(SA_LAT ~ Age_Center10 + Sex_Bin + FD_Center, data = subset_data)
    summary(model10)
    
    #Age15 Model
    HCPD_NSAR$Age_Center15 <- HCPD_NSAR$Age_in_Yrs - 15
    HCPD_NSAR$FD_Center <- HCPD_NSAR$FD_avg - (mean(HCPD_NSAR$FD_avg))
    HCPD_NSAR$Sex_Bin <- ifelse(HCPD_NSAR$sex == "M", 0, 1) #recode males (1) as 0 and Females (2) as 1
    subset_data <- subset(HCPD_NSAR, NewNetwork==12)
    model15 <- lm(SA_LAT ~ Age_Center15 + Sex_Bin + FD_Center, data = subset_data)
    summary(model15)
    
    #Age20 Model
    HCPD_NSAR$Age_Center20 <- HCPD_NSAR$Age_in_Yrs - 20
    HCPD_NSAR$FD_Center <- HCPD_NSAR$FD_avg - (mean(HCPD_NSAR$FD_avg))
    HCPD_NSAR$Sex_Bin <- ifelse(HCPD_NSAR$sex == "M", 0, 1) #recode males (1) as 0 and Females (2) as 1
    subset_data <- subset(HCPD_NSAR, NewNetwork==12)
    model20 <- lm(SA_LAT ~ Age_Center20 + Sex_Bin + FD_Center, data = subset_data)
    summary(model20)
    
    
    
#-----------------------------------DEVELOPMENT FIGURES: HCPD------------------------------
    #SETUP: 1. Run ALL DEMOS data code    
    #Load specialization data (SAI/NSAR)
    HCPD_SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/network_sa/REST/NETWORK_SA_SUB_NET_LH_RH_230214.csv")
    HCPD_SAI$SUBJID <- gsub("^.{0,4}", "", HCPD_SAI$SUBJID)
    HCPD_SAI$Network <- gsub("^.{0,8}", "", HCPD_SAI$NETWORK)
    
    #Drop Network 0 (medial wall)
    HCPD_SAI <- subset(HCPD_SAI, Network!=0)
    #Reorder CBIG Networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    HCPD_SAI$NewNetwork <- recode(HCPD_SAI$Network, !!!mapping)
    
    #Create SA LAT variable
    HCPD_SAI$SA_LAT <- (HCPD_SAI$RH_SA - HCPD_SAI$LH_SA) / (HCPD_SAI$LH_SA + HCPD_SAI$RH_SA)
    
    #Merge in SAI with 200, demos
    HCPD_NSAR <- merge(HCPD_data, HCPD_SAI, by =c("SUBJID"), all=FALSE)
    
    
    #FIGURES
    #Fig.1: Specialized networks line-up (model-adjusted values) BOXPLOT
    
    #Step1: Model-adjusted values
    HCPD_NSAR$SA_LAT_ADJ <- NA
    ci_df <- data.frame(sex=factor(),
                        NewNetwork=factor(),
                        CI_MIN=integer(),
                        CI_MAX=integer(),
                        MEAN=integer())
    for (i in 1:17) {
      # Subset the data based on NewNetwork value
      subset_data <- subset(HCPD_NSAR, NewNetwork == i)
      
      subset_data <- transform(subset_data,
                               Age_Center = Age_in_Yrs - mean(Age_in_Yrs),
                               FD_Center = FD_avg - mean(FD_avg),
                               Sex_Bin = factor(ifelse(sex == "M", 0, 1)))
      
      # Fit the linear regression model
      model <- lm(SA_LAT ~ Age_Center + Sex_Bin + FD_Center, data = subset_data)
      
      #Grab lm coefficients
      BETA_AGE <- model[["coefficients"]][["Age_Center"]]
      BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
      BETA_FD <- model[["coefficients"]][["FD_Center"]]
      
      #Grab means
      MEAN_AGE <- mean(subset_data$Age_Center)
      MEAN_FD <- mean(subset_data$FD_Center)
      MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
      
      #IDs of the subset
      subsetted_ids <- subset_data$SUBJID
      
      #Find matching rows
      matching_rows <- HCPD_NSAR$SUBJID %in% subsetted_ids &
        HCPD_NSAR$NewNetwork %in% i
      
      #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
      HCPD_NSAR$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) )
      
      #Adjust scores based on sex separately to plot sex differences
      for (loop in 0:1){
        if (loop==0){
          subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) )
        } else {
          subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Center - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Center - MEAN_FD)) )
        }  
        #Find confidence intervals for intercept based on the model
        #find mean
        MEAN <- mean(subset_data$SA_LAT_ADJ)
        n <- length(subset_data$SA_LAT_ADJ)
        std_dev <- sd(subset_data$SA_LAT_ADJ)
        std_err <- std_dev / sqrt(n)
        alpha = 0.05
        degrees_of_freedom <- n - 1
        t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
        margin_error <- t_score * std_err
        
        #lower bound
        CI_MIN <- MEAN - margin_error
        #upper bound
        CI_MAX <- MEAN + margin_error
        #Append CI data to dataframe
        row_df <- data.frame(loop, i, CI_MIN, CI_MAX, MEAN)
        names(row_df) <- c("sex", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
        ci_df <- rbind(ci_df, row_df) 
        
      }
    }
    
    
    
    
    
    
    #Step2: Create the Boxplot
    # specify the order of the networks
    #network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
    network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
    # use factor() to set the order of the factor levels
    HCPD_NSAR$NewNetwork <- factor(HCPD_NSAR$NewNetwork, level = network_order)
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    rearranged_palette <- CBIG_Palette[as.integer(network_order)]
    # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
    ggplot(HCPD_NSAR, aes(x = SA_LAT_ADJ, y = NewNetwork, fill = NewNetwork)) + 
      geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
      coord_cartesian(xlim = c(-1,1), ylim = c(1.2, NA), clip = "off") +
      labs(y = "", x = "") +
      #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
      geom_vline(xintercept=0, linetype = "dotted", color = "black") +
      scale_y_discrete(labels=c('Dorsal Attn-A', 'Default-C', 'Language', 'Default-A', 'Control-A', 'Sal/VenAttn-B', 'Dorsal Attn-B', 'Somatomotor-B', 'Default-B', 'Limbic-A', 'Somatomotor-A', 'Visual-A', 'Sal/VenAttn-A', 'Control-C', 'Visual-B', 'Control-B', 'Limbic-B'))+
      scale_colour_manual(values = rearranged_palette) +
      scale_fill_manual(values = rearranged_palette) + 
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none",
        axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      )
    ggsave(filename = paste("Study2_HCPD_NSAR_Boxplots_230606.png"), width = 3.5, height = 6,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
    
    
    
    #Step2: Line and Point plot
    # Create the scatter plot
    GroupPalette <- c("#CC6677", "#DDCC77")
    network_order <- c('6', '15', '5', '13', '10', '9', '7', '4', '14', '16', '3', '1', '8', '12', '2', '11', '17')
    ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
    group_order <- c("0", "1")
    ci_df$sex <- factor(ci_df$sex, level=group_order)
    ggplot(ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(sex, NewNetwork), fill=as.factor(sex))) +
      geom_errorbarh(aes(xmin=CI_MIN, xmax = CI_MAX), height = 0,  position = position_dodge(width = .3), color="black", size=.5) +
      geom_point(position = position_dodge(width = .3), size=2, shape=21) +
      coord_cartesian(ylim= c(1.2, NA), x=c(-0.3,.35), clip = "off") +
      labs(y = "", x = "") +
      geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
      scale_y_discrete(labels=c('Dorsal Attn-A', 'Default-C', 'Language', 'Default-A', 'Control-A', 'Sal/VenAttn-B', 'Dorsal Attn-B', 'Somatomotor-B', 'Default-B', 'Limbic-A', 'Somatomotor-A', 'Visual-A', 'Sal/VenAttn-A', 'Control-C', 'Visual-B', 'Control-B', 'Limbic-B'))+
      scale_fill_manual(values = GroupPalette) + 
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none",
        axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
        axis.text.x = element_text(colour = "black", vjust=1),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      )
    ggsave(filename = paste("Study2_HCPD_NSAR_Sex_PointLineAdjusted_230607.png"), width = 3.35, height = 6,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
    
    
    
    #Fig. 2A: % LH SA Boxplot in HCPD (13 specialized networks)
    # create % surface area per hemisphere (total SA: 126300.7)
    HCPD_NSAR$LH_SA_PERCENT <- (HCPD_NSAR$LH_SA/63103.74)*100
    HCPD_NSAR$RH_SA_PERCENT <- (HCPD_NSAR$RH_SA/63196.98)*100
    
    #Just include specialized networks
    network_order <- c('1', '2', '4', '5', '6', '8','9', '10', '11', '12', '13', '15', '17')
    HCPD_NSAR_13 <- HCPD_NSAR[HCPD_NSAR$NewNetwork %in% network_order, ]
    
    # use factor() to set the order of the factor levels
    HCPD_NSAR_13$NewNetwork <- factor(HCPD_NSAR_13$NewNetwork, level = network_order)
    CBIG_Palette <- c("#602368", "#DC1F26", "#21B88B", "#32489E", "#4A9644", "#106A36", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#1C1B55", "#BCCF7E")
    # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
    ggplot(HCPD_NSAR_13, aes(x = NewNetwork, y = LH_SA_PERCENT, fill = NewNetwork)) + 
      geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
      coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 17.1), clip = "off") +
      labs(y = "% LH Surface Area", x = "") +
      scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-B", "Language", "Dorsal Attn-A", "Sal/VenAttn-A", "Sal/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-C", "Limbic-B")) +
      scale_colour_manual(values = CBIG_Palette) +
      scale_fill_manual(values = CBIG_Palette) + 
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none",
        axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
        axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      )
    ggsave(filename = paste("Study2_HCPD_SA_LH_Boxplots_230607.png"), width = 6.9, height = 2.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
    
    
    #Fig. 2B: % RH SA Boxplot in HCPD (13 specialized networks)
    #Just include specialized networks
    network_order <- c('1', '2', '4', '5', '6', '8','9', '10', '11', '12', '13', '15', '17')
    HCPD_NSAR_13 <- HCPD_NSAR[HCPD_NSAR$NewNetwork %in% network_order, ]
    # use factor() to set the order of the factor levels
    HCPD_NSAR_13$NewNetwork <- factor(HCPD_NSAR_13$NewNetwork, level = network_order)
    CBIG_Palette <- c("#602368", "#DC1F26", "#21B88B", "#32489E", "#4A9644", "#106A36", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#1C1B55", "#BCCF7E")
    ggplot(HCPD_NSAR_13, aes(x = NewNetwork, y = RH_SA_PERCENT, fill = NewNetwork)) + 
      geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
      coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 15.2), clip = "off") +
      labs(y = "% RH Surface Area", x = "") +
      scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-B", "Language", "Dorsal Attn-A", "Sal/VenAttn-A", "Sal/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-C", "Limbic-B")) +
      scale_colour_manual(values = CBIG_Palette) +
      scale_fill_manual(values = CBIG_Palette) + 
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none",
        axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
        axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 20),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      )
    ggsave(filename = paste("Study2_HCPD_SA_RH_Boxplots_230607.png"), width = 6.9, height = 2.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
    
    
    #Fig. 2C: MEAN % SA for LH and RH. With X-axis MSHBM Labels. Boxplots
    #Mean percent
    LH_MEAN <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = HCPD_NSAR_13, FUN = mean)
    RH_MEAN <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = HCPD_NSAR_13, FUN = mean)
    
    names(LH_MEAN)[3] <- "MEAN_LH_PERCENT"
    names(RH_MEAN)[3] <- "MEAN_RH_PERCENT"
    mean_df <- merge(LH_MEAN, RH_MEAN, by=c("dataset", "NewNetwork"), all=TRUE)
    
    MEAN_LONG <- mean_df %>%
      pivot_longer(cols = starts_with("MEAN_"),
                   names_to = c("HEMI"),
                   names_pattern = "MEAN_(.*)")
    names(MEAN_LONG)[4] <- "MEAN_PERCENT"
    
    #std. error
    std <- function(x) sd(x)/sqrt(length(x))
    LH_STD <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = HCPD_NSAR_13, FUN = std)
    RH_STD <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = HCPD_NSAR_13, FUN = std)
    
    names(LH_STD)[3] <- "SE_LH_PERCENT"
    names(RH_STD)[3] <- "SE_RH_PERCENT"
    se_df <- merge(LH_STD, RH_STD, by=c("dataset", "NewNetwork"), all=TRUE)
    SE_LONG <- se_df %>%
      pivot_longer(cols = starts_with("SE_"),
                   names_to = c("HEMI"),
                   names_pattern = "SE_(.*)")
    names(SE_LONG)[4] <- "SE_PERCENT"
    
    SA_df <- merge(SE_LONG, MEAN_LONG, by=c("dataset", "NewNetwork", "HEMI"), all=TRUE)
    #combine dataset and hemi into one var
    SA_df$data_hemi = as.factor(paste0(SA_df$dataset,SA_df$HEMI))
    
    # specify the order of the networks
    network_order <- c('1', '2', '4', '5', '6', '8','9', '10', '11', '12', '13', '15', '17')
    SA_df <- SA_df[SA_df$NewNetwork %in% network_order, ]
    #Order networks numerically
    # use factor() to set the order of the factor levels
    SA_df$NewNetwork <- factor(SA_df$NewNetwork, level = network_order)
    #Order groups manually
    group_order <- c('HCPDLH_PERCENT', "HCPDRH_PERCENT")
    SA_df$data_hemi <- factor(SA_df$data_hemi, levels=group_order)
    GroupPalette <- c("#0072B2","#56B4E9")
    ggplot(SA_df, aes(x = NewNetwork, y = MEAN_PERCENT, group=interaction(data_hemi, NewNetwork), fill = data_hemi)) +
      geom_bar(stat = "identity", position=position_dodge(width = .8)) +
      geom_errorbar(aes(ymin=MEAN_PERCENT-SE_PERCENT, ymax = MEAN_PERCENT+SE_PERCENT), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
      labs(x = "", y = "Mean % Surface Area") +
      scale_colour_manual(values = GroupPalette) +
      scale_fill_manual(values = GroupPalette) + 
      scale_y_continuous(expand=c(0,0), limits=c(0,11.0))+
      scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-B", "Language", "Dorsal Attn-A", "Sal/VenAttn-A", "Sal/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-C", "Limbic-B")) +
      #theme_bw()
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none",
        axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
        axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 15),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      )
    ggsave(filename = paste("Study2_HCPD_LHRH_SA_Percent_13N_Boxplots_230607.png"), width = 6.9, height = 2.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/", dpi = 300)
    
    
    
    #Fig.3: 17 scatterplots showing trajectories of NSAR over age
    Palette <- c("#DDCC77", "#CC6677")
    #CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    
    # Convert necessary columns to appropriate data types
    HCPD_NSAR$SA_LAT <- as.numeric(as.character(HCPD_NSAR$SA_LAT))
    HCPD_NSAR$Age_in_Yrs <- as.numeric(HCPD_NSAR$Age_in_Yrs)
    HCPD_NSAR$NewNetwork <- as.factor(HCPD_NSAR$NewNetwork)
    
    #Network labels
    labels <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attn-A", "Dorsal Attn-B", "Sal/VenAttn-A", "Sal/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
    
    # Create a for loop to generate ggplots
    for (loop in 1:17) {
      # Subset the dataset based on NewNetwork == loop
      subset_data <- subset(HCPD_NSAR, NewNetwork == loop)
      
      # Create ggplot
      p <- ggplot(subset_data, aes(x = Age_in_Yrs, y = SA_LAT, color = sex)) +
        labs(x = 'Age (Years)', y = "NSAR") +
        ggtitle(labels[loop])+
        labs(fill = " ") +
        labs(color = " ") +
        scale_colour_manual(values = Palette) +
        scale_fill_manual(values = Palette, labels = c("")) +
        geom_point(aes(fill = sex), colour = "black", pch = 21) +
        #scale_y_continuous(limits = c(-1, 1)) +
        geom_smooth(method = lm, aes(fill = sex), se = TRUE) +
        guides(color = guide_legend(override.aes = list(fill = NA))) +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -0.1),
          axis.title = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black"),
          legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(colour = "black", size = 10),
          legend.background = element_rect(fill = "white", size = 0.5),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
          panel.border = element_blank(),
          panel.background = element_blank()
        )
      
      # Save the ggplot as an PNG file
      filename <- paste("Study2_HCPD_SAINetwork_Scatter_", loop, "_230606.png", sep = "")
      ggsave(
        filename = filename,
        plot = p,
        width = 2.25,
        height = 2.25,
        path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study2_figures/png_figures/",
        dpi = 300
      )
    }
    
#-----------------------------------------LAN-A-----------------------------
  #SUMMARY: Using python/3.6, LanA values were summed within the HCP 232 group parcellation network boundaries.
    
  #SETUP: 
    LANA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/LanA_atlas/sum/HCP_LanA_sum_231003.csv")
  
    #Drop Network 0 (medial wall)
    LANA <- subset(LANA, NETWORK!=0)
    
    #Reorder CBIG Networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    LANA$NewNetwork <- recode(LANA$NETWORK, !!!mapping)
    
  #BARPLOT    
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    LANA$NewNetwork <- as.factor(as.character(LANA$NewNetwork))
    network_order <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
    LANA$NewNetwork <- factor(LANA$NewNetwork, level = network_order)
    ggplot(LANA, aes(x = NewNetwork, y = Sum, fill=NewNetwork)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "LanA Sum")+
      scale_fill_manual(values = CBIG_Palette) + 
      scale_x_discrete(labels=c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B"))+
    theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -0.1),
        axis.title = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
        axis.text.x = element_text(colour = "black", angle = 35, vjust = 0.9, hjust = .8),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 10),
        legend.background = element_rect(fill = "white", size = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
        panel.border = element_blank(),
        panel.background = element_blank()
      )
    
    
    
    
    
#-------------------------------------HCP LANG LAT ZSTAT---------------------------------
    #Load SAI, zstat data
    ZSTAT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/lang_lat/zstats/MSHBM_LONG_AVG_ZSTAT_HCP_230318.csv")
    SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/ALL/HCP_ALL_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    
    #Format data
    SAI$SUBJID <- gsub("^.{0,4}", "", SAI$SUBJID)
    SAI$Network <- gsub("^.{0,8}", "", SAI$NETWORK)
   
    #Drop Network 0 (medial wall)
    SAI <- subset(SAI, Network!=0)
    ZSTAT <- subset(ZSTAT, Network==5)
    
    #Reorder CBIG Networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    SAI$NewNetwork <- recode(SAI$Network, !!!mapping)
    ZSTAT$NewNetwork <- recode(ZSTAT$Network, !!!mapping)
    
    #Subset to network 5
    ZSTAT <- subset(ZSTAT, NewNetwork==5)
    SAI <- subset(SAI, NewNetwork==5)
    
    #Create SAI variable
    SAI$SA_LAT <- (SAI$LH_SA - SAI$RH_SA) / (SAI$LH_SA + SAI$RH_SA)
    
    #Create ZSTAT asymmetry index variable
    ZSTAT$ZAI <- (ZSTAT$LH_AVG_Z - ZSTAT$RH_AVG_Z) / (ZSTAT$LH_AVG_Z + ZSTAT$RH_AVG_Z)
    
    #Merge ZSTAT and SAI datasets
    LANG <- merge(SAI, ZSTAT, by =c("SUBJID"), all=FALSE)
      
#Plot: ZSTAT avg within LH and RH for network 5 x SAI for language network (5)
    #With Spearman coefficient
    ggplot(LANG, aes(x=ZAI, y=SA_LAT))+
      labs(x = "Language Task Laterality", y = 'Language NSAR')+
      labs(fill = " ")+
      labs(color = " ")+
      geom_point(fill="#E69F00", pch=21)+
      geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
      stat_cor(method="spearman")+
      scale_y_continuous(limits = c(-.50, .95))+
      scale_x_continuous(limits = c(-1, 1))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_LangLat_Zstat_SAI_230516.svg"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/", dpi = 300)
    
#Plot: Network-avg ZSTAT x SAI for language network (5)
    #With Spearman coefficient
    ggplot(LANG, aes(x=Network_AVG_Z, y=SA_LAT))+
      labs(x = "Language Task Mean Z-Score", y = 'Language NSAR')+
      labs(fill = " ")+
      labs(color = " ")+
      geom_point(fill="#E69F00", pch=21)+
      geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
      stat_cor(method="spearman")+
      scale_y_continuous(limits = c(-.50, .95))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_LangLat_NetworkAvg_Zstat_SAI_230516.svg"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/", dpi = 300)
    
#Plot: LH_AVG ZSTAT x SAI for language network (5)
    #With Spearman coefficient
    ggplot(LANG, aes(x=LH_AVG_Z, y=SA_LAT))+
      labs(x = "Language Task LH Mean Z-Score", y = 'Language NSAR')+
      labs(fill = " ")+
      labs(color = " ")+
      geom_point(fill="#E69F00", pch=21)+
      geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
      stat_cor(method="spearman")+
      scale_y_continuous(limits = c(-.50, .95))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_LangLat_LHAvg_Zstat_SAI_230516.svg"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/", dpi = 300)


#-------------------------------------HCP LANG LAT TSTAT---------------------------------
#Analysis: 231002, using LanA to mask, then thresholded at top 10% of vertices

#SETUP    
    #Load SAI, tstat data
    TSTAT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/lang_lat/lana_mask/HCP_langlat_lanamask_230929.csv")
    SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/ALL/HCP_ALL_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    
    #Format data
    SAI$SUBJID <- gsub("^.{0,4}", "", SAI$SUBJID)
    SAI$Network <- gsub("^.{0,8}", "", SAI$NETWORK)
    
    #Only include NSAR network 5
    SAI <- subset(SAI, Network==5)

    #Create SAI variable (NSAR)
    SAI$SA_LAT <- (SAI$RH_SA - SAI$LH_SA) / (SAI$LH_SA + SAI$RH_SA)
    
    #Create TSTAT asymmetry index variable
    TSTAT$TLAT <- (TSTAT$RH_VERT - TSTAT$LH_VERT) / (TSTAT$RH_VERT + TSTAT$LH_VERT)
    
    #Merge TSTAT and SAI datasets
    LANG <- merge(SAI, TSTAT, by =c("SUBJID"), all=FALSE)
    
    
#Stat: Spearman rank correlation
    cor.test(LANG$TLAT, LANG$SA_LAT, method = 'spearman')
    
#Plot: Lang task laterality x NSAR for language network (5)
    #With Spearman coefficient
    ggplot(LANG, aes(x=TLAT, y=SA_LAT))+
      labs(x = "Language Task Laterality", y = 'Language NSAR')+
      labs(fill = " ")+
      labs(color = " ")+
      geom_point(fill="#E69F00", pch=21)+
      geom_smooth(color="black", method = "lm", size=.75, se = FALSE)+ 
      #stat_cor(method="spearman")+
      #scale_y_continuous(limits = c(-.50, .95))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=11), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6, size=10), 
            axis.text.x =element_text(colour = "black", size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_LangLat_LanAMask_Tstat_NSAR_231002.svg"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/", dpi = 300)
    
    
    
        
#---------------------------------HCP STABLE EST.: OVERALL PARC------------------------------------------
#SETUP
    #Load files: 1_dice
    DICE5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_5MIN_2SESS_dice_singular_230524.csv")
    DICE10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_10MIN_2SESS_dice_singular_230524.csv")
    DICE15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_15MIN_2SESS_dice_singular_230524.csv")
    DICE20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_20MIN_2SESS_dice_singular_230524.csv")
    DICE25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_25MIN_2SESS_dice_singular_230524.csv")
    DICE30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/1_dice/HCP_30MIN_2SESS_dice_singular_230524.csv")
    
    #Create single dataframe
    single_dice_wide <- data.frame(
      SUBJID = DICE5MIN$SUBJID,
      Dice_5min = DICE5MIN$Dice,
      Dice_10min = DICE10MIN$Dice,
      Dice_15min = DICE15MIN$Dice,
      Dice_20min = DICE20MIN$Dice,
      Dice_25min = DICE25MIN$Dice,
      Dice_30min = DICE30MIN$Dice
    )
    
    #Reshape to long format
    single_dice_long <- single_dice_wide %>% 
      pivot_longer(
        cols = c(starts_with('Dice_')),
        names_to = "TIME", 
        values_to = "Dice"
      )
    
    #Format long-format data
    single_dice_long$TIME <- gsub("^.{0,5}", "", single_dice_long$TIME) #remove "Dice_" prefix
    single_dice_long$TIME <- gsub("...$", "", single_dice_long$TIME) #remove "min" suffix

    
#Fig. 1: Raincloud plot: Single dice x Time    
    single_dice_long$TIME <- as.factor(single_dice_long$TIME)
    single_dice_long$Dice <- as.numeric(single_dice_long$Dice)
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(single_dice_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = Dice, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
      ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1)), clip = "off")+
      labs(y="Subject Dice Coefficient", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_Parc_SubRain_Singular_Dice_230524.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
#MEANS and STDDEVS
    #5MIN
    mean(DICE5MIN$Dice)
    sd(DICE5MIN$Dice)
    #30MIN
    mean(DICE30MIN$Dice)
    sd(DICE5MIN$Dice)
    

#---------------------------------HCP STABLE EST.: NETWORK PARC------------------------------------------

#SETUP    
    #Load files: network_dice
    DICEALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_ITERATION_2SESS_ALL_dice_LONG_230308.csv")
    DICEALL$ITERATION = substr(DICEALL$ITERATION,1,nchar(DICEALL$ITERATION)-3)
    
    DICE5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_5MIN_2SESS_dice_LONG_230228.csv")
    DICE10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_10MIN_2SESS_dice_LONG_230228.csv")
    DICE15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_15MIN_2SESS_dice_LONG_230228.csv")
    DICE20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_20MIN_2SESS_dice_LONG_230228.csv")
    DICE25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_25MIN_2SESS_dice_LONG_230228.csv")
    DICE30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/dice/network_dice/HCP_30MIN_2SESS_dice_LONG_230228.csv")
    
    #Drop Network 0 (medial wall)
    #DICE
    DICE5MIN <- subset(DICE5MIN, Network!=0)
    DICE10MIN <- subset(DICE10MIN, Network!=0)
    DICE15MIN <- subset(DICE15MIN, Network!=0)
    DICE20MIN <- subset(DICE20MIN, Network!=0)
    DICE25MIN <- subset(DICE25MIN, Network!=0)
    DICE30MIN <- subset(DICE30MIN, Network!=0)
    
    #Reorder networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    
    #DICE
    DICE5MIN$NewNetwork <- recode(DICE5MIN$Network, !!!mapping)
    DICE10MIN$NewNetwork <- recode(DICE10MIN$Network, !!!mapping)
    DICE15MIN$NewNetwork <- recode(DICE15MIN$Network, !!!mapping)
    DICE20MIN$NewNetwork <- recode(DICE20MIN$Network, !!!mapping)
    DICE25MIN$NewNetwork <- recode(DICE25MIN$Network, !!!mapping)
    DICE30MIN$NewNetwork <- recode(DICE30MIN$Network, !!!mapping)
    
#Question: How much data is needed for a stable Kong2019 parcellation estimate?
    #Avg. Dice within Network
    # Reshape the data
    wide_data <- data.frame(
      NewNetwork = DICE5MIN$NewNetwork,
      Dice_5min = DICE5MIN$Dice,
      Dice_10min = DICE10MIN$Dice,
      Dice_15min = DICE15MIN$Dice,
      Dice_20min = DICE20MIN$Dice,
      Dice_25min = DICE25MIN$Dice,
      Dice_30min = DICE30MIN$Dice
    )
    
    # Split the data by network
    network_data <- split(wide_data, wide_data$NewNetwork)
    
    # Calculate within-network mean dice
    avg_data <- lapply(network_data, function(x) {
      network <- unique(x$NewNetwork)
      avg_5min <- mean(x$Dice_5min)
      avg_10min <- mean(x$Dice_10min)
      avg_15min <- mean(x$Dice_15min)
      avg_20min <- mean(x$Dice_20min)
      avg_25min <- mean(x$Dice_25min)
      avg_30min <- mean(x$Dice_30min)
      data.frame(NETWORK = network, 
                 DICE = c(avg_5min, avg_10min, avg_15min, avg_20min, avg_25min, avg_30min),
                 TIME = c("5", "10", "15", "20", "25", "30"))
    })
    
    # Combine the dice data into a single dataframe
    avg_data <- do.call(rbind, avg_data)
    
    
    #Fig. 1: Subject RAINCLOUD OF DICE X TIME
    avg_data$TIME <- as.factor(avg_data$TIME)
    avg_data$DICE <- as.numeric(avg_data$DICE)
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(avg_data, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = DICE, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
      ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1)), clip = "off")+
      labs(y="Subject Mean Dice Coefficient", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_Parc_SubRain_Dice_230522.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    
    #Fig. 2: PARCELLATION x Network
    #Plot MEAN DICE x TIME per NETWORK
    avg_data$TIME <- as.factor(avg_data$TIME)
    avg_data$DICE <- as.numeric(avg_data$DICE)
    avg_data$NETWORK <- as.factor(avg_data$NETWORK)
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    ggplot(avg_data, aes(x=factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y=DICE, color=NETWORK))+
      geom_line(aes(group=NETWORK))+
      labs(x = "Quantity of Data (Minutes)", y = 'Network Mean Dice')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=CBIG_Palette)+
      scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=factor(NETWORK)), colour="black", pch=21)+
      scale_y_continuous(limits=c(.6,.9))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_StableEst_Parc_Dice_230522.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    
    #Fig. 3: Scatterplots: Dice x Network (filtered by time)
    #Fig. 3: Scatterplots: Dice x Time (filtered by network)
    #Convert to long-format data
    dice_long <- wide_data %>% 
      pivot_longer(
        cols = c(starts_with('Dice_')),
        names_to = "TIME", 
        values_to = "DICE"
      )
    dice_long$TIME <- as.factor(dice_long$TIME)
    dice_long$DICE <- as.numeric(dice_long$DICE)
    dice_long$NewNetwork <- as.factor(dice_long$NewNetwork)
    #Format long-format data
    dice_long$TIME <- gsub("^.{0,5}", "", dice_long$TIME) #remove "Dice_" prefix
    dice_long$TIME <- gsub("...$", "", dice_long$TIME) #remove "min" suffix
    
    
    time_intervals <- c("5", "10", "15", "20", "25", "30")
    
    for (interval in time_intervals) {
      filename <- paste("Study1_HCP_DICE_TimePlots_", interval, "MIN_230524.png", sep = "")
      plot_title <- paste("Dice:", interval)
      subsetted_data <- subset(dice_long, TIME==interval)
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      
      ggplot(subsetted_data, aes(x = NewNetwork, y = DICE, fill=NewNetwork)) + 
        ggdist::stat_halfeye(
          adjust = .5, 
          width = .6, 
          .width = 0, 
          justification = -.3, 
          point_colour = "NA") + 
        geom_boxplot(
          width = .25, 
          outlier.shape = 21, 
          outlier.fill = NULL
        ) +
        #geom_point(
        #  size = 1.3,
        #  alpha = .3,
        #  position = position_jitter(
        #    seed = 1, width = .1
        #  )
        #) + 
        coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1)), clip = "off")+
        labs(y=plot_title, x="Network")+
        scale_colour_manual(values=CBIG_Palette)+
        scale_fill_manual(values=CBIG_Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
        #scale_x_discrete(labels=c("Autism", "Control")) +
        theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
        theme(panel.background = element_blank())+
        theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
      
      ggsave(filename = filename, width = 7, height = 2,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    }
    
    
#---------------------------------HCP STABLE EST.: NSAR------------------------------------------
#SETUP
    #Load files: SA-workbench
    SA5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/5MIN/HCP_5MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/10MIN/HCP_10MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/15MIN/HCP_15MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/20MIN/HCP_20MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/25MIN/HCP_25MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/30MIN/HCP_30MIN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SA2SESS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/2SESS/HCP_2SESS_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    
    #Load files: SA-freesurfer
    #SA5MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/5MIN/HCP_5MIN_FS_NETSA_230526.csv")
    #SA10MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/10MIN/HCP_10MIN_FS_NETSA_230526.csv")
    #SA15MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/15MIN/HCP_15MIN_FS_NETSA_230526.csv")
    #SA20MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/20MIN/HCP_20MIN_FS_NETSA_230526.csv")
    #SA25MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/25MIN/HCP_25MIN_FS_NETSA_230526.csv")
    #SA30MIN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/30MIN/HCP_30MIN_FS_NETSA_230526.csv")
    #SA2SESS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa_fs/2SESS/HCP_2SESS_FS_NETSA_230526.csv")
    
    
    
    #SA: format NETWORK var
    SA5MIN$Network <- gsub("^.{0,8}", "", SA5MIN$NETWORK)
    SA10MIN$Network <- gsub("^.{0,8}", "", SA10MIN$NETWORK)
    SA15MIN$Network <- gsub("^.{0,8}", "", SA15MIN$NETWORK)
    SA20MIN$Network <- gsub("^.{0,8}", "", SA20MIN$NETWORK)
    SA25MIN$Network <- gsub("^.{0,8}", "", SA25MIN$NETWORK)
    SA30MIN$Network <- gsub("^.{0,8}", "", SA30MIN$NETWORK)
    SA2SESS$Network <- gsub("^.{0,8}", "", SA2SESS$NETWORK)
    
    #SA: Drop medial wall
    SA5MIN <- subset(SA5MIN, Network!=0)
    SA10MIN <- subset(SA10MIN, Network!=0)
    SA15MIN <- subset(SA15MIN, Network!=0)
    SA20MIN <- subset(SA20MIN, Network!=0)
    SA25MIN <- subset(SA25MIN, Network!=0)
    SA30MIN <- subset(SA30MIN, Network!=0)
    SA2SESS <- subset(SA2SESS, Network!=0)
    
    #Reorder networks
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)

    SA5MIN$NewNetwork <- recode(SA5MIN$Network, !!!mapping)
    SA10MIN$NewNetwork <- recode(SA10MIN$Network, !!!mapping)
    SA15MIN$NewNetwork <- recode(SA15MIN$Network, !!!mapping)
    SA20MIN$NewNetwork <- recode(SA20MIN$Network, !!!mapping)
    SA25MIN$NewNetwork <- recode(SA25MIN$Network, !!!mapping)
    SA30MIN$NewNetwork <- recode(SA30MIN$Network, !!!mapping)
    
    #SA lateralization variable
    # create a character vector of the data frame names
    df_names <- c("SA5MIN", "SA10MIN", "SA15MIN", "SA20MIN", "SA25MIN", "SA30MIN", "SA2SESS")
    
    # loop through the data frame names and apply the code to each data frame
    for (df_name in df_names) {
      # assign the data frame to a new object with the same name
      assign(df_name, transform(get(df_name), SA_LAT = (RH_SA - LH_SA) / (LH_SA + RH_SA)))
    }
    
    #Total SA variable
    # create a character vector of the data frame names
    df_names <- c("SA5MIN", "SA10MIN", "SA15MIN", "SA20MIN", "SA25MIN", "SA30MIN", "SA2SESS")
    for (df_name in df_names) {
      # assign the data frame to a new object with the same name
      assign(df_name, transform(get(df_name), SA_TOT = (LH_SA + RH_SA)))
    }
     
#Question: How much data is needed for a stable estimate of the SAI?
  #Calculate ICC for SAI(NETWORK basis) 
    #Create wide-format of SAI data
    sa_wide_data <- data.frame(
      NewNetwork = SA5MIN$NewNetwork,
      SA_5min = SA5MIN$SA_LAT,
      SA_10min = SA10MIN$SA_LAT,
      SA_15min = SA15MIN$SA_LAT,
      SA_20min = SA20MIN$SA_LAT,
      SA_25min = SA25MIN$SA_LAT,
      SA_30min = SA30MIN$SA_LAT,
      SA_2SESS = SA2SESS$SA_LAT
    )
    
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(SAI_ICC_5MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_5min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(SAI_ICC_5MIN = icc3_value))
    }
    #ICC 10MIN
    icc_df2 <- data.frame(SAI_ICC_10MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_10min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(SAI_ICC_15MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_15min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(SAI_ICC_20MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_20min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(SAI_ICC_25MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_25min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(SAI_ICC_30MIN = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("SA_30min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
   
    #Add NewNetwork variable: 
    icc_df$NewNetwork <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    
    #Format icc_df LONG
    icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('SAI_ICC_')),
        names_to = "TIME", 
        values_to = "SAI_ICC"
        )
      
    icc_long$TIME <- as.factor(icc_long$TIME)
    icc_long$SAI_ICC <- as.numeric(icc_long$SAI_ICC)
    icc_long$NewNetwork <- as.factor(icc_long$NewNetwork)
    
    icc_long$TIME <- gsub("^.{0,8}", "", icc_long$TIME) #remove extraneous characters from $TIME
    icc_long$TIME <- gsub("...$", "", icc_long$TIME)
    
 #Mean ICC for CTRL-A (Network 10)   
    MEAN_ICC <- aggregate(SAI_ICC ~ NewNetwork, data = icc_long, FUN = mean)
    CTRLA_ICC <- MEAN_ICC[MEAN_ICC$NewNetwork == 10, "SAI_ICC"]
    print(CTRLA_ICC)
    
 #SAI NETWORK ICC FIGURES   
    #Fig. 1: Plot CORR x TIME per NETWORK (Figure S2)
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    ggplot(icc_long, aes(x=factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y=SAI_ICC, color=NewNetwork))+
      geom_line(aes(group=NewNetwork))+
      labs(x = "Quantity of Data (Minutes)", y = 'Network NSAR ICC')+
      labs(fill = " ")+
      labs(color = " ")+
      scale_colour_manual(values=CBIG_Palette)+
      scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=factor(NewNetwork)), colour="black", pch=21)+
      #scale_y_continuous(limits=c(0,.68))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_HCP_StableEst_SAI_Network_ICC_230522.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    
    #JUST BOXPLOT (Network ICC) (Fig 5B Replacement)
        Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
        ggplot(icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = SAI_ICC, fill=TIME)) + 
          ggdist::stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.3, 
            point_colour = "NA") + 
          geom_boxplot(
            width = .25, 
            outlier.shape = 21, 
            outlier.fill = NULL
          ) +
          #geom_point(
          #  size = 1.3,
          #  alpha = .3,
          #  position = position_jitter(
          #    seed = 1, width = .1
          #  )
          #) + 
          coord_cartesian((xlim = c(1.2, NA)), clip = "off")+
          labs(y="Network NSAR ICC", x="Quantity of Data (Minutes)")+
          scale_colour_manual(values=Palette)+
          scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
          #scale_x_discrete(labels=c("Autism", "Control")) +
          theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
        ggsave(filename = paste("Study1_HCP_StableEst_NSAR_Network_ICC_Box_230801.png"), width = 3.35, height = 3.35,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
        

        
 
  #Calculate SAI ICC on a subject basis
    #Create wide-format of SAI data
    sa_wide_data <- data.frame(
      SUBJID = SA5MIN$SUBJID,
      SA_5min = SA5MIN$SA_LAT,
      SA_10min = SA10MIN$SA_LAT,
      SA_15min = SA15MIN$SA_LAT,
      SA_20min = SA20MIN$SA_LAT,
      SA_25min = SA25MIN$SA_LAT,
      SA_30min = SA30MIN$SA_LAT,
      SA_2SESS = SA2SESS$SA_LAT
    )
    
    # Create an empty dataframe to store ICC3 values
    #ICC 5MIN
    icc_df <- data.frame(SAI_ICC_5MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_5min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(SAI_ICC_5MIN = icc3_value))
    }
    
    #ICC 10MIN
    icc_df2 <- data.frame(SAI_ICC_10MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_10min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_10MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #15MIN
    icc_df2 <- data.frame(SAI_ICC_15MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_15min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_15MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #20MIN
    icc_df2 <- data.frame(SAI_ICC_20MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_20min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_20MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #25MIN
    icc_df2 <- data.frame(SAI_ICC_25MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_25min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_25MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #30MIN
    icc_df2 <- data.frame(SAI_ICC_30MIN = numeric(0))
    for (subj_id in unique(sa_wide_data$SUBJID)) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == subj_id)[, c("SA_30min", "SA_2SESS")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", subj_id, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(SAI_ICC_30MIN = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add SUBJID variable: 
    icc_df$SUBJID <- unique(sa_wide_data$SUBJID)
      
    #Format icc_df LONG
    icc_long <- icc_df %>% 
      pivot_longer(
        cols = c(starts_with('SAI_ICC_')),
        names_to = "TIME", 
        values_to = "SAI_ICC"
      )
    
    icc_long$TIME <- as.factor(icc_long$TIME)
    icc_long$SAI_ICC <- as.numeric(icc_long$SAI_ICC)
    icc_long$SUBJID <- as.factor(icc_long$SUBJID)
    
    icc_long$TIME <- gsub("^.{0,8}", "", icc_long$TIME) #remove extraneous characters from $TIME
    icc_long$TIME <- gsub("...$", "", icc_long$TIME)
    

        
  #Fig. 2: Subject RAINCLOUD OF SAI X TIME
    Palette <- c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")
    ggplot(icc_long, aes(x = factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y = SAI_ICC, fill=TIME)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = "NA") + 
      geom_boxplot(
        width = .25, 
        outlier.shape = 21, 
        outlier.fill = NULL
        ) +
      #geom_point(
      #  size = 1.3,
      #  alpha = .3,
      #  position = position_jitter(
      #    seed = 1, width = .1
      #  )
      #) + 
      coord_cartesian((xlim = c(1.2, NA)), (ylim=c(0, 1.0)), clip = "off")+
      labs(y="Subject NSAR ICC", x="Quantity of Data (Minutes)")+
      scale_colour_manual(values=Palette)+
      scale_fill_manual(values=Palette)+  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 12))+
      #scale_x_discrete(labels=c("Autism", "Control")) +
      theme(legend.position = "none", axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), axis.text.x =element_text(colour = "black", hjust = 0.6))+
      theme(panel.background = element_blank())+
      theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    ggsave(filename = paste("Study1_HCP_StableEst_SAI_Subject_ICC_Rain_230522.png"), width = 3.35, height = 3.35,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
    
    #Mean subject ICC x Time
    SUB_ICC_MEAN <- aggregate(SAI_ICC ~ TIME, data = icc_long, FUN = mean)
    #Std. Error
    std <- function(x) sd(x)/sqrt(length(x))
    SUB_ICC_SDERR <- aggregate(SAI_ICC ~ TIME, data = icc_long, FUN = std)
    

#Create scatterplots for NETWORK and SUBJID SAI values: 
    #Format data
    sa_wide_data <- data.frame(
      NewNetwork = SA5MIN$NewNetwork,
      SUBJID = SA5MIN$SUBJID,
      SA_5min = SA5MIN$SA_LAT,
      SA_10min = SA10MIN$SA_LAT,
      SA_15min = SA15MIN$SA_LAT,
      SA_20min = SA20MIN$SA_LAT,
      SA_25min = SA25MIN$SA_LAT,
      SA_30min = SA30MIN$SA_LAT,
      SA_2SESS = SA2SESS$SA_LAT
    )

    

#SCATTERPLOTS (TIME X 2SESS), color=Network
    CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
    sa_wide_data$NewNetwork <- as.factor(sa_wide_data$NewNetwork)
    sa_wide_data$SUBJID <- as.factor(sa_wide_data$SUBJID)
    
    time_intervals <- c("5", "10", "15", "20", "25", "30")
    
    for (interval in time_intervals) {
      filename <- paste("Study1_HCP_SAI_CorrPlots_", interval, "_230524.png", sep = "")
      plot_title <- paste("SAI:", interval)
      y_column <- paste("SA_", interval, "min", sep = "")
      
      ggplot(sa_wide_data, aes(x = SA_2SESS, y = get(y_column), color = NewNetwork)) +
        labs(x = "SAI: Independent 30MIN", y = plot_title) +
        labs(fill = " ", color = " ") +
        geom_point(aes(fill = NewNetwork), pch = 21) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
        geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
        scale_y_continuous(limits = c(-1, 1)) +
        scale_x_continuous(limits = c(-1, 1)) +
        scale_color_manual(values = CBIG_Palette) +
        scale_fill_manual(values = CBIG_Palette) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.title = element_text(colour = "black", size = 12),
              axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
              axis.text.x = element_text(colour = "black"),
              legend.position = "none",
              legend.title = element_text(colour = "black", size = 12),
              legend.text = element_text(colour = "black", size = 12),
              legend.background = element_rect(fill = "white", size = 0.5),
              axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
              axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
              panel.border = element_blank(),
              panel.background = element_blank())
      
      ggsave(filename = filename, width = 3.35, height = 3.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    }

#Scatterplots by network
    for (n in 1:17) {
      subsetted_data <- subset(sa_wide_data, NewNetwork == n)  # Subsetting the dataset based on Network
            
      CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
      subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
      subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
      
      time_intervals <- c("5", "10", "15", "20", "25", "30")
      
      for (interval in time_intervals) {
        filename <- paste("Study1_HCP_SAI_CorrPlots_", interval, "min_Network", n, "_230524.png", sep = "")
        plot_title <- paste("SAI:", interval, ", Network: ", n)
        y_column <- paste("SA_", interval, "min", sep = "")
        
        ggplot(subsetted_data, aes(x = SA_2SESS, y = get(y_column), color = NewNetwork)) +
          labs(x = "SAI: Independent 30MIN", y = plot_title) +
          labs(fill = " ", color = " ") +
          geom_point(aes(fill = NewNetwork), pch = 21) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
          geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
          scale_y_continuous(limits = c(-1, 1)) +
          scale_x_continuous(limits = c(-1, 1)) +
          scale_color_manual(values = CBIG_Palette[n]) +
          scale_fill_manual(values = CBIG_Palette[n]) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.title = element_text(colour = "black", size = 12),
                axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                axis.text.x = element_text(colour = "black"),
                legend.position = "none",
                legend.title = element_text(colour = "black", size = 12),
                legend.text = element_text(colour = "black", size = 12),
                legend.background = element_rect(fill = "white", size = 0.5),
                axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                panel.border = element_blank(),
                panel.background = element_blank())
        
        ggsave(filename = filename, width = 2.21, height = 2.21,
               path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      }
    }
    
   
    
     
#NETWORK STABLE ESTIMATE - DIFF
    # Reshape the data
    wide_data <- data.frame(
      SUBJID = SA5MIN$SUBJID,
      NETWORK = SA5MIN$NewNetwork,
      SA_5min = SA5MIN$SA_LAT,
      SA_10min = SA10MIN$SA_LAT,
      SA_15min = SA15MIN$SA_LAT,
      SA_20min = SA20MIN$SA_LAT,
      SA_25min = SA25MIN$SA_LAT,
      SA_30min = SA30MIN$SA_LAT,
      SA_2SESS = SA2SESS$SA_LAT
    )
    
   #Create difference variable (2SESS - TIME)  
   wide_data$DIFF5 <- wide_data$SA_2SESS - wide_data$SA_5min 
   wide_data$DIFF10 <- wide_data$SA_2SESS - wide_data$SA_10min 
   wide_data$DIFF15 <- wide_data$SA_2SESS - wide_data$SA_15min 
   wide_data$DIFF20 <- wide_data$SA_2SESS - wide_data$SA_20min 
   wide_data$DIFF25 <- wide_data$SA_2SESS - wide_data$SA_25min 
   wide_data$DIFF30 <- wide_data$SA_2SESS - wide_data$SA_30min 
   
   #Remove unnecessary columns
   wide_data <- wide_data[,c("SUBJID", "NETWORK", "DIFF5", "DIFF10", "DIFF15", "DIFF20", "DIFF25", "DIFF30")]
   
   #Average DIFF within each network
   # Split the data by network
   network_data <- split(wide_data, wide_data$NETWORK)
   
   # Calculate within-network mean dice
   avg_data <- lapply(network_data, function(x) {
     network <- unique(x$NETWORK)
     avg_5min <- mean(x$DIFF5)
     avg_10min <- mean(x$DIFF10)
     avg_15min <- mean(x$DIFF15)
     avg_20min <- mean(x$DIFF20)
     avg_25min <- mean(x$DIFF25)
     avg_30min <- mean(x$DIFF30)
     data.frame(NETWORK = network, 
                DIFF = c(avg_5min, avg_10min, avg_15min, avg_20min, avg_25min, avg_30min),
                TIME = c("5", "10", "15", "20", "25", "30"))
   })
   
   # Combine the dice data into a single dataframe
   avg_data <- do.call(rbind, avg_data)
   
   #FIGURE: AVG DIFF per NETWORK x TIME
   avg_data$TIME <- as.factor(avg_data$TIME)
   avg_data$ABS_DIFF <- abs(avg_data$DIFF)
   avg_data$NETWORK <- as.factor(avg_data$NETWORK)
   CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
   ggplot(avg_data, aes(x=factor(TIME, level=c('5', '10', '15', '20', '25', '30')), y=ABS_DIFF, color=NETWORK))+
     geom_line(aes(group=NETWORK))+
     labs(x = "Quantity of Data (Minutes)", y = 'NSAR Network Mean Difference')+
     labs(fill = " ")+
     labs(color = " ")+
     scale_colour_manual(values=CBIG_Palette)+
     scale_fill_manual(values=CBIG_Palette)+
     geom_point(aes(fill=factor(NETWORK)), colour="black", pch=21)+
     scale_y_continuous(limits=c(0,.14), expand=c(0,0))+
     theme_bw()+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
     theme(axis.title = element_text(colour = "black", size=12), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
           axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
           legend.position = "none", legend.title=element_text(colour = "black", size = 12), legend.text=element_text(colour = "black", size = 12), 
           legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
           axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
   ggsave(filename = paste("Study1_HCP_StableEst_NSAR_DIFF_230527.png"), width = 3.35, height = 3.35,
          path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
   
   
   
#-------------------------------------TEST-RETEST HCP 6 NETWORKS------------------------
#Load HCP test-retest dataset
   HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
   HCP_ALL_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
   HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
   FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_HCP_ALL_230515.csv")
   DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_HCP_ALL_230515.csv")
   TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_HCP_230515.csv")
   
   #Format data
   names(HCP_DEMOS)[1] <- "SUBJID"
   names(HCP_R_DEMOS)[1] <- "SUBJID"
   names(FD_AVG)[1] <- "SUBJID"
   names(DVARS_AVG)[1] <- "SUBJID"
   names(TOT_VOLS)[1] <- "SUBJID"
   
   FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
   DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
   TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
   TOT_VOLS$Percent_Vols <- TOT_VOLS$Sum_Volumes / (1196*4) #Number of volumes after skip4 = 1196 x4 runs
   
   #Merge HCP datasets
   comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
   comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
   comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
   HCP_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
   
   #Variable for sex (Male=1, Female=2; same as UT-TD)
   HCP_df$sex <- ifelse(HCP_df$Gender == "F", 2, ifelse(HCP_df$Gender == "M", 1, NA))
   
   #Just include relevant columns
   HCP_df <- HCP_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex")]
   
   #Load test-retest NSAR/SAI values
   RETEST1_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/RETEST1/HCP_RETEST1_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
   RETEST2_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/RETEST2/HCP_RETEST2_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
   #Formatting variables
   RETEST1_SA$SUBJID <- gsub("^.{0,4}", "", RETEST1_SA$SUBJID) #remove "sub-" string
   RETEST2_SA$SUBJID <- gsub("^.{0,4}", "", RETEST2_SA$SUBJID) #remove "sub-" string
   RETEST1_SA$Network <- gsub("^.{0,8}", "", RETEST1_SA$NETWORK) #remove "NETWORK-" string
   RETEST2_SA$Network <- gsub("^.{0,8}", "", RETEST2_SA$NETWORK) #remove "NETWORK-" string
   
   #Drop medial wall
   RETEST1_SA <- subset(RETEST1_SA, Network!=0) #drop network0
   RETEST2_SA <- subset(RETEST2_SA, Network!=0) #drop network0
   
   #Switch network ordering to reflect CBIG legend ordering
   mapping <- c(12, 6, 3, 13, 5, 1, 8, 7, 10, 11, 15, 14, 4, 2, 17, 16, 9)
   oldvalues <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
   RETEST1_SA$NewNetwork <- mapping[ match(RETEST1_SA$Network, oldvalues) ]
   RETEST1_SA <- RETEST1_SA[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
   RETEST2_SA$NewNetwork <- mapping[ match(RETEST2_SA$Network, oldvalues) ]
   RETEST2_SA <- RETEST2_SA[,c("SUBJID", "LH_SA", "RH_SA", "NewNetwork")]
   
   #Create SA LAT variable (NSAR)
   RETEST1_SA$SA_LAT_RETEST1 <- (RETEST1_SA$RH_SA - RETEST1_SA$LH_SA) / (RETEST1_SA$LH_SA + RETEST1_SA$RH_SA)
   RETEST2_SA$SA_LAT_RETEST2 <- (RETEST2_SA$RH_SA - RETEST2_SA$LH_SA) / (RETEST2_SA$LH_SA + RETEST2_SA$RH_SA)
   
   #Merge SAI RETEST datasets
   RETEST_SA_HCP <- merge(RETEST1_SA, RETEST2_SA, by = c("SUBJID", "NewNetwork"), all=TRUE)
   
   #Fence outliers for AI TEST and RETEST
   for (network in 1:17) {
     # Subset the dataset based on the current NewNetwork value
     subset_indices <- RETEST_SA_HCP$NewNetwork == network
     
     # Loop through each variable to apply the fencing method
     variables <- c("SA_LAT_RETEST1", "SA_LAT_RETEST2")
     for (variable in variables) {
       subset_data <- RETEST_SA_HCP[subset_indices, variable]
       
       # Calculate quartiles (Q1 and Q3) and IQR for the current subset
       Q1 <- quantile(subset_data, 0.25)
       Q3 <- quantile(subset_data, 0.75)
       IQR <- Q3 - Q1
       
       # Calculate upper and lower limits for the current subset
       upper_limit <- Q3 + 1.5 * IQR
       lower_limit <- Q1 - 1.5 * IQR
       
       # Create a new column with "_Fenced" suffix for each variable
       fence_column <- paste0(variable, "_Fenced")
       
       # Replace outliers with NA in the new Fenced column
       RETEST_SA_HCP[subset_indices, fence_column] <- subset_data
       RETEST_SA_HCP[subset_indices, fence_column][subset_data < lower_limit | subset_data > upper_limit] <- NA
     }
   }
   
   
#Calculate network-level ICC (HCP dataset)
   # Create an empty dataframe to store ICC3 values
   icc_net_df <- data.frame(NSAR_ICC = numeric(0))
   for (network in unique(RETEST_SA_HCP$NewNetwork)) {
     # Create dataframe subset
     df_subset <- subset(RETEST_SA_HCP, NewNetwork == network)[, c("SA_LAT_RETEST1_Fenced", "SA_LAT_RETEST2_Fenced")]
     
     # Rename dataframe
     df_name <- paste("nsar_wide_dataR.", network, sep = "")
     assign(df_name, df_subset)
     
     # Compute ICC and save ICC3 value to dataframe
     icc_result <- suppressWarnings(ICC(get(df_name)))
     icc3_value <- icc_result$results$ICC[3]
     icc_net_df <- rbind(icc_net_df, data.frame(NSAR_ICC = icc3_value))
   }
   
   #Add SUBJID variable: 
   icc_net_df$NewNetwork <- unique(RETEST_SA_HCP$NewNetwork)
   
   #Add dataset variable
   icc_net_df$dataset <- "HCP"
   
   

#FIGURES
   #NETWORK SCATTERPLOTS
   for (n in 1:17) {
     subsetted_data <- subset(RETEST_SA_HCP, NewNetwork == n)  # Subsetting the dataset based on Network
     icc <- icc_net_df$NSAR_ICC
     icc_ordered <- icc[order(icc_net_df$NewNetwork)]
     Network_Names<- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
     
     min_value <- min(subsetted_data$SA_LAT_RETEST1_Fenced, subsetted_data$SA_LAT_RETEST2_Fenced)
     max_value <- max(subsetted_data$SA_LAT_RETEST1_Fenced, subsetted_data$SA_LAT_RETEST2_Fenced)
     
     CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
     subsetted_data$NewNetwork <- as.factor(subsetted_data$NewNetwork)
     subsetted_data$SUBJID <- as.factor(subsetted_data$SUBJID)
     
     filename <- paste("Study1_HCP_NSAR_RETEST_Plots_Network", n, "_230701.png", sep = "")
     plot_title <- paste(Network_Names[n], " ICC: ", format(round(icc_ordered[n], 2), nsmall = 2), sep="")
     
     ggplot(subsetted_data, aes(x = SA_LAT_RETEST2_Fenced, y = SA_LAT_RETEST1_Fenced, color = NewNetwork)) +
       labs(x = "NSAR Session 2", y = "NSAR Session 1") +
       labs(fill = " ", color = " ") +
       ggtitle(plot_title)+
       geom_point(aes(fill = NewNetwork),color="black", pch = 21) +
       geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
       geom_smooth(aes(color = NewNetwork), method = "lm", size = 0.75, se = FALSE) +
       scale_y_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
       scale_x_continuous(limits = c(min_value, max_value), labels = function(x) gsub("^0\\.", ".", sprintf("%.2f", x))) +
       scale_color_manual(values = CBIG_Palette[n]) +
       scale_fill_manual(values = CBIG_Palette[n]) +
       theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0, size=10),
             axis.title = element_text(colour = "black", size = 10),
             axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=9),
             axis.text.x = element_text(colour = "black", size=9),
             legend.position = "none",
             legend.title = element_text(colour = "black", size = 11),
             legend.text = element_text(colour = "black", size = 11),
             legend.background = element_rect(fill = "white", size = 0.5),
             axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
             axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
             panel.border = element_blank(),
             panel.background = element_blank())
     
     ggsave(filename = filename, width = 1.675, height = 1.675,
            path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
   }
   

   
    
#-------------------------------------NSD TASK EFFECTS: NSAR---------------------------------------
#SETUP    
    #Load files: Dice
    DICE_REST_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST/NSD_REST_EVEN_ODD_dice_LONG_230302.csv")
    DICE_REST_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST/NSD_REST_FIRST_SECOND_dice_LONG_230302.csv")
    DICE_REST_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST/NSD_REST_RAND1_RAND2_dice_LONG_230302.csv")
    DICE_TASK_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK/NSD_TASK_EVEN_ODD_dice_LONG_230302.csv")
    DICE_TASK_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK/NSD_TASK_FIRST_SECOND_dice_LONG_230302.csv")
    DICE_TASK_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK/NSD_TASK_RAND1_RAND2_dice_LONG_230302.csv")
    
    DICE_REST_TASK_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST_TASK/NSD_REST_TASK_EVEN_ODD_dice_LONG_230302.csv")
    DICE_REST_TASK_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST_TASK/NSD_REST_TASK_FIRST_SECOND_dice_LONG_230302.csv")
    DICE_REST_TASK_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/REST_TASK/NSD_REST_TASK_RAND1_RAND2_dice_LONG_230302.csv")
    DICE_TASK_REST_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK_REST/NSD_TASK_REST_EVEN_ODD_dice_LONG_230302.csv")
    DICE_TASK_REST_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK_REST/NSD_TASK_REST_FIRST_SECOND_dice_LONG_230302.csv")
    DICE_TASK_REST_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/TASK_REST/NSD_TASK_REST_RAND1_RAND2_dice_LONG_230302.csv")
    
    DICE_ALL_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/ALL_DICE/NSD_ITERATION_COMBO_ALL_EVENODD_dice_LONG_230308.csv")
    DICE_ALL_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/ALL_DICE/NSD_ITERATION_COMBO_ALL_FIRSTSECOND_dice_LONG_230308.csv")
    DICE_ALL_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/ALL_DICE/NSD_ITERATION_COMBO_ALL_RAND1RAND2_dice_LONG_230308.csv")
    
    DICE_NET_EVENODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/NETWORK_DICE/NSD_ITERATION_COMBO_NETWORK_EVENODD_dice_LONG_230308.csv")
    DICE_NET_FIRSTSECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/NETWORK_DICE/NSD_ITERATION_COMBO_NETWORK_FIRSTSECOND_dice_LONG_230308.csv")
    DICE_NET_RAND1RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/dice/NETWORK_DICE/NSD_ITERATION_COMBO_NETWORK_RAND1RAND2_dice_LONG_230308.csv")
    
    
    #Load files: SA  
    #REST
    SAREST_EVEN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_EVEN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SAREST_ODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_ODD_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SAREST_FIRST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_FIRST_HALF_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SAREST_SECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_SECOND_HALF_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SAREST_RAND1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_RAND1_1_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SAREST_RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/REST/NSD_REST_RAND1_2_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    
    #TASK
    SATASK_EVEN <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_EVEN_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SATASK_ODD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_ODD_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SATASK_FIRST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_FIRST_HALF_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SATASK_SECOND <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_SECOND_HALF_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SATASK_RAND1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_RAND1_1_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    SATASK_RAND2 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/network_sa/TASK/NSD_TASK_RAND1_2_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
    
    #Format network variable for SA datasets
    list_of_dfs <- c("SAREST_EVEN", "SAREST_ODD", "SAREST_FIRST", "SAREST_SECOND", "SAREST_RAND1", "SAREST_RAND2", "SATASK_EVEN", "SATASK_ODD", "SATASK_FIRST", "SATASK_SECOND", "SATASK_RAND1", "SATASK_RAND2")
    # Define the pattern and replacement string
    pattern <- "^.{0,8}"
    replacement <- ""
    for (df_name in list_of_dfs) {
      assign(df_name, transform(get(df_name), 
                                Network = gsub(pattern, replacement, NETWORK)))
    }
    
    #Drop Network 0 (medial wall)
    #SA
    list_of_sa <- c("SAREST_EVEN", "SAREST_ODD", "SAREST_FIRST", "SAREST_SECOND", "SAREST_RAND1", "SAREST_RAND2", "SATASK_EVEN", "SATASK_ODD", "SATASK_FIRST", "SATASK_SECOND", "SATASK_RAND1", "SATASK_RAND2")
    # Loop over each dataframe in the list and apply the subset function
    for (df_name in list_of_sa) {
      assign(df_name, subset(get(df_name), Network != 0))
    }
    
    #Dice
    list_of_dice <- c("DICE_REST_EVENODD", "DICE_REST_FIRSTSECOND", "DICE_REST_RAND1RAND2", "DICE_TASK_EVENODD", "DICE_TASK_FIRSTSECOND", "DICE_TASK_RAND1RAND2", "DICE_REST_TASK_EVENODD", "DICE_REST_TASK_FIRSTSECOND", "DICE_REST_TASK_RAND1RAND2", "DICE_TASK_REST_EVENODD", "DICE_TASK_REST_FIRSTSECOND", "DICE_TASK_REST_RAND1RAND2")
    # Loop over each dataframe in the list and apply the subset function
    for (df_name in list_of_dice) {
      assign(df_name, subset(get(df_name), Network != 0))
    }
    
    #Dice -NETWORK
    list_of_dice <- c("DICE_NET_EVENODD", "DICE_NET_FIRSTSECOND", "DICE_NET_RAND1RAND2")
    # Loop over each dataframe in the list and apply the subset function
    for (df_name in list_of_dice) {
      assign(df_name, subset(get(df_name), NETWORK != 0))
    }
    
    
    #Reorder networks to match Kong2019 network order 
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    list_of_dfs <- c("SAREST_EVEN", "SAREST_ODD", "SAREST_FIRST", "SAREST_SECOND", "SAREST_RAND1", "SAREST_RAND2", "SATASK_EVEN", "SATASK_ODD", "SATASK_FIRST", "SATASK_SECOND", "SATASK_RAND1", "SATASK_RAND2", "DICE_REST_EVENODD", "DICE_REST_FIRSTSECOND", "DICE_REST_RAND1RAND2", "DICE_TASK_EVENODD", "DICE_TASK_FIRSTSECOND", "DICE_TASK_RAND1RAND2", "DICE_REST_TASK_EVENODD", "DICE_REST_TASK_FIRSTSECOND", "DICE_REST_TASK_RAND1RAND2", "DICE_TASK_REST_EVENODD", "DICE_TASK_REST_FIRSTSECOND", "DICE_TASK_REST_RAND1RAND2")
    for (df_name in list_of_dfs) {
      df <- get(df_name)
      df$NewNetwork <- recode(df$Network, !!!mapping)
      assign(df_name, df)
    }
    
    mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
    list_of_dfs <- c("DICE_NET_EVENODD", "DICE_NET_FIRSTSECOND", "DICE_NET_RAND1RAND2")
    for (df_name in list_of_dfs) {
      df <- get(df_name)
      df$NewNetwork <- recode(df$NETWORK, !!!mapping)
      assign(df_name, df)
    }
    
    #SA lateralization variable
    # create a character vector of the data frame names
    list_of_sa <- c("SAREST_EVEN", "SAREST_ODD", "SAREST_FIRST", "SAREST_SECOND", "SAREST_RAND1", "SAREST_RAND2", "SATASK_EVEN", "SATASK_ODD", "SATASK_FIRST", "SATASK_SECOND", "SATASK_RAND1", "SATASK_RAND2")
    
    # loop through the data frame names and apply the code to each data frame
    for (df_name in list_of_sa) {
      # assign the data frame to a new object with the same name
      assign(df_name, transform(get(df_name), SA_LAT = (LH_SA - RH_SA) / (LH_SA + RH_SA)))
    }
    
    
    #SA DATASET
    #Setup dataframe
    sa_wide_data <- data.frame(
      NewNetwork = SAREST_EVEN$NewNetwork,
      SUBJID = SAREST_EVEN$SUBJID,
      REST_EVEN = SAREST_EVEN$SA_LAT,
      REST_ODD = SAREST_ODD$SA_LAT,
      REST_FIRST = SAREST_FIRST$SA_LAT,
      REST_SECOND = SAREST_SECOND$SA_LAT,
      REST_RAND1 = SAREST_RAND1$SA_LAT,
      REST_RAND2 = SAREST_RAND2$SA_LAT,
      TASK_EVEN = SATASK_EVEN$SA_LAT, 
      TASK_ODD = SATASK_ODD$SA_LAT,
      TASK_FIRST = SATASK_FIRST$SA_LAT,
      TASK_SECOND = SATASK_SECOND$SA_LAT,
      TASK_RAND1 = SATASK_RAND1$SA_LAT,
      TASK_RAND2 = SATASK_RAND2$SA_LAT
    )
    #Reshape to long
    sa_long_data <- sa_wide_data %>% 
      pivot_longer(
        cols = c("REST_EVEN", "REST_ODD", "REST_FIRST", "REST_SECOND", "REST_RAND1", "REST_RAND2", "TASK_EVEN", "TASK_ODD", "TASK_FIRST", "TASK_SECOND", "TASK_RAND1", "TASK_RAND2"), 
        names_to = "ITERATION",
        values_to = "SA_LAT"
      )
    sa_long_data$ITERATION <- as.factor(sa_long_data$ITERATION)
    sa_long_data$NewNetwork <- as.factor(sa_long_data$NewNetwork)
    sa_long_data$SA_LAT <- as.numeric(sa_long_data$SA_LAT)
    sa_wide_data$NewNetwork <- as.factor(sa_wide_data$NewNetwork)
    
    #DICE DATASET
    #Setup dataframe
    net_wide_data <- data.frame(
      NewNetwork = DICE_REST_EVENODD$NewNetwork,
      SUBJID = DICE_REST_EVENODD$SUBJID,
      REST_EVENODD = DICE_REST_EVENODD$Dice,
      REST_FIRSTSECOND = DICE_REST_FIRSTSECOND$Dice,
      REST_RAND1RAND2 = DICE_REST_RAND1RAND2$Dice,
      TASK_EVENODD = DICE_TASK_EVENODD$Dice,
      TASK_FIRSTSECOND = DICE_TASK_FIRSTSECOND$Dice,
      TASK_RAND1RAND2 = DICE_TASK_RAND1RAND2$Dice, 
      REST_TASK_EVENODD = DICE_REST_TASK_EVENODD$Dice,
      REST_TASK_FIRSTSECOND = DICE_REST_TASK_FIRSTSECOND$Dice,
      REST_TASK_RAND1RAND2 = DICE_REST_TASK_RAND1RAND2$Dice,
      TASK_REST_EVENODD = DICE_TASK_REST_EVENODD$Dice,
      TASK_REST_FIRSTSECOND = DICE_TASK_REST_FIRSTSECOND$Dice,
      TASK_REST_RAND1RAND2 = DICE_TASK_REST_RAND1RAND2$Dice
    )
    #Reshape to long
    net_long_data <- net_wide_data %>% 
      pivot_longer(
        cols = c("REST_EVENODD", "REST_FIRSTSECOND", "REST_RAND1RAND2", "TASK_EVENODD", "TASK_FIRSTSECOND", "TASK_RAND1RAND2", "REST_TASK_EVENODD", "REST_TASK_FIRSTSECOND", "REST_TASK_RAND1RAND2", "TASK_REST_EVENODD", "TASK_REST_FIRSTSECOND", "TASK_REST_RAND1RAND2", "REST_TASK_EVENODD", "REST_TASK_FIRSTSECOND", "REST_TASK_RAND1RAND2"), 
        names_to = "ITERATION",
        values_to = "Dice"
      )
    net_long_data$ITERATION <- as.factor(net_long_data$ITERATION)
    net_long_data$NewNetwork <- as.factor(net_long_data$NewNetwork)
    net_long_data$Dice <- as.numeric(net_long_data$Dice)
    net_wide_data$NewNetwork <- as.factor(net_wide_data$NewNetwork)

    
    #Create ICC dataset
    #REST-REST
    #EVEN_ODD
    # Create an empty dataframe to store ICC3 values
    icc_df <- data.frame(REST_EVEN_ODD_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_EVEN", "REST_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df <- rbind(icc_df, data.frame(REST_EVEN_ODD_ICC = icc3_value))
    }
    #FIRST_SECOND
    icc_df2 <- data.frame(REST_FIRST_SECOND_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_FIRST", "REST_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(REST_FIRST_SECOND_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #RAND1_RAND2
    icc_df2 <- data.frame(REST_RAND1_RAND2_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_RAND1", "REST_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(REST_RAND1_RAND2_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    #TASK-TASK
    #EVEN_ODD
    icc_df2 <- data.frame(TASK_EVEN_ODD_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_EVEN", "TASK_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_EVEN_ODD_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #FIRST_SECOND
    icc_df2 <- data.frame(TASK_FIRST_SECOND_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_FIRST", "TASK_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_FIRST_SECOND_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #RAND1_RAND2
    icc_df2 <- data.frame(TASK_RAND1_RAND2_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_RAND1", "TASK_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_RAND1_RAND2_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #Add NewNetwork variable: 
    icc_df$NewNetwork <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    
    #TASK-REST
    #EVEN_ODD
    icc_df2 <- data.frame(TASK_REST_EVEN_ODD_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_EVEN", "REST_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_REST_EVEN_ODD_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #FIRST_SECOND
    icc_df2 <- data.frame(TASK_REST_FIRST_SECOND_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_FIRST", "REST_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_REST_FIRST_SECOND_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #RAND1/RAND2
    icc_df2 <- data.frame(TASK_REST_RAND1_RAND2_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("TASK_RAND1", "REST_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(TASK_REST_RAND1_RAND2_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #REST-TASK
    #EVEN_ODD
    icc_df2 <- data.frame(REST_TASK_EVEN_ODD_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_EVEN", "TASK_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(REST_TASK_EVEN_ODD_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    #FIRST_SECOND
    icc_df2 <- data.frame(REST_TASK_FIRST_SECOND_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_FIRST", "TASK_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(REST_TASK_FIRST_SECOND_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    
    #RAND1_RAND2
    icc_df2 <- data.frame(REST_TASK_RAND1_RAND2_ICC = numeric(0))
    for (i in 1:17) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, NewNetwork == i)[, c("REST_RAND1", "TASK_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      icc_df2 <- rbind(icc_df2, data.frame(REST_TASK_RAND1_RAND2_ICC = icc3_value))
    }
    icc_df <- cbind(icc_df, icc_df2)
    
    
    
    
    
    #MEAN ICC
    #TASK-REST
    mean(icc_df$TASK_REST_EVEN_ODD_ICC)
    mean(icc_df$TASK_REST_FIRST_SECOND_ICC)
    mean(icc_df$TASK_REST_RAND1_RAND2_ICC)
    
    #REST-TASK
    mean(icc_df$REST_TASK_EVEN_ODD_ICC)
    mean(icc_df$REST_TASK_FIRST_SECOND_ICC)
    mean(icc_df$REST_TASK_RAND1_RAND2_ICC)
    
    
    #INDIVIDUAL SUBJECT SA ICC
    sa_wide_data$SUBJID <- gsub("^.{0,5}", "", sa_wide_data$SUBJID)
    
    #EVEN-ODD
    #REST-REST
    # Create an empty dataframe to store ICC3 values
    eo_sa_icc_df <- data.frame(RESTREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_EVEN", "REST_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      eo_sa_icc_df <- rbind(eo_sa_icc_df, data.frame(RESTREST = icc3_value))
    }
    
    eo_sa_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
    
    #TASK-TASK
    # Create an empty dataframe to store ICC3 values
    eo_sa_icc_df2 <- data.frame(TASKTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_EVEN", "TASK_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      eo_sa_icc_df2 <- rbind(eo_sa_icc_df2, data.frame(TASKTASK = icc3_value))
    } 
    eo_sa_icc_df <- cbind(eo_sa_icc_df, eo_sa_icc_df2)
    
    #TASK-REST
    # Create an empty dataframe to store ICC3 values
    eo_sa_icc_df2 <- data.frame(TASKREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_ODD", "REST_ODD")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      eo_sa_icc_df2 <- rbind(eo_sa_icc_df2, data.frame(TASKREST = icc3_value))
    } 
    eo_sa_icc_df <- cbind(eo_sa_icc_df, eo_sa_icc_df2)
    
    #REST-TASK
    # Create an empty dataframe to store ICC3 values
    eo_sa_icc_df2 <- data.frame(RESTTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_EVEN", "TASK_EVEN")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      eo_sa_icc_df2 <- rbind(eo_sa_icc_df2, data.frame(RESTTASK = icc3_value))
    } 
    eo_sa_icc_df <- cbind(eo_sa_icc_df, eo_sa_icc_df2)
    
    #RESHAPE EO DATASET
    eo_sa_icc_long <- eo_sa_icc_df %>% 
      pivot_longer(
        cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
        names_to = "ITERATION",
        values_to = "SA_ICC"
      )
    eo_sa_icc_long$ITERATION <- as.factor(eo_sa_icc_long$ITERATION)
    eo_sa_icc_long$SA_ICC <- as.numeric(eo_sa_icc_long$SA_ICC)
    eo_sa_icc_long$SUBJID <- as.factor(eo_sa_icc_long$SUBJID)
    
    
    #FIRST-SECOND            
    #REST-REST
    # Create an empty dataframe to store ICC3 values
    fs_sa_icc_df <- data.frame(RESTREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_FIRST", "REST_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      fs_sa_icc_df <- rbind(fs_sa_icc_df, data.frame(RESTREST = icc3_value))
    }
    
    fs_sa_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
    
    #TASK-TASK
    # Create an empty dataframe to store ICC3 values
    fs_sa_icc_df2 <- data.frame(TASKTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_FIRST", "TASK_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      fs_sa_icc_df2 <- rbind(fs_sa_icc_df2, data.frame(TASKTASK = icc3_value))
    } 
    fs_sa_icc_df <- cbind(fs_sa_icc_df, fs_sa_icc_df2)
    
    #TASK-REST
    # Create an empty dataframe to store ICC3 values
    fs_sa_icc_df2 <- data.frame(TASKREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_SECOND", "REST_SECOND")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      fs_sa_icc_df2 <- rbind(fs_sa_icc_df2, data.frame(TASKREST = icc3_value))
    } 
    fs_sa_icc_df <- cbind(fs_sa_icc_df, fs_sa_icc_df2)
    
    #REST-TASK
    # Create an empty dataframe to store ICC3 values
    fs_sa_icc_df2 <- data.frame(RESTTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_FIRST", "TASK_FIRST")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      fs_sa_icc_df2 <- rbind(fs_sa_icc_df2, data.frame(RESTTASK = icc3_value))
    } 
    fs_sa_icc_df <- cbind(fs_sa_icc_df, fs_sa_icc_df2)
    
    #RESHAPE EO DATASET
    fs_sa_icc_long <- fs_sa_icc_df %>% 
      pivot_longer(
        cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
        names_to = "ITERATION",
        values_to = "SA_ICC"
      )
    fs_sa_icc_long$ITERATION <- as.factor(fs_sa_icc_long$ITERATION)
    fs_sa_icc_long$SA_ICC <- as.numeric(fs_sa_icc_long$SA_ICC)
    fs_sa_icc_long$SUBJID <- as.factor(fs_sa_icc_long$SUBJID)
    
    #RAND1-RAND2            
    #REST-REST
    # Create an empty dataframe to store ICC3 values
    rr_sa_icc_df <- data.frame(RESTREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_RAND1", "REST_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      rr_sa_icc_df <- rbind(rr_sa_icc_df, data.frame(RESTREST = icc3_value))
    }
    
    rr_sa_icc_df$SUBJID <- c(1, 2, 3, 4, 5, 6, 7, 8)
    
    #TASK-TASK
    # Create an empty dataframe to store ICC3 values
    rr_sa_icc_df2 <- data.frame(TASKTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_RAND1", "TASK_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      rr_sa_icc_df2 <- rbind(rr_sa_icc_df2, data.frame(TASKTASK = icc3_value))
    } 
    rr_sa_icc_df <- cbind(rr_sa_icc_df, rr_sa_icc_df2)
    
    #TASK-REST
    # Create an empty dataframe to store ICC3 values
    rr_sa_icc_df2 <- data.frame(TASKREST = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("TASK_RAND2", "REST_RAND2")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      rr_sa_icc_df2 <- rbind(rr_sa_icc_df2, data.frame(TASKREST = icc3_value))
    } 
    rr_sa_icc_df <- cbind(rr_sa_icc_df, rr_sa_icc_df2)
    
    #REST-TASK
    # Create an empty dataframe to store ICC3 values
    rr_sa_icc_df2 <- data.frame(RESTTASK = numeric(0))
    for (i in 1:8) {
      # Create dataframe subset
      df_subset <- subset(sa_wide_data, SUBJID == i)[, c("REST_RAND1", "TASK_RAND1")]
      
      # Rename dataframe
      df_name <- paste("sa_wide_dataR.", i, sep = "")
      assign(df_name, df_subset)
      
      # Compute ICC and save ICC3 value to dataframe
      icc_result <- suppressWarnings(ICC(get(df_name)))
      icc3_value <- icc_result$results$ICC[3]
      rr_sa_icc_df2 <- rbind(rr_sa_icc_df2, data.frame(RESTTASK = icc3_value))
    } 
    rr_sa_icc_df <- cbind(rr_sa_icc_df, rr_sa_icc_df2)
    
    #RESHAPE EO DATASET
    rr_sa_icc_long <- rr_sa_icc_df %>% 
      pivot_longer(
        cols = c("RESTREST", "TASKTASK", "TASKREST", "RESTTASK"), 
        names_to = "ITERATION",
        values_to = "SA_ICC"
      )
    rr_sa_icc_long$ITERATION <- as.factor(rr_sa_icc_long$ITERATION)
    rr_sa_icc_long$SA_ICC <- as.numeric(rr_sa_icc_long$SA_ICC)
    rr_sa_icc_long$SUBJID <- as.factor(rr_sa_icc_long$SUBJID)
    
    
    
    
#Question: Are network parcellations task-dependent? 
    #WILCOXON SIGNED-RANK TEST: DICE
    #EO
    DICE_ALL_EVENODD <- DICE_ALL_EVENODD[,c("SUBJID", "DIR_COMBO", "Dice")]
    eo_wide <- pivot_wider(DICE_ALL_EVENODD, names_from = DIR_COMBO, values_from = Dice, names_prefix = "ITERATION.")
    #TASKTASK vs RESTREST
    wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.RESTREST, paired = TRUE)
    #TASKTASK vs TASKREST
    wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.TASKREST, paired = TRUE)
    #RESTREST vs RESTTASK
    wilcox.test(eo_wide$ITERATION.RESTREST, eo_wide$ITERATION.RESTTASK, paired = TRUE)
    
    #FS
    DICE_ALL_FIRSTSECOND <- DICE_ALL_FIRSTSECOND[,c("SUBJID", "DIR_COMBO", "Dice")]
    fs_wide <- pivot_wider(DICE_ALL_FIRSTSECOND, names_from = DIR_COMBO, values_from = Dice, names_prefix = "ITERATION.")
    #TASKTASK vs RESTREST
    wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.RESTREST, paired = TRUE)
    #TASKTASK vs TASKREST
    wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.TASKREST, paired = TRUE)
    #RESTREST vs RESTTASK
    wilcox.test(fs_wide$ITERATION.RESTREST, fs_wide$ITERATION.RESTTASK, paired = TRUE)
    
    #R1R2
    DICE_ALL_RAND1RAND2 <- DICE_ALL_RAND1RAND2[,c("SUBJID", "DIR_COMBO", "Dice")]
    rr_wide <- pivot_wider(DICE_ALL_RAND1RAND2, names_from = DIR_COMBO, values_from = Dice, names_prefix = "ITERATION.")
    #TASKTASK vs RESTREST
    wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.RESTREST, paired = TRUE)
    #TASKTASK vs TASKREST
    wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.TASKREST, paired = TRUE)
    #RESTREST vs RESTTASK
    wilcox.test(rr_wide$ITERATION.RESTREST, rr_wide$ITERATION.RESTTASK, paired = TRUE)
    
#SET 4: ALL DICE 
    #FIGURE 4.1: EVEN/ODD
    ggplot(DICE_ALL_EVENODD, aes(x=factor(DIR_COMBO, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=Dice))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Even/Odd Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0.5, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_Parc_Dice_Set4_1_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    
    #Figure 4.2: FIRST/SECOND
    ggplot(DICE_ALL_FIRSTSECOND, aes(x=factor(DIR_COMBO, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=Dice))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'First/Second Half of Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0.5, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_Parc_Dice_Set4_2_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #FIGURE 4.3: RAND1/RAND2
    ggplot(DICE_ALL_RAND1RAND2, aes(x=factor(DIR_COMBO, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=Dice))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Random Selection of Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0.5, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_Parc_Dice_Set4_3_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    

    
#Question: Is SAI task-dependent?
#WILCOXON SIGNED-RANK TEST: NSAR
    #EO
      eo_wide <- pivot_wider(eo_sa_icc_long, names_from = ITERATION, values_from = SA_ICC, names_prefix = "ITERATION.")
      #TASKTASK vs RESTREST
      wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.RESTREST, paired = TRUE)
      #TASKTASK vs TASKREST
      wilcox.test(eo_wide$ITERATION.TASKTASK, eo_wide$ITERATION.TASKREST, paired = TRUE)
      #RESTREST vs RESTTASK
      wilcox.test(eo_wide$ITERATION.RESTREST, eo_wide$ITERATION.RESTTASK, paired = TRUE)
      
    #FS
      fs_wide <- pivot_wider(fs_sa_icc_long, names_from = ITERATION, values_from = SA_ICC, names_prefix = "ITERATION.")
      #TASKTASK vs RESTREST
      wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.RESTREST, paired = TRUE)
      #TASKTASK vs TASKREST
      wilcox.test(fs_wide$ITERATION.TASKTASK, fs_wide$ITERATION.TASKREST, paired = TRUE)
      #RESTREST vs RESTTASK
      wilcox.test(fs_wide$ITERATION.RESTREST, fs_wide$ITERATION.RESTTASK, paired = TRUE)
      
    #R1R2
      rr_wide <- pivot_wider(rr_sa_icc_long, names_from = ITERATION, values_from = SA_ICC, names_prefix = "ITERATION.")
      #TASKTASK vs RESTREST
      wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.RESTREST, paired = TRUE)
      #TASKTASK vs TASKREST
      wilcox.test(rr_wide$ITERATION.TASKTASK, rr_wide$ITERATION.TASKREST, paired = TRUE)
      #RESTREST vs RESTTASK
      wilcox.test(rr_wide$ITERATION.RESTREST, rr_wide$ITERATION.RESTTASK, paired = TRUE)
      
#SET 4: SAI (NSAR) ICC SUBJECT-LEVEL
    #FIGURE 4.1: EVEN/ODD
    ggplot(eo_sa_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=SA_ICC))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Even/Odd Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_SAI_ICC_Set4_1_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #FIGURE 4.2: FIRST/SECOND
    ggplot(fs_sa_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=SA_ICC))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'First/Second Half of Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_SAI_ICC_Set4_2_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
    #FIGURE 4.3: RAND1/RAND2
    ggplot(rr_sa_icc_long, aes(x=factor(ITERATION, level=c('TASKTASK', 'RESTREST', 'TASKREST', 'RESTTASK')), y=SA_ICC))+
      geom_line(aes(group=SUBJID))+
      labs(x = '', y = 'Random Selection of Runs')+
      #scale_colour_manual(values=CBIG_Palette)+
      #scale_fill_manual(values=CBIG_Palette)+
      geom_point(aes(fill=SUBJID), colour="black", pch=21, size=2.5)+
      scale_y_continuous(limits = c(0, 1.0))+
      scale_x_discrete(labels=c("Task-Task", "Rest-Rest", "Task-Rest", "Rest-Task")) +
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6), 
            axis.text.x = element_text(colour = "black", hjust = 0.6, vjust=.8, size=10, angle = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position ="none", legend.title=element_blank(), legend.text=element_blank(), 
            legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
            axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
    ggsave(filename = paste("Study1_NSD_SAI_ICC_Set4_3_230522.png"), width = 2.21, height = 2.21,
           path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
    
        
      
#---------------------------MOST SPECIALIZED NETWORKS------------------------------
#SETUP
      #Load ALL .csv
      study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
      study1$Sex_Bin <- as.factor(study1$Sex_Bin)  

      
#Multiple regressions to test for significant intercept given covariates
      #Covariates=mean-centered age, sex, handedness, mean-centered mean FD
      #Bonferroni-corrected alpha = .003
      
      
  #HCP-DISC (276) Analysis
      HCP_NSAR <- subset(study1, dataset=="HCP-DISC")
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCP_NSAR, NewNetwork == i)
        
        model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCP_DISC_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCP_DISC_model1)
      
     
      
  #HCP-REP 277 ANALYSIS
      HCPR_NSAR <- subset(study1, dataset=="HCP-REP")
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPR_NSAR, NewNetwork == i)
        
        model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCP_REP_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCP_REP_model1)
      
      
  #HCPD 343 ANALYSIS  
      HCPD_NSAR <- subset(study1, dataset=="HCPD")
      for (i in 1:17) {
        # Subset the data based on NewNetwork value
        subset_data <- subset(HCPD_NSAR, NewNetwork == i)
        
        model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered + Handedness, data = subset_data)
        # Create a unique name for each model
        model_name <- paste("HCPD_model", i, sep = "")
        
        # Assign the model to the unique name
        assign(model_name, model)
      }
      #Access model results through: summary(HCPD_model1)
      
     
      
#Model-adjusted AI for visual inspection (HCP, HCPD)
      study1$SA_LAT_ADJ <- NA
      ci_df <- data.frame(dataset=factor(),
                             NewNetwork=factor(),
                             CI_MIN=integer(),
                             CI_MAX=integer(),
                             PERC97.5=integer(),
                             PERC2.5=integer(),
                             PERC50=integer(),
                             MEAN=integer())
      
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study1, dataset==d)
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          # Fit the linear regression model
          model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- study1$SUBJID %in% subsetted_ids &
            study1$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          study1$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$SA_LAT_ADJ)
          
          n <- length(subset_data$SA_LAT_ADJ)
          std_dev <- sd(subset_data$SA_LAT_ADJ)
          std_err <- std_dev / sqrt(n)
          z_score=1.96
          margin_error <- z_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          
          #calculate 97.5 and 2.5 percentiles
          PERC97.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.975)
          PERC2.5 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.025)
          PERC50 <- quantile(subset_data$SA_LAT_ADJ, probs = 0.5)
          
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, PERC97.5, PERC2.5, PERC50, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "PERC97.5", "PERC2.5", "PERC50", "MEAN")
          ci_df <- rbind(ci_df, row_df)
        }
      }
      
      
    
      
#FIGURE: Model-adjusted NSAR for HCP
      #FIG 1: Vertical point and line
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      network_order <- c('6', '15', '5', '10', '13', '9', '7', '4', '3', '1', '14', '16', '12', '8', '2', '11', '17')
      ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCPD", "HCP-REP", "HCP-DISC")
      ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
      ggplot(ci_df, aes(x = MEAN, y = NewNetwork, group=interaction(dataset, NewNetwork), fill=dataset)) +
        geom_hline(yintercept = seq(from=.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbarh(aes(xmin=PERC2.5, xmax = PERC97.5), height = 0,  position = position_dodge(width = .5), color="black", size=.5) +
        geom_point(position = position_dodge(width = .5), size=2, shape=21) +
        coord_cartesian(ylim= c(1.2, NA), clip = "off") +
        labs(y = "", x = "Adjusted Avg. NSAR") +
        scale_y_discrete(labels = c("DAN-A", "DEF-C", "LANG", "CTRL-A", "DEF-A", "SAL-B", "DAN-B", "SOM-B", "SOM-A", "VIS-A", "DEF-B", "LIM-A", "CTRL-C", "SAL-A", "VIS-B","CTRL-B",  "LIM-B"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_SpecNetworks_PointLineAdjusted_230701.png"), width = 3.5, height = 6,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      

      
      #FIG 1: Horizontal point and line
      GroupPalette <- c("#E69F00", "#D55E00", "#0072B2")
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      ci_df$NewNetwork <- as.factor(ci_df$NewNetwork)
      network_order <- c('6', '15', '5', '10', '13', '9', '7', '4', '3', '1', '14', '16', '12', '8', '2', '11', '17')
      ci_df$NewNetwork <- factor(ci_df$NewNetwork, level = network_order)
      dataset_order <- c("HCP-DISC", "HCP-REP", "HCPD")
      ci_df$dataset <- factor(ci_df$dataset, level=dataset_order)
      ggplot(ci_df, aes(x = NewNetwork, y = MEAN, group=interaction(dataset, NewNetwork), fill=dataset)) +
        geom_vline(xintercept = seq(from=1.5, to=17.5, by = 1), linetype="dotted", color="darkgray") +
        geom_errorbar(aes(ymin=PERC2.5, ymax = PERC97.5), width = 0,  color="black", size=1, position = position_dodge(width = 0.5)) +
        geom_point(size=2.5, shape=21, position = position_dodge(width = 0.5)) +
        coord_cartesian(xlim= c(1.2, NA), clip = "off") +
        labs(y = "Adjusted Avg. NSAR", x = "") +
        scale_x_discrete(labels = c("DAN-A", "DEF-C", "LANG", "CTRL-A", "DEF-A", "SAL-B", "DAN-B", "SOM-B", "SOM-A", "VIS-A", "DEF-B", "LIM-A", "CTRL-C", "SAL-A", "VIS-B","CTRL-B",  "LIM-B"))+
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
          axis.text.x = element_text(colour = "black", vjust=1, angle=25, hjust=1),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_SpecNetworks_PointLineAdjusted_HORZ_230706.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
           
      
      #FIG 1: LH Vertical boxplot
      #network_order <- c('17', '16', '15', '14', '13', '12', '11', '10', '9', '8', '7', '6', '5', '4', '3', '2', '1')
      #network_order <- c('14', '1', '2', '16', '12', '11', '10', '9', '8', '4', '3', '7', '15', '17', '5', '13', '6')
      # use factor() to set the order of the factor levels
      #HCP_AI$NewNetwork <- factor(HCP_AI$NewNetwork, level = network_order)
      #GroupPalette <- c("#E69F00","#D55E00", "#009E73")
      # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
      #ggplot(HCP_AI, aes(x = LH_AVG_AI_ADJ, y = NewNetwork, group=interaction(dataset, NewNetwork), fill = dataset)) + 
      #  geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = "black") +
      #  coord_cartesian(ylim= c(1.2, NA), clip = "off") +
      #  labs(y = "", x = "Adjusted LH AVG AI") +
      #  #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
      #  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
      #  #scale_y_discrete(labels =c("Limbic-B", "Limbic-A", "Default-C", "Default-B", "Default-A", "Control-C", "Control-B", "Control-A", "Salience/VenAttn-B", "Salience/VenAttn-A", "Dorsal Attention-B", "Dorsal Attention-A", "Language", "Somatomotor-B", "Somatomotor-A", "Visual-B", "Visual-A"))+
      #  scale_y_discrete(labels = c("Default-B", "Visual-A", "Visual-B", "Limbic-A", "Control-C", "Control-B", "Control-A", "Sal/VenAttn-B", "Sal/VenAttn-A", "Somatomotor-B", "Somatomotor-A", "Dorsal Attn-B", "Default-C", "Limbic-B", "Language", "Default-A", "Dorsal Attn-A"))+
      #  scale_colour_manual(values = GroupPalette) +
      #  scale_fill_manual(values = GroupPalette) + 
      #  theme(
      #    axis.text = element_text(size = 10),
      #    axis.title = element_text(size = 10),
      #    legend.position = "none",
      #    axis.text.y = element_text(colour = "black", angle = 0, hjust = 1),
      #    axis.text.x = element_text(colour = "black", vjust=1),
      #    panel.background = element_blank(),
      #    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      #  )
      #ggsave(filename = paste("Study3_HCP_AI_LH_Boxplots_230615.png"), width = 3.35, height = 6,
      #       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study3_figures/png_figures/", dpi = 300)
      

#Fig. 9: 8 networks percent SA LH and RH in separate panels, together, also NSAR adjusted       
      #Subset to 8 networks
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      study1_8 <- study1[study1$NewNetwork %in% network_order, ]
      # create % surface area per hemisphere (total SA: 126300.7)
      study1_8$LH_SA_PERCENT <- (study1_8$LH_SA/63103.74)*100
      study1_8$RH_SA_PERCENT <- (study1_8$RH_SA/63196.98)*100
      
      #Fig. 9A: LH SA Percent With X-axis MSHBM Labels. Boxplots
      # specify the order of the networks
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      study1_8$NewNetwork <- factor(study1_8$NewNetwork, level = network_order)
      GroupPalette <- c("#E69F00","#D55E00","#0072B2")
      # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
      ggplot(study1_8, aes(x = NewNetwork, y = LH_SA_PERCENT, group=interaction(dataset, NewNetwork), fill = dataset)) + 
        geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
        coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 17), clip = "off") +
        scale_y_continuous(expand=c(0,0))+
        labs(y = '% LH Surface Area', x = "") +
        #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A"))+
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_HCP_NSD_LH_SA_Percent_8N_Boxplots_230603.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
      #Fig. 9B: RH SA Percent With X-axis MSHBM Labels. Boxplots
      # specify the order of the networks
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      study1_8$NewNetwork <- factor(study1_8$NewNetwork, level = network_order)
      GroupPalette <- c("#E69F00","#D55E00","#0072B2")
      # use scale_fill_manual() to specify the order of the colors in CBIG_Palette
      ggplot(study1_8, aes(x = NewNetwork, y = RH_SA_PERCENT, group=interaction(dataset, NewNetwork), fill = dataset)) + 
        geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
        coord_cartesian(xlim = c(1.2, NA), ylim = c(0, 17), clip = "off") +
        scale_y_continuous(expand=c(0,0))+
        labs(y = '% RH Surface Area', x = "") +
        #geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A"))+
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_HCP_NSD_RH_SA_Percent_8N_Boxplots_230603.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
      
      #Fig. 9C: % SA for LH and RH. With X-axis MSHBM Labels. Boxplots
      #Mean percent
      LH_MEAN <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = mean)
      RH_MEAN <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = mean)
      
      names(LH_MEAN)[3] <- "MEAN_LH_PERCENT"
      names(RH_MEAN)[3] <- "MEAN_RH_PERCENT"
      mean_df <- merge(LH_MEAN, RH_MEAN, by=c("dataset", "NewNetwork"), all=TRUE)
      
      MEAN_LONG <- mean_df %>%
        pivot_longer(cols = starts_with("MEAN_"),
                     names_to = c("HEMI"),
                     names_pattern = "MEAN_(.*)")
      names(MEAN_LONG)[4] <- "MEAN_PERCENT"
      
      
      #Find percentiles (2.5, 97.5)
      LH_PERCENTILES1 <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = function(x) quantile(x, probs = 0.025))
      LH_PERCENTILES2 <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = function(x) quantile(x, probs = 0.975))
      RH_PERCENTILES1 <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = function(x) quantile(x, probs = 0.025))
      RH_PERCENTILES2 <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = function(x) quantile(x, probs = 0.975))
      names(LH_PERCENTILES1)[3] <- c("PERC_LH_2.5")
      names(LH_PERCENTILES2)[3] <- c("PERC_LH_97.5")
      names(RH_PERCENTILES1)[3] <- c("PERC_RH_2.5")
      names(RH_PERCENTILES2)[3] <- c("PERC_RH_97.5")

      percentiles_df1 <- merge(LH_PERCENTILES1, LH_PERCENTILES2, by = c("dataset", "NewNetwork"), all = TRUE)
      percentiles_df2 <- merge(RH_PERCENTILES1, RH_PERCENTILES2, by = c("dataset", "NewNetwork"), all=TRUE)
      percentiles_df <- merge(percentiles_df1, percentiles_df2, by=c("dataset", "NewNetwork"), all=TRUE)
      PERC_LONG <- percentiles_df %>%
      pivot_longer(
          cols = starts_with("PERC_"),
          names_to = c("HEMI", ".value"),
          names_pattern = "PERC_(\\w+)_(\\d+\\.\\d+)"
        )
      PERC_LONG$HEMI <- paste0(PERC_LONG$HEMI, "_PERCENT")
      

      #std. error
      std <- function(x) sd(x)/sqrt(length(x))
      LH_STD <- aggregate(LH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = std)
      RH_STD <- aggregate(RH_SA_PERCENT ~ dataset + NewNetwork, data = study1_8, FUN = std)
      
      names(LH_STD)[3] <- "SE_LH_PERCENT"
      names(RH_STD)[3] <- "SE_RH_PERCENT"
      se_df <- merge(LH_STD, RH_STD, by=c("dataset", "NewNetwork"), all=TRUE)
      SE_LONG <- se_df %>%
        pivot_longer(cols = starts_with("SE_"),
                     names_to = c("HEMI"),
                     names_pattern = "SE_(.*)")
      names(SE_LONG)[4] <- "SE_PERCENT"
      
      SA_df <- merge(SE_LONG, MEAN_LONG, by=c("dataset", "NewNetwork", "HEMI"), all=TRUE)
      SA_df <- merge(SA_df, PERC_LONG, by=c("dataset", "NewNetwork", "HEMI"), all=TRUE)
      
      #combine dataset and hemi into one var
      SA_df$data_hemi = as.factor(paste0(SA_df$dataset,SA_df$HEMI))
     
    #USING STD ERR BARS   
      # specify the order of the networks
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      study1_8$NewNetwork <- factor(study1_8$NewNetwork, level = network_order)
      GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #Order networks numerically
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      SA_df$NewNetwork <- factor(SA_df$NewNetwork, level = network_order)
      #Order groups manually
      group_order <- c('HCP-DISCLH_PERCENT', "HCP-DISCRH_PERCENT", "HCP-REPLH_PERCENT", "HCP-REPRH_PERCENT", "HCPDLH_PERCENT", "HCPDRH_PERCENT")
      SA_df$data_hemi <- factor(SA_df$data_hemi, levels=group_order)
      GroupPalette <- c("#E69F00","#FFDF97","#D55E00", "#FFB075", "#0072B2", "#9BDBFF")
      ggplot(SA_df, aes(x = NewNetwork, y = MEAN_PERCENT, group=interaction(data_hemi, NewNetwork), fill = data_hemi)) +
        geom_bar(stat = "identity", position=position_dodge(width = .9)) +
        geom_errorbar(aes(ymin=MEAN_PERCENT-SE_PERCENT, ymax = MEAN_PERCENT+SE_PERCENT), width = .5,  position = position_dodge(width = .9), color="black", size=.5) +
        labs(x = "", y = "Mean % Surface Area") +
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        scale_y_continuous(expand=c(0,0), limits=c(0,11.0))+
        scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A"))+
        #theme_bw()
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_HCP_HCPD_UT_LHRH_SA_Percent_8N_Boxplots_230603.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
    #USING PERCENTILES for bars  
      # specify the order of the networks
      #network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      #study1_8$NewNetwork <- factor(study1_8$NewNetwork, level = network_order)
      #GroupPalette <- c("#0072B2","#D55E00","#E69F00")
      #Order networks numerically
      #network_order <- c('17', '12' ,'2', '8', '11', '5', '`5', '6')
      # use factor() to set the order of the factor levels
      #SA_df$NewNetwork <- factor(SA_df$NewNetwork, level = network_order)
      #Order groups manually
      #group_order <- c('HCP-DISCLH_PERCENT', "HCP-DISCRH_PERCENT", "HCP-REPLH_PERCENT", "HCP-REPRH_PERCENT", "HCPDLH_PERCENT", "HCPDRH_PERCENT")
      #SA_df$data_hemi <- factor(SA_df$data_hemi, levels=group_order)
      #GroupPalette <- c("#E69F00","#FFDF97","#D55E00", "#FFB075", "#0072B2", "#9BDBFF")
      #ggplot(SA_df, aes(x = NewNetwork, y = MEAN_PERCENT, group=interaction(data_hemi, NewNetwork), fill = data_hemi)) +
      #  geom_bar(stat = "identity", position=position_dodge(width = .8)) +
      #  geom_errorbar(aes(ymin=`2.5`, ymax = `97.5`), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
      #  labs(x = "", y = "Mean % Surface Area") +
      #  scale_colour_manual(values = GroupPalette) +
      #  scale_fill_manual(values = GroupPalette) + 
      #  scale_y_continuous(expand=c(0,0), limits=c(0,14.4))+
      #  scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A"))+
      #  #theme_bw()
      #  theme(
      #    axis.text = element_text(size = 10),
      #    axis.title = element_text(size = 10),
      #    legend.position = "none",
      #    axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
      #    axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
      #    panel.background = element_blank(),
      #    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
      #  )
      #ggsave(filename = paste("Study1_HCP_HCPD_UT_LHRH_SA_Percent_8N_Boxplots_PERCENTILE_230714.png"), width = 6.9, height = 2.35,
      #       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
      
      
      #Fig. 9D: Adjusted NSAR values.
      # specify the order of the networks
      network_order <- c('17', '12' ,'2', '8', '11', '5', '15', '6')
      # use factor() to set the order of the factor levels
      study1_8$NewNetwork <- factor(study1_8$NewNetwork, level = network_order)
      GroupPalette <- c("#E69F00","#D55E00","#0072B2")
      ggplot(study1_8, aes(x = NewNetwork, y = SA_LAT_ADJ, group=interaction(dataset, NewNetwork), fill = dataset)) +
        geom_boxplot(width = .75, outlier.shape = 21, outlier.fill = NULL) +
        coord_cartesian(xlim = c(1.2, NA), ylim = c(-1, 1), clip = "off") +
        #geom_errorbar(aes(ymin=MEAN_PERCENT-SE_PERCENT, ymax = MEAN_PERCENT+SE_PERCENT), width = .5,  position = position_dodge(width = .8), color="black", size=.5) +
        labs(x = "", y = "Adjusted NSAR Value") +
        scale_colour_manual(values = GroupPalette) +
        scale_fill_manual(values = GroupPalette) + 
        scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A"))+
        #theme_bw()
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.position = "none",
          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
          axis.text.x = element_text(colour = "black", hjust = .5, vjust=1, angle = 0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1, linetype = "solid")
        )
      ggsave(filename = paste("Study1_HCP_HCPD_UT_NSAR_ADJ_8N_Boxplots_230603.png"), width = 6.9, height = 2.35,
             path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
      
      
      
          
      
      
      
#Most Specialized Network Comparisons 
      #Question: Which networks are the most right- and left-lateralized?
        #Left-lateralized: Language (5), Dorsal Attention-A (6), Default-C (15)
        #Right-lateralized: Visual-B (2), Sal/VenAttn-A (8), Control-B (11), Control-C (12), Limbic-B (17)
      
      #Load ALL .csv
      study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
      study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
      study1$NewNetwork <- as.factor(study1$NewNetwork)
      
      #HCP-DISC dataset
          #Subset to HCP-DISC dataset
          HCPDISC <- subset(study1, dataset=="HCP-DISC")
          
          #LEFT Comparisons
            #1. LANG vs. DAN-A
              subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(5, 6))
              HCPDISC_left_model1 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
              summary(HCPDISC_left_model1)
              #6 < 5
            #2. DAN-A vs. DEF-C
              subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(6, 15))
              HCPDISC_left_model2 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
              summary(HCPDISC_left_model2)
              # 6 < 15
            #3. LANG vs. DEF-C
              subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(5, 15))
              HCPDISC_left_model3 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
              summary(HCPDISC_left_model3)
              # NOT SIG
            #LEFT ORDER: 6, 15=5 (no sig. diff.)
              
          #RIGHT Comparisons (2, 8, 11, 12, 17)
              #1. 2 vs. 8
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(2, 8))
                HCPDISC_right_model1<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model1)
                  #8 < 2 
              #2. 2 vs. 11
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(2, 11))
                HCPDISC_right_model2<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model2)
                # 11 > 2
              #3. 2 vs. 12
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(2, 12))
                HCPDISC_right_model3<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model3)
                # NOT SIG
              #4. 2 vs. 17    
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(2, 17))
                HCPDISC_right_model4<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model4)
                # 17 > 2
              #5. 8 vs. 11
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(8, 11))
                HCPDISC_right_model5<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model5)
                # 11 > 8
              #6. 8 vs. 12  
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(8, 12))
                HCPDISC_right_model6<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model6)
                # 12 > 8
              #7. 8 vs. 17  
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(8, 17))
                HCPDISC_right_model7 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model7)
                #  17 > 8
              #8. 11 vs. 12
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(11, 12))
                HCPDISC_right_model8 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model8)
                # 12 < 11
              #9. 11 vs. 17
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(11, 17))
                HCPDISC_right_model9 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model9)
                # 17 > 11
              #10. 12 vs. 17
                subset_data <- subset(HCPDISC, subset = NewNetwork %in% c(12, 17))
                HCPDISC_right_model10 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPDISC_right_model10)
                # 17 > 12
              #RIGHT ORDER: 8, 2=12, 11, 17
              
                
    #HCP-REP Replication        
            #Subset to HCP-REP dataset
            HCPREP <- subset(study1, dataset=="HCP-REP")
                
          #LEFT Comparisons
              #1. LANG vs. DAN-A
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(5, 6))
                HCPREP_left_model1 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_left_model1)
                #6 < 5
              #2. DAN-A vs. DEF-C
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(6, 15))
                HCPREP_left_model2 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_left_model2)
                # 6 < 15
              #3. LANG vs. DEF-C
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(5, 15))
                HCPREP_left_model3 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_left_model3)
                # NOT SIG
                #LEFT ORDER: 6, 15=5 (no sig. diff.)  
                
          #RIGHT Comparisons (2, 8, 11, 12, 17)
              #1. 2 vs. 8
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(2, 8))
                HCPREP_right_model1<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model1)
                #8 < 2 
              #2. 2 vs. 11
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(2, 11))
                HCPREP_right_model2<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model2)
                # 11 > 2
              #3. 2 vs. 12
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(2, 12))
                HCPREP_right_model3<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model3)
                # NOT SIG
              #4. 2 vs. 17    
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(2, 17))
                HCPREP_right_model4<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model4)
                # 17 > 2
              #5. 8 vs. 11
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(8, 11))
                HCPREP_right_model5<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model5)
                # 11 > 8
              #6. 8 vs. 12  
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(8, 12))
                HCPREP_right_model6<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model6)
                # 12 > 8
              #7. 8 vs. 17  
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(8, 17))
                HCPREP_right_model7 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model7)
                #  17 > 8
              #8. 11 vs. 12
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(11, 12))
                HCPREP_right_model8 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model8)
                # 12 < 11
              #9. 11 vs. 17
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(11, 17))
                HCPREP_right_model9 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model9)
                # 17 > 11
              #10. 12 vs. 17
                subset_data <- subset(HCPREP, subset = NewNetwork %in% c(12, 17))
                HCPREP_right_model10 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPREP_right_model10)
                # 17 > 12
                #RIGHT ORDER: 8, 2=12, 11, 17
                
                
                
  #HCPD dataset      
        #Subset to HCPD dataset
        HCPD <- subset(study1, dataset=="HCPD")
                
        #LEFT Comparisons
              #1. LANG vs. DAN-A
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(5, 6))
                HCPD_left_model1 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_left_model1)
                #6 < 5
              #2. DAN-A vs. DEF-C
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(6, 15))
                HCPD_left_model2 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_left_model2)
                # 6 < 15
              #3. LANG vs. DEF-C
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(5, 15))
                HCPD_left_model3 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_left_model3)
                # NOT SIG
                #LEFT ORDER: 6, 15=5 (no sig. diff.)  
       
                
        #RIGHT Comparisons (2, 8, 11, 12, 17)
              #1. 2 vs. 8
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(2, 8))
                HCPD_right_model1<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model1)
                #8 < 2 
              #2. 2 vs. 11
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(2, 11))
                HCPD_right_model2<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model2)
                # 11 > 2
              #3. 2 vs. 12
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(2, 12))
                HCPD_right_model3<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model3)
                # NOT SIG
              #4. 2 vs. 17    
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(2, 17))
                HCPD_right_model4<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model4)
                # 17 > 2
              #5. 8 vs. 11
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(8, 11))
                HCPD_right_model5<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model5)
                # 11 > 8
              #6. 8 vs. 12  
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(8, 12))
                HCPD_right_model6<- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model6)
                # 12 > 8
              #7. 8 vs. 17  
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(8, 17))
                HCPD_right_model7 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model7)
                #  17 > 8
              #8. 11 vs. 12
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(11, 12))
                HCPD_right_model8 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model8)
                # 12 < 11
              #9. 11 vs. 17
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(11, 17))
                HCPD_right_model9 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model9)
                # 17 > 11
              #10. 12 vs. 17
                subset_data <- subset(HCPD, subset = NewNetwork %in% c(12, 17))
                HCPD_right_model10 <- lm(SA_LAT ~ NewNetwork + Age_Centered +Sex_Bin + FD_Centered + Handedness, data = subset_data)
                summary(HCPD_right_model10)
                # 17 > 12
                #RIGHT ORDER: 8, 2=12, 11, 17
                
                
#---------------------------NETWORK RELATIONSHIPS-FIGURES------------------------------
#SETUP
  #Load ALL .csv
  study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
  study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
                
#Model-adjusted NSAR for visual inspection (HCP, HCPD)
  study1$SA_LAT_ADJ <- NA
  ci_df <- data.frame(dataset=factor(),
                      NewNetwork=factor(),
                      CI_MIN=integer(),
                      CI_MAX=integer(),
                      MEAN=integer())
  
  dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
  for (d in dataset_list){
    subset1 <- subset(study1, dataset==d)
    
    for (i in 1:17) {
      # Subset the data based on NewNetwork value
      subset_data <- subset(subset1, NewNetwork == i)
      
      # Fit the linear regression model
      model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
      
      #Grab lm coefficients
      BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
      BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
      BETA_FD <- model[["coefficients"]][["FD_Centered"]]
      BETA_HAND <- model[["coefficients"]][["Handedness"]]
      
      #Grab means
      MEAN_AGE <- mean(subset_data$Age_Centered)
      MEAN_FD <- mean(subset_data$FD_Centered)
      MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
      MEAN_HAND <- mean(subset_data$Handedness)
      
      #IDs of the subset
      subsetted_ids <- subset_data$SUBJID
      
      #Find matching rows
      matching_rows <- study1$SUBJID %in% subsetted_ids &
        study1$NewNetwork %in% i
      
      #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
      study1$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
      subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
      
      #find mean
      MEAN <- mean(subset_data$SA_LAT_ADJ)
      
      n <- length(subset_data$SA_LAT_ADJ)
      std_dev <- sd(subset_data$SA_LAT_ADJ)
      std_err <- std_dev / sqrt(n)
      z_score=1.96
      margin_error <- z_score * std_err
      
      #lower bound
      CI_MIN <- MEAN - margin_error
      #upper bound
      CI_MAX <- MEAN + margin_error
      #Append CI data to dataframe
      row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
      names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
      ci_df <- rbind(ci_df, row_df)
    }
  }
  
#Reshape dataframe to WIDE
    study1_wide  <- reshape(study1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")

#At a most general level, is there a relationship between left- and right-lateralized networks?
    #HCP-DISC
      HCPDISC <- subset(study1_wide, dataset.1=="HCP-DISC")
      HCPDISC$LH_AVG_LAT <- rowMeans(HCPDISC[, c("SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.15")], na.rm = TRUE)
      HCPDISC$RH_AVG_LAT <- rowMeans(HCPDISC[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.8", "SA_LAT_ADJ.11", "SA_LAT_ADJ.12", "SA_LAT_ADJ.17")], na.rm = TRUE)
      cor.test(HCPDISC$LH_AVG_LAT, HCPDISC$RH_AVG_LAT)
    #HCP-REP
      HCPREP <- subset(study1_wide, dataset.1=="HCP-REP")
      HCPREP$LH_AVG_LAT <- rowMeans(HCPREP[, c("SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.15")], na.rm = TRUE)
      HCPREP$RH_AVG_LAT <- rowMeans(HCPREP[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.8", "SA_LAT_ADJ.11", "SA_LAT_ADJ.12", "SA_LAT_ADJ.17")], na.rm = TRUE)
      cor.test(HCPREP$LH_AVG_LAT, HCPREP$RH_AVG_LAT)
    #HCPD
      HCPD <- subset(study1_wide, dataset.1=="HCPD")
      HCPD$LH_AVG_LAT <- rowMeans(HCPD[, c("SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.15")], na.rm = TRUE)
      HCPD$RH_AVG_LAT <- rowMeans(HCPD[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.8", "SA_LAT_ADJ.11", "SA_LAT_ADJ.12", "SA_LAT_ADJ.17")], na.rm = TRUE)
      cor.test(HCPD$LH_AVG_LAT, HCPD$RH_AVG_LAT)
      
      
#Correlation values to report
    #HCP-DISC
    HCPDISC <- subset(study1_wide, dataset.1=="HCP-DISC")
    comp1 <- cor.test(HCPDISC$SA_LAT_ADJ.17, HCPDISC$SA_LAT_ADJ.6, method="pearson")
    comp2 <- cor.test(HCPDISC$SA_LAT_ADJ.17, HCPDISC$SA_LAT_ADJ.15, method="pearson")   
    comp3 <- cor.test(HCPDISC$SA_LAT_ADJ.15, HCPDISC$SA_LAT_ADJ.2, method="pearson")
    comp4 <- cor.test(HCPDISC$SA_LAT_ADJ.15, HCPDISC$SA_LAT_ADJ.11, method = "pearson")
    comp5 <- cor.test(HCPDISC$SA_LAT_ADJ.15, HCPDISC$SA_LAT_ADJ.12, method="pearson")
    comp6 <- cor.test(HCPDISC$SA_LAT_ADJ.11, HCPDISC$SA_LAT_ADJ.5, method="pearson")
    comp7 <- cor.test(HCPDISC$SA_LAT_ADJ.5, HCPDISC$SA_LAT_ADJ.8, method="pearson")
    comp8 <- cor.test(HCPDISC$SA_LAT_ADJ.6, HCPDISC$SA_LAT_ADJ.5, method="pearson")
    
#Linear models to report in correlation scatterplots figure caption
    #HCP-DISC
      HCPDISC <- subset(study1_wide, dataset.1=="HCP-DISC")
    
      comp1 <- cor.test(HCPDISC$SA_LAT_ADJ.17, HCPDISC$SA_LAT_ADJ.6, method="pearson")
      lm1 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.6, data=HCPDISC)
      summary(lm1)
      
      comp2 <- cor.test(HCPDISC$SA_LAT_ADJ.17, HCPDISC$SA_LAT_ADJ.15, method = "pearson")
      lm2 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.15, data=HCPDISC)
      summary(lm2)
    
      comp3 <- cor.test(HCPDISC$SA_LAT_ADJ.6, HCPDISC$SA_LAT_ADJ.5, method = "pearson")
      lm3 <- lm(SA_LAT_ADJ.6~SA_LAT_ADJ.5, data=HCPDISC)
      summary(lm3)
    
      comp4 <- cor.test(HCPDISC$SA_LAT_ADJ.8, HCPDISC$SA_LAT_ADJ.5, method = "pearson")
      lm4 <- lm(SA_LAT_ADJ.8~SA_LAT_ADJ.5, data=HCPDISC)
      summary(lm4)
      
    #HCP-REP
      HCPREP <- subset(study1_wide, dataset.1=="HCP-REP")
      
      comp1 <- cor.test(HCPREP$SA_LAT_ADJ.17, HCPREP$SA_LAT_ADJ.6, method="pearson")
      lm1 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.6, data=HCPREP)
      summary(lm1)
      
      comp2 <- cor.test(HCPREP$SA_LAT_ADJ.17, HCPREP$SA_LAT_ADJ.15, method = "pearson")
      lm2 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.15, data=HCPREP)
      summary(lm2)
      
      comp3 <- cor.test(HCPREP$SA_LAT_ADJ.6, HCPREP$SA_LAT_ADJ.5, method = "pearson")
      lm3 <- lm(SA_LAT_ADJ.6~SA_LAT_ADJ.5, data=HCPREP)
      summary(lm3)
      
      comp4 <- cor.test(HCPREP$SA_LAT_ADJ.8, HCPREP$SA_LAT_ADJ.5, method = "pearson")
      lm4 <- lm(SA_LAT_ADJ.8~SA_LAT_ADJ.5, data=HCPREP)
      summary(lm4)
      
    #HCPD
      HCPD <- subset(study1_wide, dataset.1=="HCPD")
      
      comp1 <- cor.test(HCPD$SA_LAT_ADJ.17, HCPD$SA_LAT_ADJ.6, method="pearson")
      lm1 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.6, data=HCPD)
      summary(lm1)
      
      comp2 <- cor.test(HCPD$SA_LAT_ADJ.17, HCPD$SA_LAT_ADJ.15, method = "pearson")
      lm2 <- lm(SA_LAT_ADJ.17~SA_LAT_ADJ.15, data=HCPD)
      summary(lm2)
      
      comp3 <- cor.test(HCPD$SA_LAT_ADJ.6, HCPD$SA_LAT_ADJ.5, method = "pearson")
      lm3 <- lm(SA_LAT_ADJ.6~SA_LAT_ADJ.5, data=HCPD)
      summary(lm3)
      
      comp4 <- cor.test(HCPD$SA_LAT_ADJ.8, HCPD$SA_LAT_ADJ.5, method = "pearson")
      lm4 <- lm(SA_LAT_ADJ.8~SA_LAT_ADJ.5, data=HCPD)
      summary(lm4)
    
#HCP-DISC CORRELATION MATRIX
            HCPDISC_wide <- subset(study1_wide, dataset.1=="HCP-DISC")
                #selected_vars <- HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                #                              "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                #                              "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                #                              "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                selected_vars <- HCPDISC_wide[, c("SA_LAT_ADJ.17", "SA_LAT_ADJ.12", 
                                                  "SA_LAT_ADJ.2", "SA_LAT_ADJ.8",
                                                  "SA_LAT_ADJ.11", "SA_LAT_ADJ.5",
                                                  "SA_LAT_ADJ.15", "SA_LAT_ADJ.6")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #Identify upper and lower triangles
                upper_triangle <- cor_matrix
                upper_triangle[lower.tri(upper_triangle)] <- 5000
                upper_data <- as.data.frame(as.table(upper_triangle))
                colnames(upper_data) <- c("Variable1", "Variable2", "Correlation")
                upper_data$Triangle <- "UpperTriangle"
                upper_data <- subset(upper_data, Correlation!=5000)
                
                lower_triangle <- cor_matrix
                lower_triangle[upper.tri(lower_triangle)] <- 5000
                lower_data <- as.data.frame(as.table(lower_triangle))
                colnames(lower_data) <- c("Variable1", "Variable2", "Correlation")
                lower_data$Triangle <- "LowerTriangle"
                lower_data <- subset(lower_data, Correlation!=5000)
                
                #Combine both upper and lower
                cor_data <- merge(lower_data, upper_data, by=c("Variable1", "Variable2", "Correlation", "Triangle"), all=TRUE)
                
                
                #Fig. 1: Heatmap (HCP only)
                # Convert the correlation matrix to a tidy format
                #cor_data <- as.data.frame(as.table(cor_matrix))
                #colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")

                #bonferroni correction threshold
                #r_value <- 3.8348/(sqrt(276-3)) #Bonferroni correction = .05/64 = .000078. Critical z-value = 3.8348
                
                #using p=.05
                r_value <- 1.96/(sqrt(276-3))
                
                #Maximum corr value excluding 1
                  filtered_vector <- cor_data$Correlation[cor_data$Correlation != 1]
                  max_value_excluding_1 <- max(abs(filtered_vector))
                  min_value <- min(filtered_vector)
                  
                #Set ones to NA
                  cor_data$Correlation <- ifelse(cor_data$Correlation==1, NA, cor_data$Correlation)
                
                #Visualize the NSAR matrix using ggplot heatmap 
                color_breaks <- seq(-.6, .6, by = 0.2)
                # Create the ggplot heatmap with reordered axis labels
                ggplot(cor_data, aes(x = as.factor(Variable1), y = as.factor(Variable2), fill = Correlation)) +
                  geom_tile(color = "white",
                            lwd = 1,
                            linetype = 1) +
                  scale_fill_gradient(low = "#E69F00", high = "white", na.value="white", breaks=color_breaks, labels=round(color_breaks,2), limits=c(-.6, round(max_value_excluding_1,2))) +
                  geom_text(aes(Variable1, Variable2, label = ifelse(abs(round(Correlation,2)) < r_value & Triangle!="LowerTriangle", "", round(Correlation, 2))), color = "black", size = 1.75) +
                  geom_text(aes(label = ifelse(is.na(Correlation), "1", "")), color = "black", size = 1.75) +
                  #scale_x_discrete(labels=c("VIS-B","LANG", "DAN-A","SAL-A", "CTRL-B", "CTRL-C", "DEF-C", "LIM-B")) +
                  #scale_y_discrete(labels=c("VIS-B","LANG", "DAN-A","SAL-A", "CTRL-B", "CTRL-C", "DEF-C", "LIM-B")) +
                  scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  scale_y_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  xlab("") +
                  ylab("") +
                  labs(fill = "")+
                  theme(
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 10),
                    legend.position = "right",
                    legend.key.height = unit(1, "cm"),
                    legend.key.width = unit(.1, "cm"),
                    axis.text.y = element_text(colour = "black", hjust = 1),
                    axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                  )
                ggsave(filename = paste("Study1_HCP-DISC_NSAR_CORRMATRIX_230701.png"), width = 3.35, height = 3,
                       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
                
                
#HCP-REP CORRELATION MATRIX
                HCPREP_wide <- subset(study1_wide, dataset.1=="HCP-REP")
                selected_vars <- HCPREP_wide[, c("SA_LAT_ADJ.17", "SA_LAT_ADJ.12", 
                                                  "SA_LAT_ADJ.2", "SA_LAT_ADJ.8",
                                                  "SA_LAT_ADJ.11", "SA_LAT_ADJ.5",
                                                  "SA_LAT_ADJ.15", "SA_LAT_ADJ.6")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #Identify upper and lower triangles
                upper_triangle <- cor_matrix
                upper_triangle[lower.tri(upper_triangle)] <- 5000
                upper_data <- as.data.frame(as.table(upper_triangle))
                colnames(upper_data) <- c("Variable1", "Variable2", "Correlation")
                upper_data$Triangle <- "UpperTriangle"
                upper_data <- subset(upper_data, Correlation!=5000)
                
                lower_triangle <- cor_matrix
                lower_triangle[upper.tri(lower_triangle)] <- 5000
                lower_data <- as.data.frame(as.table(lower_triangle))
                colnames(lower_data) <- c("Variable1", "Variable2", "Correlation")
                lower_data$Triangle <- "LowerTriangle"
                lower_data <- subset(lower_data, Correlation!=5000)
                
                #Combine both upper and lower
                cor_data <- merge(lower_data, upper_data, by=c("Variable1", "Variable2", "Correlation", "Triangle"), all=TRUE)
                
                
              #Fig. 1: Heatmap 
                # Convert the correlation matrix to a tidy format
                #cor_data <- as.data.frame(as.table(cor_matrix))
                #colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
                
                #bonferroni correction threshold
                #r_value <- 3.8346/(sqrt(277-3)) #Bonferroni correction = .05/64 = .000078. Critical z-value = 3.8346
                
                #using p=.05
                r_value <- 1.96/(sqrt(277-3))
                
                #Maximum corr value excluding 1
                filtered_vector <- cor_data$Correlation[cor_data$Correlation != 1]
                max_value_excluding_1 <- max(abs(filtered_vector))
                min_value <- min(filtered_vector)
                
                #Set ones to NA
                cor_data$Correlation <- ifelse(cor_data$Correlation==1, NA, cor_data$Correlation)
                
                
                #Visualize the NSAR matrix using ggplot heatmap 
                color_breaks <- seq(-.6, .4, by = 0.2)
                # Create the ggplot heatmap with reordered axis labels
                ggplot(cor_data, aes(x = as.factor(Variable1), y = as.factor(Variable2), fill = Correlation)) +
                  geom_tile(color = "white",
                            lwd = 1,
                            linetype = 1) +
                  scale_fill_gradient(low = "#D55E00", high = "white", na.value="white", breaks=color_breaks, labels=round(color_breaks,2), limits=c(-.6, round(max_value_excluding_1,2))) +
                  geom_text(aes(Variable1, Variable2, label = ifelse(abs(round(Correlation,2)) < r_value & Triangle!="LowerTriangle", "", round(Correlation, 2))), color = "black", size = 1.75) +
                  geom_text(aes(label = ifelse(is.na(Correlation), "1", "")), color = "black", size = 1.75) +
                  scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  scale_y_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  xlab("") +
                  ylab("") +
                  labs(fill = "")+
                  theme(
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 10),
                    legend.position = "right",
                    legend.key.height = unit(1, "cm"),
                    legend.key.width = unit(.1, "cm"),
                    axis.text.y = element_text(colour = "black", hjust = 1),
                    axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                  )
                ggsave(filename = paste("Study1_HCP-REP_NSAR_CORRMATRIX_230701.png"), width = 3.35, height = 3,
                       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
                
                
                
                
#HCPD CORRELATION MATRIX
                HCPD_wide <- subset(study1_wide, dataset.1=="HCPD")
                selected_vars <- HCPD_wide[, c("SA_LAT_ADJ.17", "SA_LAT_ADJ.12", 
                                                 "SA_LAT_ADJ.2", "SA_LAT_ADJ.8",
                                                 "SA_LAT_ADJ.11", "SA_LAT_ADJ.5",
                                                 "SA_LAT_ADJ.15", "SA_LAT_ADJ.6")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #Identify upper and lower triangles
                upper_triangle <- cor_matrix
                upper_triangle[lower.tri(upper_triangle)] <- 5000
                upper_data <- as.data.frame(as.table(upper_triangle))
                colnames(upper_data) <- c("Variable1", "Variable2", "Correlation")
                upper_data$Triangle <- "UpperTriangle"
                upper_data <- subset(upper_data, Correlation!=5000)
                
                lower_triangle <- cor_matrix
                lower_triangle[upper.tri(lower_triangle)] <- 5000
                lower_data <- as.data.frame(as.table(lower_triangle))
                colnames(lower_data) <- c("Variable1", "Variable2", "Correlation")
                lower_data$Triangle <- "LowerTriangle"
                lower_data <- subset(lower_data, Correlation!=5000)
                
                #Combine both upper and lower
                cor_data <- merge(lower_data, upper_data, by=c("Variable1", "Variable2", "Correlation", "Triangle"), all=TRUE)
                
                
                
                #Fig. 1: Heatmap 
                # Convert the correlation matrix to a tidy format
                #cor_data <- as.data.frame(as.table(cor_matrix))
                #colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
                
                #bonferroni correction threshold
                #r_value <- 3.8242/(sqrt(343-3)) #Bonferroni correction = .05/64 = .000078. Critical z-value = 3.8242
                
                #using p=.05
                r_value <- 1.96/(sqrt(343-3))
                
                #Maximum corr value excluding 1
                filtered_vector <- cor_data$Correlation[cor_data$Correlation != 1]
                max_value_excluding_1 <- max(abs(filtered_vector))
                min_value <- min(filtered_vector)
                
                #Set ones to NA
                cor_data$Correlation <- ifelse(cor_data$Correlation==1, NA, cor_data$Correlation)
                
                #Visualize the NSAR matrix using ggplot heatmap 
                color_breaks <- seq(-.6, .4, by = 0.2)
                
                # Create the ggplot heatmap with reordered axis labels
                ggplot(cor_data, aes(x = as.factor(Variable1), y = as.factor(Variable2), fill = Correlation)) +
                  geom_tile(color = "white",
                            lwd = 1,
                            linetype = 1) +
                  scale_fill_gradient(low = "#0072B2", high = "white", na.value="white", breaks=color_breaks, labels=round(color_breaks,2), limits=c(-.6, round(max_value_excluding_1,2))) +
                  geom_text(aes(Variable1, Variable2, label = ifelse(abs(round(Correlation,2)) < r_value & Triangle!="LowerTriangle", "", round(Correlation, 2))), color = "black", size = 1.75) +
                  geom_text(aes(label = ifelse(is.na(Correlation), "1", "")), color = "black", size = 1.75) +
                  scale_x_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  scale_y_discrete(labels=c("LIM-B", "CTRL-C", "VIS-B", "SAL-A", "CTRL-B", "LANG", "DEF-C", "DAN-A")) +
                  xlab("") +
                  ylab("") +
                  labs(fill = "")+
                  theme(
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 10),
                    legend.position = "right",
                    legend.key.height = unit(1, "cm"),
                    legend.key.width = unit(.1, "cm"),
                    axis.text.y = element_text(colour = "black", hjust = 1),
                    axis.text.x = element_text(colour = "black", hjust = 1, vjust=1, angle = 40),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 1, linetype = "solid")
                  )
                ggsave(filename = paste("Study1_HCPD_NSAR_CORRMATRIX_230701.png"), width = 3.35, height = 3,
                       path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
                
                
#SCATTERPLOTS: exemplary networks (17&6, 17&15, 6&11, 6&5)                
                comp_networks=c("6", "15")
                network_names=c("Dorsal Attention-A", "Default-C")
                Group_Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")

                study1_wide$SUBJID <- as.factor(study1_wide$SUBJID)
                count=0
                for (n in comp_networks) {
                  count=count+1
                  filename <- paste("Study1_CorrPlots_LimbicBvs_", "Network", n, "_230603.png", sep = "")
                  x_title <- paste(network_names[count], " NSAR", sep="")
                  x_var <- paste("SA_LAT_ADJ.", n, sep = "")
                  plot_title <- paste(network_names[count])
                  
                  ggplot(study1_wide, aes(x = get(x_var), y = SA_LAT_ADJ.17, color=dataset.1)) +
                    labs(x = x_title, y = "Limbic-B NSAR") +
                    #ggtitle(plot_title)+
                    labs(fill = " ", color = " ") +
                    geom_point(aes(fill = dataset.1),colour="black", pch = 21) +
                    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                    geom_smooth(method = "lm", aes(fill=dataset.1), size = 0.75, se = FALSE) +
                    #scale_y_continuous(limits = c(-.5, 1.03)) +
                    #scale_x_continuous(limits = c(-1, .5)) +
                    #stat_cor(method="spearman", color="black")+
                    scale_color_manual(values = Group_Palette) +
                    scale_fill_manual(values = Group_Palette) +
                    theme_bw() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          axis.title = element_text(colour = "black", size = 12),
                          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                          axis.text.x = element_text(colour = "black"),
                          legend.position = "none",
                          legend.title = element_text(colour = "black", size = 12),
                          legend.text = element_text(colour = "black", size = 12),
                          legend.background = element_rect(fill = "white", size = 0.5),
                          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                          panel.border = element_blank(),
                          panel.background = element_blank())
                  
                  ggsave(filename = filename, width = 3.35, height = 3.35,
                         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
                }          
                
                
                
                #DEF-C vs. CTRL-B
                
                comp_networks=c("11")
                network_names=c("Control-B")
                Group_Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
                
                study1_wide$SUBJID <- as.factor(study1_wide$SUBJID)
                count=0
                for (n in comp_networks) {
                  count=count+1
                  filename <- paste("Study1_CorrPlots_DEFCvs_", "Network", n, "_230603.png", sep = "")
                  x_title <- paste(network_names[count], " NSAR", sep="")
                  x_var <- paste("SA_LAT_ADJ.", n, sep = "")
                  plot_title <- paste(network_names[count])
                  
                  ggplot(study1_wide, aes(x = get(x_var), y = SA_LAT_ADJ.15, color=dataset.1)) +
                    labs(x = x_title, y = "Default-C NSAR") +
                    #ggtitle(plot_title)+
                    labs(fill = " ", color = " ") +
                    geom_point(aes(fill = dataset.1),colour="black", pch = 21) +
                    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                    geom_smooth(method = "lm", aes(fill=dataset.1), size = 0.75, se = FALSE) +
                    #scale_y_continuous(limits = c(-.5, 1.03)) +
                    #scale_x_continuous(limits = c(-1, .5)) +
                    #stat_cor(method="spearman", color="black")+
                    scale_color_manual(values = Group_Palette) +
                    scale_fill_manual(values = Group_Palette) +
                    theme_bw() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          axis.title = element_text(colour = "black", size = 12),
                          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                          axis.text.x = element_text(colour = "black"),
                          legend.position = "none",
                          legend.title = element_text(colour = "black", size = 12),
                          legend.text = element_text(colour = "black", size = 12),
                          legend.background = element_rect(fill = "white", size = 0.5),
                          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                          panel.border = element_blank(),
                          panel.background = element_blank())
                  
                  ggsave(filename = filename, width = 3.35, height = 3.35,
                         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
                }         
                
                
              #LANG vs. CTRL-B
                comp_networks=c("11")
                network_names=c("Control-B")
                Group_Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
                
                study1_wide$SUBJID <- as.factor(study1_wide$SUBJID)
                count=0
                for (n in comp_networks) {
                  count=count+1
                  filename <- paste("Study1_CorrPlots_LANGvs_", "Network", n, "_231202.png", sep = "")
                  x_title <- paste(network_names[count], " NSAR", sep="")
                  x_var <- paste("SA_LAT_ADJ.", n, sep = "")
                  plot_title <- paste(network_names[count])
                  
                  ggplot(study1_wide, aes(x = get(x_var), y = SA_LAT_ADJ.5, color=dataset.1)) +
                    labs(x = x_title, y = "Language NSAR") +
                    #ggtitle(plot_title)+
                    labs(fill = " ", color = " ") +
                    geom_point(aes(fill = dataset.1),colour="black", pch = 21) +
                    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                    geom_smooth(method = "lm", aes(fill=dataset.1), size = 0.75, se = FALSE) +
                    #scale_y_continuous(limits = c(-.5, 1.03)) +
                    #scale_x_continuous(limits = c(-1, .5)) +
                    #stat_cor(method="spearman", color="black")+
                    scale_color_manual(values = Group_Palette) +
                    scale_fill_manual(values = Group_Palette) +
                    theme_bw() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          axis.title = element_text(colour = "black", size = 12),
                          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                          axis.text.x = element_text(colour = "black"),
                          legend.position = "none",
                          legend.title = element_text(colour = "black", size = 12),
                          legend.text = element_text(colour = "black", size = 12),
                          legend.background = element_rect(fill = "white", size = 0.5),
                          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                          panel.border = element_blank(),
                          panel.background = element_blank())
                  
                  ggsave(filename = filename, width = 3.35, height = 3.35,
                         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
                }         
                
                
                
                #LANG vs. SAL-A
                comp_networks=c("8")
                network_names=c("Salience/VenAttn-A")
                Group_Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
                
                study1_wide$SUBJID <- as.factor(study1_wide$SUBJID)
                count=0
                for (n in comp_networks) {
                  count=count+1
                  filename <- paste("Study1_CorrPlots_LANGvs_", "Network", n, "_231202.png", sep = "")
                  x_title <- paste(network_names[count], " NSAR", sep="")
                  x_var <- paste("SA_LAT_ADJ.", n, sep = "")
                  plot_title <- paste(network_names[count])
                  
                  ggplot(study1_wide, aes(x = get(x_var), y = SA_LAT_ADJ.5, color=dataset.1)) +
                    labs(x = x_title, y = "Language NSAR") +
                    #ggtitle(plot_title)+
                    labs(fill = " ", color = " ") +
                    geom_point(aes(fill = dataset.1),colour="black", pch = 21) +
                    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                    geom_smooth(method = "lm", aes(fill=dataset.1), size = 0.75, se = FALSE) +
                    #scale_y_continuous(limits = c(-.5, 1.03)) +
                    #scale_x_continuous(limits = c(-1, .5)) +
                    #stat_cor(method="spearman", color="black")+
                    scale_color_manual(values = Group_Palette) +
                    scale_fill_manual(values = Group_Palette) +
                    theme_bw() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          axis.title = element_text(colour = "black", size = 12),
                          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                          axis.text.x = element_text(colour = "black"),
                          legend.position = "none",
                          legend.title = element_text(colour = "black", size = 12),
                          legend.text = element_text(colour = "black", size = 12),
                          legend.background = element_rect(fill = "white", size = 0.5),
                          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                          panel.border = element_blank(),
                          panel.background = element_blank())
                  
                  ggsave(filename = filename, width = 3.35, height = 3.35,
                         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
                }         
                
                
                
                
                
                #DAN-A vs LANG
                
                comp_networks=c("6")
                network_names=c("Dorsal Attention-A")
                Group_Palette <- c("#E69F00", "#D55E00", "#0072B2",  "#009E73", "#C9FFE1")
                
                study1_wide$SUBJID <- as.factor(study1_wide$SUBJID)
                count=0
                for (n in comp_networks) {
                  count=count+1
                  filename <- paste("Study1_CorrPlots_DANAvs_", "Network", n, "_230725.png", sep = "")
                  x_title <- paste(network_names[count], " NSAR", sep="")
                  x_var <- paste("SA_LAT_ADJ.", n, sep = "")
                  plot_title <- paste(network_names[count])
                  
                  ggplot(study1_wide, aes(x = get(x_var), y = SA_LAT_ADJ.5, color=dataset.1)) +
                    labs(x = x_title, y = "Language NSAR") +
                    #ggtitle(plot_title)+
                    labs(fill = " ", color = " ") +
                    geom_point(aes(fill = dataset.1),colour="black", pch = 21) +
                    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                    geom_smooth(method = "lm", aes(fill=dataset.1), size = 0.75, se = FALSE) +
                    #scale_y_continuous(limits = c(-.5, 1.03)) +
                    #scale_x_continuous(limits = c(-1, .5)) +
                    #stat_cor(method="spearman", color="black")+
                    scale_color_manual(values = Group_Palette) +
                    scale_fill_manual(values = Group_Palette) +
                    theme_bw() +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          axis.title = element_text(colour = "black", size = 12),
                          axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6),
                          axis.text.x = element_text(colour = "black"),
                          legend.position = "none",
                          legend.title = element_text(colour = "black", size = 12),
                          legend.text = element_text(colour = "black", size = 12),
                          legend.background = element_rect(fill = "white", size = 0.5),
                          axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                          panel.border = element_blank(),
                          panel.background = element_blank())
                  
                  ggsave(filename = filename, width = 3.35, height = 3.35,
                         path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
                }           
                                
#ODDS & ENDS
                #Threshold!
                r_value <- 3.58/(sqrt(232-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.58
                cor_matrix[abs(cor_matrix) < r_value] <- 0 #removes values less than .25
                
                
                
                
      #Fig. 2: Plot the correlation matrix as a scatterplot grid
          # Convert correlation matrix to long format
          cor_data <- melt(cor_matrix)
          
          #Format cor_data
          cor_data$X1 <- gsub("^.{0,7}", "", cor_data$X1) #remove string
          cor_data$X2 <- gsub("^.{0,7}", "", cor_data$X2) #remove string
          
          
          # Filter correlation data for specific variable
          filtered_data <- subset(cor_data, X1 != X2)  # Exclude diagonal elements
          
          #Convert to factor
          filtered_data$X1 <- as.factor(filtered_data$X1)
          filtered_data$X2 <- as.factor(filtered_data$X2)
          
        # Create a list to store scatterplot objects
          scatterplot_list <- list()
          
          # Define the labels vector
          labels <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
          
          # Loop through each unique variable in Var1
          for (var in seq_along(unique(filtered_data$X1))) {            # Filter data for current variable
            var_data <- subset(filtered_data, X1 == var)
            
            # Get the current label
            label <- labels[var]
            
            # Create scatterplot for current variable
            scatterplot <- ggplot(data = var_data, aes(x = factor(X2, level=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")), y = value)) +
              #scale_colour_manual(values=CBIG_Palette)+
              #scale_fill_manual(values=CBIG_Palette)+
              geom_point(aes(fill=value), colour="black", pch=21)+
              scale_y_continuous(limits = c(-.41, .22))+
              #scale_x_discrete(labels=c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")) +
              labs(x = "Network", y = "Correlation") +
              ggtitle(paste(label)) +
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
              theme(plot.title = element_text(size = 10), axis.title = element_text(colour = "black", size=10), axis.text.y =element_text(colour = "black", angle = 90, hjust = 0.6, size=8), 
                    axis.text.x =element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    legend.position = "none", legend.title=element_text(colour = "black", size = 10), legend.text=element_text(colour = "black", size = 10), 
                    legend.background = element_rect(fill="white", size=0.5) , axis.line = element_line(colour = "black", size = 1, linetype = "solid"), 
                    axis.ticks = element_line(colour = "black", size =1, linetype ="solid"), panel.border = element_blank(), panel.background = element_blank())
            
            # Add scatterplot to the list
            scatterplot_list[[length(scatterplot_list) + 1]] <- scatterplot
          }
          
          # Arrange scatterplots in a grid
          grid_arranged <- grid.arrange(grobs = scatterplot_list, ncol = 3)
          ggsave(filename = paste("Study1_HCP_SAI_Corr_Scatterplot_230522.png"), grid_arranged, width = 8.5, height = 8.5,
                 path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures", dpi = 300)
          
          
   
    #Fig. 3: Network graph
    #HCP dataset      
          #Correlation matrix (HCP only)
          # Select the variables you want to include in the correlation matrix
          selected_vars <- HCP_wide[, c("SA_LAT.1", "SA_LAT.2", "SA_LAT.3", "SA_LAT.4", "SA_LAT.5", 
                                        "SA_LAT.6", "SA_LAT.7", "SA_LAT.8", "SA_LAT.9", "SA_LAT.10",
                                        "SA_LAT.11", "SA_LAT.12", "SA_LAT.13", "SA_LAT.14",
                                        "SA_LAT.15", "SA_LAT.16", "SA_LAT.17")]
          
          # Compute the correlation matrix
          cor_matrix <- cor(selected_vars)
          cor_matrix[cor_matrix == 1.0] <- 0 #removes self-looping values
          
          #Format for circlize
          adj_matrix <- as.matrix(cor_matrix)
          
          
          # Create a chord diagram using circlize
          CBIG_Palette <- c('Visual-A'="#602368", 'Visual-B'="#DC1F26", 'Somatomotor-A'="#4582A7", 'Somatomotor-B'="#21B88B", 'Language'="#32489E", 'Dorsal Attention-A'="#4A9644", 'Dorsal Attention-B'="#106A36", 'Salience/VenAttn-A'="#833C98", 'Salience/VentAttn-B'="#E383B1", 'Control-A'="#CC8229", 'Control-B'="#7B2C3F", 'Control-C'="#6F809A", 'Default-A'="#E3E218", 'Default-B'="#A9313D", 'Default-C'="#1C1B55", 'Limbic-A'="#40471D", 'Limbic-B'="#BCCF7E")

          row_names <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
          col_names <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
          
          rownames(adj_matrix) <- row_names
          colnames(adj_matrix) <- col_names
          
          #Chord diagram as .svg for HCP dataset
          CBIG_Palette <- c('Visual-A'="#602368", 'Visual-B'="#DC1F26", 'Somatomotor-A'="#4582A7", 'Somatomotor-B'="#21B88B", 'Language'="#32489E", 'Dorsal Attention-A'="#4A9644", 'Dorsal Attention-B'="#106A36", 'Salience/VenAttn-A'="#833C98", 'Salience/VentAttn-B'="#E383B1", 'Control-A'="#CC8229", 'Control-B'="#7B2C3F", 'Control-C'="#6F809A", 'Default-A'="#E3E218", 'Default-B'="#A9313D", 'Default-C'="#1C1B55", 'Limbic-A'="#40471D", 'Limbic-B'="#BCCF7E")
          svg("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/Study1_HCP_ChordDiagram_230517.svg") 
          tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = c("grid","name"))
          #Below: code for radiating label names
          #circos.track(track.index = 1, panel.fun = function(x, y) {
          #  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
          #              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          #}, bg.border = NA)
          dev.off()
          

       #Fig.3: Chord diagram   
          #Filter based on correlation strength
          r_value <- 3.58/(sqrt(232-3)) #Bonferroni correction = .05/289 = .00017. Critical z-value = 3.58
          cor_matrix[abs(cor_matrix) < r_value] <- 0 #removes values less than .25
          #Format for circlize
          adj_matrix <- as.matrix(cor_matrix)
          CBIG_Palette <- c('Visual-A'="#602368", 'Visual-B'="#DC1F26", 'Somatomotor-A'="#4582A7", 'Somatomotor-B'="#21B88B", 'Language'="#32489E", 'Dorsal Attention-A'="#4A9644", 'Dorsal Attention-B'="#106A36", 'Salience/VenAttn-A'="#833C98", 'Salience/VentAttn-B'="#E383B1", 'Control-A'="#CC8229", 'Control-B'="#7B2C3F", 'Control-C'="#6F809A", 'Default-A'="#E3E218", 'Default-B'="#A9313D", 'Default-C'="#1C1B55", 'Limbic-A'="#40471D", 'Limbic-B'="#BCCF7E")
          row_names <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
          col_names <- c("Visual-A", "Visual-B", "Somatomotor-A", "Somatomotor-B", "Language", "Dorsal Attention-A", "Dorsal Attention-B", "Salience/VenAttn-A", "Salience/VentAttn-B", "Control-A", "Control-B", "Control-C", "Default-A", "Default-B", "Default-C", "Limbic-A", "Limbic-B")
          
          rownames(adj_matrix) <- row_names
          colnames(adj_matrix) <- col_names
          
          #Chord diagram as .svg for HCP dataset
          svg("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/Study1_HCP_ChordDiagram_Filtered.25_230522.svg") 
          #Below: code for no labels
          #tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = "grid", 
          #                    preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(adj_matrix))))))
          #Below: code for network labels
          tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = c("grid", "name"))
          #Below: code for radiating label names
          #circos.track(track.index = 1, panel.fun = function(x, y) {
          #  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
          #              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          #}, bg.border = NA)
          dev.off()
          
          
          
          
          #Filter to negative connections
          CBIG_Palette <- c('Visual-A'="#602368", 'Visual-B'="#DC1F26", 'Somatomotor-A'="#4582A7", 'Somatomotor-B'="#21B88B", 'Language'="#32489E", 'Dorsal Attention-A'="#4A9644", 'Dorsal Attention-B'="#106A36", 'Salience/VenAttn-A'="#833C98", 'Salience/VentAttn-B'="#E383B1", 'Control-A'="#CC8229", 'Control-B'="#7B2C3F", 'Control-C'="#6F809A", 'Default-A'="#E3E218", 'Default-B'="#A9313D", 'Default-C'="#1C1B55", 'Limbic-A'="#40471D", 'Limbic-B'="#BCCF7E")
          CBIG_Palette[adj_matrix == 0] = "#00000000"

          #Chord diagram as .svg for HCP dataset
          svg("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/Study1_HCP_ChordDiagram_FilteredNeg_230518.svg") 
          #Below: code for no labels
          tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = "grid", 
                              preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(adj_matrix))))))
          #Below: code for network labels
          #tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = c("grid","name"))
          #Below: code for radiating label names
          #circos.track(track.index = 1, panel.fun = function(x, y) {
          #  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
          #              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          #}, bg.border = NA)
          dev.off()
          
          
          #Filter to positive connections
          CBIG_Palette <- c('Visual-A'="#602368", 'Visual-B'="#DC1F26", 'Somatomotor-A'="#4582A7", 'Somatomotor-B'="#21B88B", 'Language'="#32489E", 'Dorsal Attention-A'="#4A9644", 'Dorsal Attention-B'="#106A36", 'Salience/VenAttn-A'="#833C98", 'Salience/VentAttn-B'="#E383B1", 'Control-A'="#CC8229", 'Control-B'="#7B2C3F", 'Control-C'="#6F809A", 'Default-A'="#E3E218", 'Default-B'="#A9313D", 'Default-C'="#1C1B55", 'Limbic-A'="#40471D", 'Limbic-B'="#BCCF7E")
          CBIG_Palette[adj_matrix > 0] = "#00000000"
          
          #Chord diagram as .svg for HCP dataset
          svg("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/svg_figures/Study1_HCP_ChordDiagram_FilteredPos_230518.svg") 
          tmp <- chordDiagram(adj_matrix, grid.col = CBIG_Palette, annotationTrack = "grid", 
                              preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(adj_matrix))))))
          #Below: code for radiating label names
          #circos.track(track.index = 1, panel.fun = function(x, y) {
          #  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
          #              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          #}, bg.border = NA)
          dev.off()
          
          
        
#---------------------------------CONVERGENT VALIDITY: AI--------------------------------
        
#Analysis description: A Spearman’s rank correlation coefficient was used to compare the autonomy index and NSAR.
#Three left-specialized networks (Language, Dorsal Attention-A, and Control-B) determined a priori. 
#Three right-specialized networks (Limbic-B, Visual-B, and Ventral Attention-A) 
#Bonferroni-corrected alpha level of 0.008         
        
        
#SETUP
        #Load HCP SAI, AI
        HCP_AI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/ai_spec/ALL/avg_ai/MSHBM_LONG_AVG_AI_HCP_ALL_230221.csv")
        HCP_SA <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/ALL/HCP_ALL_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
        HCP_SA$SUBJID <- gsub("^.{0,4}", "", HCP_SA$SUBJID)
        HCP_SA$Network <- gsub("^.{0,8}", "", HCP_SA$NETWORK)
        
        #Drop Network 0 (medial wall)
        HCP_AI <- subset(HCP_AI, Network!=0)
        HCP_SA <- subset(HCP_SA, Network!=0)
        #Reorder CBIG Networks
        mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
        HCP_AI$NewNetwork <- recode(HCP_AI$Network, !!!mapping)
        HCP_SA$NewNetwork <- recode(HCP_SA$Network, !!!mapping)
        
        #Create SA LAT variable
        HCP_SA$SA_LAT <- (HCP_SA$RH_SA - HCP_SA$LH_SA) / (HCP_SA$LH_SA + HCP_SA$RH_SA)
        
        #Create dataset marker
        HCP_SA$dataset <- "HCP"
        
        #Merge datasets
        HCP_df <- merge(HCP_SA, HCP_AI, by =c("SUBJID", "NewNetwork"), all=FALSE)
        
        #Convert AI to %
        HCP_df$RH_AVG_AI <- HCP_df$RH_AVG_AI*100
        HCP_df$LH_AVG_AI <- HCP_df$LH_AVG_AI*100
        
        #Format wide
        HCP_wide <- reshape(HCP_df,            
                           idvar = "SUBJID",
                           timevar = "NewNetwork",
                           direction = "wide")
        
        
#Left-lateralized networks: Spearman Rank
        #Language
          LANG_COR = cor.test(HCP_wide$SA_LAT.5, HCP_wide$LH_AVG_AI.5, method = "spearman") 
          print(LANG_COR)
        #Dorsal Attention-A
          DANA_COR = cor.test(HCP_wide$SA_LAT.6, HCP_wide$LH_AVG_AI.6, method = "spearman") 
          print(DANA_COR)
        #Control-B
          CTRLB_COR = cor.test(HCP_wide$SA_LAT.11, HCP_wide$LH_AVG_AI.11, method = "spearman") 
          print(CTRLB_COR)
          
#Right-lateralized networks: Spearman Rank
        #Limbic-B
          LIMBB_COR = cor.test(HCP_wide$SA_LAT.17, HCP_wide$RH_AVG_AI.17, method = "spearman") 
          print(LIMBB_COR)
        #Visual-B
          VISB_COR = cor.test(HCP_wide$SA_LAT.2, HCP_wide$RH_AVG_AI.2, method = "spearman") 
          print(VISB_COR)
        #Ventral Attention A
          VENA_COR = cor.test(HCP_wide$SA_LAT.8, HCP_wide$RH_AVG_AI.8, method = "spearman") 
          print(VENA_COR)  
          
          
#FIGURES: Scatterplots comparing NSAR with AI for 6 networks          
    #Just LANGUAGE
          left_networks=c("5")
          r_ordered <- c(LANG_COR[["estimate"]], DANA_COR[["estimate"]], CTRLB_COR[["estimate"]])
          network_names=c("LANG", "DAN-A", "CTRL-B")
          CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
          HCP_wide$SUBJID <- as.factor(HCP_wide$SUBJID)
          count=0
          for (n in left_networks) {
            count=count+1
            #convert AI to %
            filename <- paste("Study1_HCP_ECO_AI_SpearCorrPlots_", "Network", n, "_230527.png", sep = "")
            y_title <- paste("Autonomy Index (%)")
            x_title <- paste("Language NSAR")
            y_column <- paste("LH_AVG_AI.", n, sep = "")
            x_var <- paste("SA_LAT.", n, sep = "")
            #plot_title <- paste(network_names[count], " rho: ", format(round(r_ordered[count], 2), nsmall = 2), sep="")
            
            ggplot(HCP_wide, aes(x = get(x_var), y = get(y_column), color = dataset.1)) +
              labs(x = x_title, y = y_title) +
              #ggtitle(plot_title)+
              labs(fill = " ", color = " ") +
              geom_point(aes(fill = dataset.1), pch = 21) +
              #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
              geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
              #scale_y_continuous(limits = c(-0.05, .1)) +
              #scale_x_continuous(limits = c(-1, 1)) +
              #stat_cor(method="spearman", color="black")+
              scale_color_manual(values = "black") +
              scale_fill_manual(values = CBIG_Palette[as.numeric(n)]) +
              theme_bw() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    axis.title = element_text(colour = "black", size = 11),
                    axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=10),
                    axis.text.x = element_text(colour = "black", size=10),
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5, vjust = -0.1, size=11),
                    legend.title = element_text(colour = "black", size = 12),
                    legend.text = element_text(colour = "black", size = 12),
                    legend.background = element_rect(fill = "white", size = 0.5),
                    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                    axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                    panel.border = element_blank(),
                    panel.background = element_blank())
            
            ggsave(filename = filename, width = 3.35, height = 3.35,
                   path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
          }
          
          
          
          
    #Left-lateralized: Networks 5, 6, 11
          
          left_networks=c("5", "6", "11")
          r_ordered <- c(LANG_COR[["estimate"]], DANA_COR[["estimate"]], CTRLB_COR[["estimate"]])
          network_names=c("LANG", "DAN-A", "CTRL-B")
          CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
          HCP_wide$SUBJID <- as.factor(HCP_wide$SUBJID)
          count=0
          for (n in left_networks) {
              count=count+1
              #convert AI to %
              filename <- paste("Study1_HCP_ECO_AI_SpearCorrPlots_", "Network", n, "_230527.png", sep = "")
              y_title <- paste("Autonomy Index (%)")
              x_title <- paste("NSAR")
              y_column <- paste("LH_AVG_AI.", n, sep = "")
              x_var <- paste("SA_LAT.", n, sep = "")
              plot_title <- paste(network_names[count], " rho: ", format(round(r_ordered[count], 2), nsmall = 2), sep="")

              ggplot(HCP_wide, aes(x = get(x_var), y = get(y_column), color = dataset.1)) +
                labs(x = x_title, y = y_title) +
                ggtitle(plot_title)+
                labs(fill = " ", color = " ") +
                geom_point(aes(fill = dataset.1), pch = 21) +
                #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
                geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
                #scale_y_continuous(limits = c(-0.05, .1)) +
                #scale_x_continuous(limits = c(-1, 1)) +
                #stat_cor(method="spearman", color="black")+
                scale_color_manual(values = "black") +
                scale_fill_manual(values = CBIG_Palette[as.numeric(n)]) +
                theme_bw() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.title = element_text(colour = "black", size = 11),
                      axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=10),
                      axis.text.x = element_text(colour = "black", size=10),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5, vjust = -0.1, size=11),
                      legend.title = element_text(colour = "black", size = 12),
                      legend.text = element_text(colour = "black", size = 12),
                      legend.background = element_rect(fill = "white", size = 0.5),
                      axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                      axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                      panel.border = element_blank(),
                      panel.background = element_blank())
              
              ggsave(filename = filename, width = 2.21, height = 2.21,
                     path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
          }
          
          
      #Right-lateralized: Networks 2, 8 17
          right_networks=c("2", "8", "17")
          r_ordered <- c(VISB_COR[["estimate"]], VENA_COR[["estimate"]], LIMBB_COR[["estimate"]])
          network_names=c("VIS-B", "SAL-A", "LIM-B")
          CBIG_Palette <- c("#602368", "#DC1F26", "#4582A7", "#21B88B", "#32489E", "#4A9644", "#106A36", "#833C98", "#E383B1", "#CC8229", "#7B2C3F", "#6F809A", "#E3E218", "#A9313D", "#1C1B55", "#40471D", "#BCCF7E")
          HCP_wide$SUBJID <- as.factor(HCP_wide$SUBJID)
          count=0
          for (n in right_networks) {
            count=count+1
            filename <- paste("Study1_HCP_ECO_AI_SpearCorrPlots_", "Network", n, "_230527.png", sep = "")
            y_title <- paste("Autonomy Index (%)")
            x_title <- paste("NSAR")
            y_column <- paste("RH_AVG_AI.", n, sep = "")
            x_var <- paste("SA_LAT.", n, sep = "")
            plot_title <- paste(network_names[count], " rho: ", format(round(r_ordered[count], 2), nsmall = 2), sep="")
            
            ggplot(HCP_wide, aes(x = get(x_var), y = get(y_column), color = dataset.1)) +
              labs(x = x_title, y = y_title) +
              ggtitle(plot_title)+
              labs(fill = " ", color = " ") +
              geom_point(aes(fill = dataset.1), pch = 21) +
              #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
              geom_smooth(color = "black", method = "lm", size = 0.75, se = FALSE) +
              #scale_y_continuous(limits = c(-0.05, .1)) +
              #scale_x_continuous(limits = c(-1, 1)) +
              #stat_cor(method="spearman", color="black")+
              scale_color_manual(values = "black") +
              scale_fill_manual(values = CBIG_Palette[as.numeric(n)]) +
              theme_bw() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    axis.title = element_text(colour = "black", size = 11),
                    axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=10),
                    axis.text.x = element_text(colour = "black", size=10),
                    plot.title = element_text(hjust = 0.5, vjust = -0.1, size=11),
                    legend.position = "none",
                    legend.title = element_text(colour = "black", size = 11),
                    legend.text = element_text(colour = "black", size = 12),
                    legend.background = element_rect(fill = "white", size = 0.5),
                    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                    axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
                    panel.border = element_blank(),
                    panel.background = element_blank())
            
            ggsave(filename = filename, width = 2.21, height = 2.21,
                   path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/", dpi = 300)
          }
          
       
#---------------------------NETWORK RELATIONSHIPS: EFA-------------------------    
#SETUP
  #Load ALL .csv
          study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
          study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
          
  #Model-adjusted NSAR for visual inspection (HCP, HCPD)
          study1$SA_LAT_ADJ <- NA
          ci_df <- data.frame(dataset=factor(),
                              NewNetwork=factor(),
                              CI_MIN=integer(),
                              CI_MAX=integer(),
                              MEAN=integer())
          
          dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
          for (d in dataset_list){
            subset1 <- subset(study1, dataset==d)
            
            for (i in 1:17) {
              # Subset the data based on NewNetwork value
              subset_data <- subset(subset1, NewNetwork == i)
              
              # Fit the linear regression model
              model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
              
              #Grab lm coefficients
              BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
              BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
              BETA_FD <- model[["coefficients"]][["FD_Centered"]]
              BETA_HAND <- model[["coefficients"]][["Handedness"]]
              
              #Grab means
              MEAN_AGE <- mean(subset_data$Age_Centered)
              MEAN_FD <- mean(subset_data$FD_Centered)
              MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
              MEAN_HAND <- mean(subset_data$Handedness)
              
              #IDs of the subset
              subsetted_ids <- subset_data$SUBJID
              
              #Find matching rows
              matching_rows <- study1$SUBJID %in% subsetted_ids &
                study1$NewNetwork %in% i
              
              #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
              study1$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
              
              #find mean
              MEAN <- mean(subset_data$SA_LAT_ADJ)
              
              n <- length(subset_data$SA_LAT_ADJ)
              std_dev <- sd(subset_data$SA_LAT_ADJ)
              std_err <- std_dev / sqrt(n)
              z_score=1.96
              margin_error <- z_score * std_err
              
              #lower bound
              CI_MIN <- MEAN - margin_error
              #upper bound
              CI_MAX <- MEAN + margin_error
              #Append CI data to dataframe
              row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
              names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
              ci_df <- rbind(ci_df, row_df)
            }
          }
          
          #Reshape dataframe to WIDE
          study1_wide  <- reshape(study1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
          
            
#HCP-DISC Assumptions testing
            #Subset to HCP dataset
            HCPDISC_wide <- subset(study1_wide, dataset.1=="HCP-DISC")
            
            #1. Pairwise plots
                HCPDISC_SA_wide_pair <- HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                                 "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                                 "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                                 "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                # Create pairwise scatterplots
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPDISC_PairsPlot_230529.png", height = 1000, width = 1000)
                pairs(HCPDISC_SA_wide_pair)            # Create scatterplots
                dev.off()              # Close device and save as PNG
            
            #2. Multivariate tests for normality: Doornik-Hansen      
                library(mvnTest)
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPDISC_DHTestQQPlot_230529.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                DH.test(HCPDISC_SA_wide_pair, qqplot = TRUE)
                dev.off()              # Close device and save as PNG
              
                
            #3. Multicollinearity tests (VIF from package 'car')
                library(car)
                vif(lm(SA_LAT_ADJ.2 ~ SA_LAT_ADJ.5 + SA_LAT_ADJ.6 + SA_LAT_ADJ.8 + SA_LAT_ADJ.11 + SA_LAT_ADJ.12 + SA_LAT_ADJ.15 + SA_LAT_ADJ.17, data = HCPDISC_SA_wide_pair))
                
                
            #4. Bartlett's test of sphericity (from package 'psych')    
                #create corr matrix
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                                 "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                                 "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                                 "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #perform Bartlett's Test of Sphericity ('psych' package)
                cortest.bartlett(cor_matrix, n = nrow(HCPDISC_wide))
                
  
           #5. KMO test of sampling adequacy (package 'psych')     
                KMO(HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.8", "SA_LAT_ADJ.11", "SA_LAT_ADJ.12", "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")])
                
                
     #HCP Iterated principal factor analysis ('psych' package)
                #create corr matrix
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                              "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                              "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                              "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)     
                
                
                #Initial PFA model hypothesizing three factors. nfactors=max number of factors you would consider
                result_pca <- fa(cor_matrix, nfactors = 4, rotate = "none")
                print(result_pca, cutoff=0) 

                  #Eigenvalues of extracted factors
                  result_pca[["values"]]
                 
                  #R-squared scores of extracted factors
                  result_pca[["R2.scores"]]
                  
                  #Factor loadings
                  result_pca[["loadings"]]
                  fa.diagram(result_rotated$loadings, main = "Factor Loadings") #visualization
                  
                  
                #Parallel analysis  
                parallel_model <- fa.parallel(cor_matrix, n.obs=276, fa="fa", fm="ml", nfactors=4, n.iter=100)
                print(parallel_model)
                
                #Scree plot
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPDISC_EFAScreePlot_230529.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                plot(parallel_model,show.legend=FALSE)
                dev.off()              # Close device and save as PNG
                
          #Final HCP EFA model with 2 factors
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPDISC_wide[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                                  "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                                  "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                                  "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)     
                
                
                result_pca <- fa(cor_matrix, nfactors = 2, rotate = "none")
                print(result_pca, cutoff=0) 
                
                #Eigenvalues of extracted factors
                result_pca[["values"]]
                
                #Multiple R-squared scores of extracted factors
                result_pca[["R2.scores"]]
                
                # %variance explained
                result_pca[["Vaccounted"]]
                
                #Factor loadings
                result_pca[["loadings"]]
                fa.diagram(result_pca$loadings, main = "Factor Loadings") #visualization
 
                #Parallel analysis with scree
                parallel_model <- fa.parallel(cor_matrix, n.obs=276, fa="fa", fm="ml", nfactors=2, n.iter=100)
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPDISC_EFAScreePlot_230605.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                plot(parallel_model,show.legend=FALSE)
                dev.off()              # Close device and save as PNG
                
                
                

#----------------------------------NETWORK RELATIONSHIPS: CFA------------------------------------
#SETUP
          #Load ALL .csv
          study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
          study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
                
          #Model-adjusted NSAR for visual inspection (HCP, HCPD)
          study1$SA_LAT_ADJ <- NA
          ci_df <- data.frame(dataset=factor(),
                                    NewNetwork=factor(),
                                    CI_MIN=integer(),
                                    CI_MAX=integer(),
                                    MEAN=integer())
                
                dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
                for (d in dataset_list){
                  subset1 <- subset(study1, dataset==d)
                  
                  for (i in 1:17) {
                    # Subset the data based on NewNetwork value
                    subset_data <- subset(subset1, NewNetwork == i)
                    
                    # Fit the linear regression model
                    model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
                    
                    #Grab lm coefficients
                    BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
                    BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
                    BETA_FD <- model[["coefficients"]][["FD_Centered"]]
                    BETA_HAND <- model[["coefficients"]][["Handedness"]]
                    
                    #Grab means
                    MEAN_AGE <- mean(subset_data$Age_Centered)
                    MEAN_FD <- mean(subset_data$FD_Centered)
                    MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
                    MEAN_HAND <- mean(subset_data$Handedness)
                    
                    #IDs of the subset
                    subsetted_ids <- subset_data$SUBJID
                    
                    #Find matching rows
                    matching_rows <- study1$SUBJID %in% subsetted_ids &
                      study1$NewNetwork %in% i
                    
                    #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
                    study1$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                    subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
                    
                    #find mean
                    MEAN <- mean(subset_data$SA_LAT_ADJ)
                    
                    n <- length(subset_data$SA_LAT_ADJ)
                    std_dev <- sd(subset_data$SA_LAT_ADJ)
                    std_err <- std_dev / sqrt(n)
                    z_score=1.96
                    margin_error <- z_score * std_err
                    
                    #lower bound
                    CI_MIN <- MEAN - margin_error
                    #upper bound
                    CI_MAX <- MEAN + margin_error
                    #Append CI data to dataframe
                    row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
                    names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
                    ci_df <- rbind(ci_df, row_df)
                  }
                }
                
        #Reshape dataframe to WIDE
        study1_wide  <- reshape(study1, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
                
              
#HCP-REP Assumptions testing
                #Subset to HCP-REP dataset
                HCPREP_wide <- subset(study1_wide, dataset.1=="HCP-REP")
                
                #1. Pairwise plots
                HCPREP_SA_wide_pair <- HCPREP_wide[, c("SA_LAT_ADJ.5", 
                                                       "SA_LAT_ADJ.6", 
                                                       "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                # Create pairwise scatterplots
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPREP_PairsPlot_230529.png", height = 1000, width = 1000)
                pairs(HCPREP_SA_wide_pair)            # Create scatterplots
                dev.off()              # Close device and save as PNG
                
                #2. Multivariate tests for normality: Doornik-Hansen      
                library(mvnTest)
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPREP_DHTestQQPlot_230529.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                DH.test(HCPREP_SA_wide_pair, qqplot = TRUE)
                dev.off()              # Close device and save as PNG
                
                
                #3. Multicollinearity tests (VIF from package 'car')
                library(car)
                vif(lm(SA_LAT_ADJ.5 ~ SA_LAT_ADJ.6 + SA_LAT_ADJ.15 + SA_LAT_ADJ.17, data = HCPREP_SA_wide_pair))
                
                
                #4. Bartlett's test of sphericity (from package 'psych')    
                #create corr matrix
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPREP_wide[, c("SA_LAT_ADJ.5", 
                                                 "SA_LAT_ADJ.6", 
                                                 "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #perform Bartlett's Test of Sphericity ('psych' package)
                cortest.bartlett(cor_matrix, n = nrow(HCPREP_wide))
                
                
                #5. KMO test of sampling adequacy (package 'psych')     
                KMO(HCPREP_wide[, c("SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")])
                

      #HCP-REP CFA                          
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPREP_wide[, c("SA_LAT_ADJ.5", 
                                                 "SA_LAT_ADJ.6", 
                                                 "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)     
                
                
                #Set up model
                cfa_model <- "
                F1 =~ SA_LAT_ADJ.15 + SA_LAT_ADJ.17
                F2 =~ SA_LAT_ADJ.5 + SA_LAT_ADJ.6
                F1 ~~ 0*F2"
                
                #Fit model
                library(lavaan)
                fit_cfa <- cfa(cfa_model, data=selected_vars, estimator = "MLM", se="robust", std.lv=TRUE)
                summary(fit_cfa, fit.measures =TRUE, standardized=TRUE)
                
                #Grab standardized loadings
                inspect(fit_cfa,what="std")
                
                #visualize model
                library(semPlot)
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPREP_CFATree_231206.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                semPaths(fit_cfa, "std", layout = "tree")
                dev.off()              # Close device and save as PNG
                
                
                #How to write up CFA results: 
                #i.	This initial structural model provided fair fit to the data χ2(6) = 71.62, p < .001; confirmatory fit index (CFI) = 0.969; root-mean-square error of approximation (RMSEA) = 0.108; standardized root mean square residual (SRMR) = 0.021. However, the modification indices indicated that one change, that of covarying the anomia at 1967 and anomia at 1971 measures, would be beneficial. Since this change would significantly improve the model fit and made theoretical sense, it was implemented. The final structural model (see Figure 1) was of excellent fit to the data χ2(5) = 6.39, p = .27; CFI = 0.999; RMSEA = 0.017; SRMR = 0.011. The results support the idea that SES is impacting alienation in 1967 (β = -0.55, p < 0.001) and in 1971 (β = -0.19, p < 0.001). Also, alienation in 1967 positively predicts alienation in 1971 (β = 0.58, p < 0.001). Standardized estimates for all parameters are shown in Table 3.
                #ii.	Chi-square:
                #  iii.	The null hypothesis of the Chi-square test of model fit is that the predicted covariance matrix is equivalent to the actual covariance matrix. Using the goodness-of-fit command in Stata 16.1, we rejected the null hypothesis that the predicted covariance matrix is equivalent to the actual covariance matrix (χ2(2) = 1.499, p = 0.473). Thus, this model does not appear to fit the data well. For this model, the degrees of freedom came from the difference between the number of items in the covariance matrix (10) and the number of parameters estimated by the model (8), resulting in 2 degrees of freedom.
               
 
#HCPD Assumptions testing
                #Subset to HCPD dataset
                HCPD_wide <- subset(study1_wide, dataset.1=="HCPD")
                
                #1. Pairwise plots
                HCPD_SA_wide_pair <- HCPD_wide[, c("SA_LAT_ADJ.5", 
                                                   "SA_LAT_ADJ.6", 
                                                   "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                # Create pairwise scatterplots
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPD_PairsPlot_230529.png", height = 1000, width = 1000)
                pairs(HCPD_SA_wide_pair)            # Create scatterplots
                dev.off()              # Close device and save as PNG
                
                #2. Multivariate tests for normality: Doornik-Hansen      
                library(mvnTest)
                png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPD_DHTestQQPlot_230529.png", height = 3.25, width = 3.25, units="in", res= 1200,)
                DH.test(HCPD_SA_wide_pair, qqplot = TRUE)
                dev.off()              # Close device and save as PNG
                
                
                #3. Multicollinearity tests (VIF from package 'car')
                library(car)
                vif(lm(SA_LAT_ADJ.5 ~ SA_LAT_ADJ.6 + SA_LAT_ADJ.15 + SA_LAT_ADJ.17, data = HCPD_SA_wide_pair))
                
                
                #4. Bartlett's test of sphericity (from package 'psych')    
                #create corr matrix
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPD_wide[, c("SA_LAT_ADJ.5", 
                                               "SA_LAT_ADJ.6", 
                                               "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)
                
                #perform Bartlett's Test of Sphericity ('psych' package)
                cortest.bartlett(cor_matrix, n = nrow(HCPD_wide))
                
                
                #5. KMO test of sampling adequacy (package 'psych')     
                KMO(HCPD_wide[, c("SA_LAT_ADJ.5", "SA_LAT_ADJ.6", "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")])
                
                # Select the variables you want to include in the correlation matrix
                selected_vars <- HCPD_wide[, c("SA_LAT_ADJ.5", 
                                               "SA_LAT_ADJ.6", 
                                               "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
                
                # Compute the correlation matrix
                cor_matrix <- cor(selected_vars)     
            
         #HCPD CFA
                #Set up model
                cfa_model <- "
                F1 =~ SA_LAT_ADJ.15 + SA_LAT_ADJ.17
                F2 =~ SA_LAT_ADJ.5 + SA_LAT_ADJ.6
                F1 ~~ 0*F2"
                
                #Fit model
                library(lavaan)
                fit_cfa <- cfa(cfa_model, data=selected_vars, estimator = "MLM", se="robust", std.lv=TRUE)
                summary(fit_cfa, fit.measures =TRUE, standardized=TRUE)
                
                #Grab standardized loadings
                inspect(fit_cfa,what="std")
                
                                
#-----------------------------------------COG-LAT CORRESPONDENCE: CCA-------------------------------------------
#Purpose: Examine a potential relationship between cognitive ability and brain network lateralization for eight specialized networks. 
#Requires package: library(CCA)

#SETUP
      #Load study1
      study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
      study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
      
      #Drop NSD dataset
      study1 <- subset(study1, dataset!="REST")
      study1 <- subset(study1, dataset!="TASK")
      
      #Model-adjusted NSAR for 17 networks
      study1$SA_LAT_ADJ <- NA
      ci_df <- data.frame(dataset=factor(),
                          NewNetwork=factor(),
                          CI_MIN=integer(),
                          CI_MAX=integer(),
                          MEAN=integer())
      
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      for (d in dataset_list){
        subset1 <- subset(study1, dataset==d)
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          # Fit the linear regression model
          model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- study1$SUBJID %in% subsetted_ids &
            study1$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          study1$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$SA_LAT_ADJ)
          
          n <- length(subset_data$SA_LAT_ADJ)
          std_dev <- sd(subset_data$SA_LAT_ADJ)
          std_err <- std_dev / sqrt(n)
          z_score=1.96
          margin_error <- z_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          ci_df <- rbind(ci_df, row_df)
        
      }
      }
      
      #Reshape dataframe LONG to WIDE
      study1_wide <- study1 %>%
        pivot_wider(
          names_from = c("NewNetwork"),
          names_prefix = "SA_LAT_ADJ.",
          values_from = c("SA_LAT_ADJ"),
          id_cols = c("SUBJID","sessions", "dataset","Age_Centered","FD_Centered", "Age_in_Yrs","Handedness", "FD_avg", "Sum_Volumes", "DVARS_avg", "Age_Centered", "FD_Centered", "Sex_Bin", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj")
        )
      
      
  # Model-adjusted COG measures
      study1_wide$ReadEng_Unadj_ADJ <- NA
      study1_wide$PicVocab_Unadj_ADJ <- NA
      study1_wide$Flanker_Unadj_ADJ <- NA
      study1_wide$CardSort_Unadj_ADJ <- NA
      study1_wide$ListSort_Unadj_ADJ <- NA
      
      ci_df <- data.frame(dataset = factor(),
                          Variable = factor(),
                          CI_MIN = integer(),
                          CI_MAX = integer(),
                          MEAN = integer())
      
      dataset_list <- c("HCP-DISC", "HCP-REP", "HCPD")
      variables_to_loop <- c("ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj")
      
      for (d in dataset_list) {
        subset1 <- subset(study1_wide, dataset == d)
        
        for (variable in variables_to_loop) {
          subset1 <- subset(subset1, paste0(variable)!="NA")
          # Fit the linear regression model
          model <- lm(paste0(variable, " ~ Age_Centered + Sex_Bin"), data = subset1)
          
          # Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
         
          # Grab means
          MEAN_AGE <- mean(subset1$Age_Centered)
          MEAN_SEX <- nrow(subset1[subset1$Sex_Bin == "0", ]) / nrow(subset1)

          # IDs of the subset
          subsetted_ids <- subset1$SUBJID

          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          adjusted_variable <- paste0(variable, "_ADJ")
          
          # Example formula for adjustment
          subset1[[adjusted_variable]] <- subset1[[variable]] - (
            (BETA_AGE * (subset1$Age_Centered - MEAN_AGE)) +
              (BETA_SEX * (as.numeric(subset1$Sex_Bin) - MEAN_SEX))
          )
          
          study1_wide[study1_wide$SUBJID %in% subsetted_ids, adjusted_variable] <- subset1[[variable]] - (
            (BETA_AGE * (subset1$Age_Centered - MEAN_AGE)) +
              (BETA_SEX * (as.numeric(subset1$Sex_Bin) - MEAN_SEX))
          )
          
          # Find mean
          MEAN <- mean(subset1[[adjusted_variable]])
          
          n <- length(subset1[[adjusted_variable]])
          std_dev <- sd(subset1[[adjusted_variable]])
          std_err <- std_dev / sqrt(n)
          z_score <- 1.96
          margin_error <- z_score * std_err
          
          # Lower bound
          CI_MIN <- MEAN - margin_error
          # Upper bound
          CI_MAX <- MEAN + margin_error
          
          # Append CI data to dataframe
          row_df <- data.frame(d, variable, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "Variable", "CI_MIN", "CI_MAX", "MEAN")
          ci_df <- rbind(ci_df, row_df)
        }
      }
      
      
 
      
      
#HCP 232 Assumptions testing
      #Subset to HCP232 dataset
      HCP232 <- subset(study1_wide, sessions=="4")

      #Drop all subjects missing COG measures: ORRT (ReadEng_Unadj), Flanker_Unadj, CardSort_Unadj, ListSort_Unadj
      HCP232 <- HCP232[complete.cases(HCP232$ReadEng_Unadj_ADJ, HCP232$Flanker_Unadj_ADJ), ]
      
      #LAT DATA
      HCP232_SA_wide_pair <- HCP232[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                               "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                               "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                               "SA_LAT_ADJ.15", "SA_LAT_ADJ.17")]
      #COG/BEH DATA
      HCP232_COG_wide_pair <- HCP232[, c("ReadEng_Unadj_ADJ", "Flanker_Unadj_ADJ")]
      
      #BOTH COG + LAT
      HCP232_ALL_wide_pair <- HCP232[, c("SA_LAT_ADJ.2", "SA_LAT_ADJ.5", 
                                                "SA_LAT_ADJ.6", "SA_LAT_ADJ.8",
                                                "SA_LAT_ADJ.11", "SA_LAT_ADJ.12",
                                                "SA_LAT_ADJ.15", "SA_LAT_ADJ.17",
                                                "ReadEng_Unadj_ADJ", "Flanker_Unadj_ADJ")]
      
      #1. Multivariate tests for normality: Doornik-Hansen      
      library(mvnTest)
      png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCP232_DHTestQQPlot_231027.png", height = 3.25, width = 3.25, units="in", res= 1200,)
      DH.test(HCP232_ALL_wide_pair, qqplot = TRUE)
      dev.off()              # Close device and save as PNG
      
      #2. Pairwise plots
      png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCP232_PairsPlot_ALL_231027.png", height = 1000, width = 1000)
      pairs(HCP232_ALL_wide_pair)            # Create scatterplots
      dev.off()              # Close device and save as PNG
      
      #3. Use 'matcor' from CCA package: correlations between and within matrices
      matcor(HCP232_SA_wide_pair, HCP232_COG_wide_pair)
      

#HCP232 CCA
      #Implement CCA using "cc" function from CCA package (allows computations of loadings+visualizations)
      HCP232_CCA <- cc(HCP232_SA_wide_pair,HCP232_COG_wide_pair)
      HCP232_CCA$cor #correlations between canonical variates       
      
      #Compute canonical loadings
      HCP232_CCA2 <- comput(HCP232_SA_wide_pair, HCP232_COG_wide_pair, HCP232_CCA)  
      
      # display canonical loadings -> tells you which variables load strongest onto the canonical variates
      HCP232_CCA2[3:6]  
      
      #Test number of canonical dimensions (package: CCP)
      #Setup variables
      rho <- HCP232_CCA$cor
      n <- dim(HCP232_SA_wide_pair)[1]
      p <- length(HCP232_SA_wide_pair)
      q <- length(HCP232_COG_wide_pair)
      
      #Calculate p-values using the F-approximations of different test statistics: (package: CCP)
      #Answers question: Which canonical variates are statistically sig? (more than one? just one? etc.)
      p.asym(rho, n, p, q, tstat = "Wilks")
      
      p.asym(rho, n, p, q, tstat = "Hotelling")
      
      p.asym(rho, n, p, q, tstat = "Pillai")
      
      p.asym(rho, n, p, q, tstat = "Roy")
      
      #Standardized NSAR canonical coefficients diagonal matrix of NSAR sd's
      s1 <- diag(sqrt(diag(cov(HCP232_SA_wide_pair))))
      s1 %*% HCP232_CCA$xcoef
      
      #Standardized COG canonical coefficients diagonal matrix of COG sd's
      s2 <- diag(sqrt(diag(cov(HCP232_COG_wide_pair))))
      s2 %*% HCP232_CCA$ycoef   
      
      #Correlate raw scores with canonical variates (purpose: to check which network and which cognitive variable are loading most significantly)
       #Apply Haufe transformation first: https://www.sciencedirect.com/science/article/pii/S1053811913010914
          #LAT
          Cov_A <- cov(HCP232_SA_wide_pair) #grab covariance matrix
          W_transformed_A <- Cov_A %*% HCP232_CCA$xcoef
          #COG 
          Cov_B <- cov(HCP232_COG_wide_pair) #grab covariance matrix
          W_transformed_B <- Cov_B %*% HCP232_CCA$ycoef
        
       #LAT and COG variates
          cor_COGvariates <- cor(HCP232_SA_wide_pair, HCP232_CCA[["scores"]][["yscores"]])
          print(cor_COGvariates)
          
        #COG and LAT variates
          cor_LATvariates <- cor(HCP232_COG_wide_pair, HCP232_CCA[["scores"]][["xscores"]])
          print(cor_LATvariates)

        #Threshold for significance
          r_value <- 1.96/(sqrt(dim(HCP232_SA_wide_pair)[1] -3))
          print(r_value)
      
      
#------------------------------------------HCPD DEV CHANGES-------------------------------------------
#Purpose: Demonstrate lack of developmental changes in specialization for the HCPD dataset, with accompanying supplementary figure
                
#SETUP
      #Load ALL .csv
      study1 <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv")
      study1$Sex_Bin <- as.factor(study1$Sex_Bin)  
                
      #Subset to HCPD
      HCPD_NSAR <- subset(study1, dataset=="HCPD")
         
#MODEL-ADJUSTED VALUES
      HCPD_NSAR$SA_LAT_ADJ <- NA
      ci_df <- data.frame(dataset=factor(),
                          NewNetwork=factor(),
                          CI_MIN=integer(),
                          CI_MAX=integer(),
                          MEAN=integer())
      
      dataset_list <- c("HCPD")
      for (d in dataset_list){
        subset1 <- subset(HCPD_NSAR, dataset==d)
        
        for (i in 1:17) {
          # Subset the data based on NewNetwork value
          subset_data <- subset(subset1, NewNetwork == i)
          
          # Fit the linear regression model
          model <- lm(SA_LAT ~ Age_Centered + Sex_Bin + FD_Centered +Handedness, data = subset_data)
          
          #Grab lm coefficients
          BETA_AGE <- model[["coefficients"]][["Age_Centered"]]
          BETA_SEX <- model[["coefficients"]][["Sex_Bin1"]]
          BETA_FD <- model[["coefficients"]][["FD_Centered"]]
          BETA_HAND <- model[["coefficients"]][["Handedness"]]
          
          #Grab means
          MEAN_AGE <- mean(subset_data$Age_Centered)
          MEAN_FD <- mean(subset_data$FD_Centered)
          MEAN_SEX <- nrow(subset_data[subset_data$Sex_Bin == "0",])/nrow(subset_data) #The mean of a dichotomous variable is just the proportion which has been coded as 0 (or the reference category, which is 0 or Male). Determine ref category with: levels(subset_data$Sex_Bin)[1]
          MEAN_HAND <- mean(subset_data$Handedness)
          
          #IDs of the subset
          subsetted_ids <- subset_data$SUBJID
          
          #Find matching rows
          matching_rows <- HCPD_NSAR$SUBJID %in% subsetted_ids &
            HCPD_NSAR$NewNetwork %in% i
          
          #Example formula (Qurechi 2014): VOLadj = VOLnat - [Beta1(Agenat - MeanAge) + Beta2(Sexnat - MeanSex) + Beta3(Sitenat - MeanSitenat) + Beta4()]
          HCPD_NSAR$SA_LAT_ADJ[matching_rows] <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          subset_data$SA_LAT_ADJ <- subset_data$SA_LAT - ( (BETA_AGE*(subset_data$Age_Centered - MEAN_AGE)) + (BETA_FD*(subset_data$FD_Centered - MEAN_FD)) + (BETA_SEX*(as.numeric(subset_data$Sex_Bin) - MEAN_SEX)) + (BETA_HAND*(subset_data$Handedness - MEAN_HAND)) )
          
          #find mean
          MEAN <- mean(subset_data$SA_LAT_ADJ)
          
          n <- length(subset_data$SA_LAT_ADJ)
          std_dev <- sd(subset_data$SA_LAT_ADJ)
          std_err <- std_dev / sqrt(n)
          z_score=1.96
          margin_error <- z_score * std_err
          
          #lower bound
          CI_MIN <- MEAN - margin_error
          #upper bound
          CI_MAX <- MEAN + margin_error
          #Append CI data to dataframe
          row_df <- data.frame(d, i, CI_MIN, CI_MAX, MEAN)
          names(row_df) <- c("dataset", "NewNetwork", "CI_MIN", "CI_MAX", "MEAN")
          ci_df <- rbind(ci_df, row_df)
        }
      }
      
      
             
#SUPPLEMENTARY FIGURE      
      #Fig.3: 17 scatterplots showing trajectories of NSAR over age
      Palette <- c("#0072B2")

      # Convert necessary columns to appropriate data types
      HCPD_NSAR$SA_LAT_ADJ <- as.numeric(as.character(HCPD_NSAR$SA_LAT_ADJ))
      HCPD_NSAR$Age_in_Yrs <- as.numeric(HCPD_NSAR$Age_in_Yrs)
      HCPD_NSAR$NewNetwork <- as.factor(HCPD_NSAR$NewNetwork)
      
      #Network labels
      labels <- c("VIS-A", "VIS-B", "SOM-A", "SOM-B", "LANG", "DAN-A", "DAN-B", "SAL-A", "SAL-B", "CTRL-A", "CTRL-B", "CTRL-C", "DEF-A", "DEF-B", "DEF-C", "LIM-A", "LIM-B")
      
      # Create a for loop to generate ggplots
      for (loop in 1:17) {
        # Subset the dataset based on NewNetwork == loop
        subset_data <- subset(HCPD_NSAR, NewNetwork == loop)
        
        # Create ggplot
        p <- ggplot(subset_data, aes(x = Age_in_Yrs, y = SA_LAT_ADJ, color = dataset)) +
          labs(x = 'Age (Years)', y = "NSAR") +
          ggtitle(labels[loop])+
          labs(fill = " ") +
          labs(color = " ") +
          scale_colour_manual(values = Palette) +
          scale_fill_manual(values = Palette, labels = c("")) +
          geom_point(aes(fill = dataset), colour = "black", pch = 21) +
          #scale_y_continuous(limits = c(-1, 1)) +
          geom_smooth(method = lm, aes(fill = dataset), se = TRUE) +
          guides(color = guide_legend(override.aes = list(fill = NA))) +
          theme_bw() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = -0.1),
            axis.title = element_text(colour = "black", size = 12),
            axis.text.y = element_text(colour = "black", angle = 90, hjust = 0.6, size=10),
            axis.text.x = element_text(colour = "black", size=10),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(colour = "black", size = 10),
            legend.background = element_rect(fill = "white", size = 0.5),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 1, linetype = "solid"),
            panel.border = element_blank(),
            panel.background = element_blank()
          )
        
        # Save the ggplot as an PNG file
        filename <- paste("Study1_HCPD_NSARNetwork_Scatter_", loop, "_230829.png", sep = "")
        ggsave(
          filename = filename,
          plot = p,
          width = 2.25,
          height = 2.25,
          path = "C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/",
          dpi = 300
        )
      }         
                                               
#-------------------------------------------HCPD200 NETWORK RELATIONSHIPS: CFA-----------------------------
#SETUP            
                #Load HCPD dataset
                #Load data descriptor (Box)
                HCPD_data <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_LS_2.0_subject_completeness.xlsx")
                #Load IDs of 583 participants with REST fMRI data post-preproc
                PARC_IDS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/subjids/REST_ids.txt")
                DVARS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/DVARS_avg_HCPD_ALL_230517.csv")
                FD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/FD_avg_HCPD_ALL_230517.csv")
                VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/RemainingVols_HCPD_230517.csv")
                
                #Clean up motion_metrics
                names(DVARS)[1] <- "SUBJID"
                names(FD)[1] <- "SUBJID"
                names(VOLS)[1] <- "SUBJID"
                
                FD$SUBJID <- gsub("^.{0,4}", "", FD$SUBJID) #remove "sub-" string
                DVARS$SUBJID <- gsub("^.{0,4}", "", DVARS$SUBJID) #remove "sub-" string
                VOLS$SUBJID <- gsub("^.{0,4}", "", VOLS$SUBJID) #remove "sub-" string
                
                #Clean up demos dataset
                HCPD_data <- HCPD_data[,c("src_subject_id", "sex", "interview_age", "Full_MR_Compl")]
                HCPD_data <- subset(HCPD_data, src_subject_id!="HCA or HCD subject id")
                names(HCPD_data)[1] <- "SUBJID"
                
                #Age Years (convert from months to years)
                HCPD_data$interview_age <- as.numeric(as.character(HCPD_data$interview_age))
                HCPD_data$Age_in_Yrs <- HCPD_data$interview_age/12
                
                #Create dataset marker
                HCPD_data$dataset <- "HCPD"
                
                #Merge datasets
                HCPD_data <- merge(HCPD_data, PARC_IDS, by =c("SUBJID"), all=FALSE)
                HCPD_data <- merge(HCPD_data, DVARS, by =c("SUBJID"), all=FALSE)
                HCPD_data <- merge(HCPD_data, FD, by =c("SUBJID"), all=FALSE)
                HCPD_data <- merge(HCPD_data, VOLS, by =c("SUBJID"), all=FALSE)
                
                #Exclusions
                #1. Participants with FD_avg greater than 0.2
                HCPD_data <- subset(HCPD_data, FD_avg<.2)
                #2. Participants with avg DVARS greater than 50
                HCPD_data <- subset(HCPD_data, DVARS_avg<51)
                #3. % volumes remaining is less than 50% (474 * 4 = 1896) or 253*6=1518 for ages 5-7
                HCPD_data$Percent_Vols <- ifelse(HCPD_data$Age_in_Yrs < 7, (HCPD_data$Sum_Volumes / (253*6)), (HCPD_data$Sum_Volumes / (474*4)))
                HCPD_data <- subset(HCPD_data, Percent_Vols>.5)
                
                #Grab oldest 200
                HCPD200 <- head(HCPD_data[order(-HCPD_data$Age_in_Yrs), ], 200)
                
                #Load specialization data (SAI/NSAR)
                HCPD_SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/network_sa/REST/NETWORK_SA_SUB_NET_LH_RH_230214.csv")
                HCPD_SAI$SUBJID <- gsub("^.{0,4}", "", HCPD_SAI$SUBJID)
                HCPD_SAI$Network <- gsub("^.{0,8}", "", HCPD_SAI$NETWORK)
                
                #Drop Network 0 (medial wall)
                HCPD_SAI <- subset(HCPD_SAI, Network!=0)
                #Reorder CBIG Networks
                mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
                HCPD_SAI$NewNetwork <- recode(HCPD_SAI$Network, !!!mapping)
                
                #Create SA LAT variable
                HCPD_SAI$SA_LAT <- (HCPD_SAI$RH_SA - HCPD_SAI$LH_SA) / (HCPD_SAI$LH_SA + HCPD_SAI$RH_SA)
                
                #Merge in SAI with 200, demos
                HCPD_200SAI <- merge(HCPD200, HCPD_SAI, by =c("SUBJID"), all=FALSE)
                
                
            #Wide-format data
            HCPD_200_wide  <- reshape(HCPD_200SAI, idvar = "SUBJID", timevar = "NewNetwork", direction = "wide")
            
#Assumptions testing
            #1. Pairwise plots
            HCPD_200_wide_pair <- HCPD_200_wide[,c("SA_LAT.11", "SA_LAT.15", "SA_LAT.17")]
            
            # Create pairwise scatterplots
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPD_PairsPlot_230601.png", height = 1000, width = 1000)
            pairs(HCPD_200_wide_pair)            # Create scatterplots
            dev.off()              # Close device and save as PNG
            
            #2. Multivariate tests for normality: Doornik-Hansen      
            library(mvnTest)
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPD200_DHTestQQPlot_230601.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            DH.test(HCPD_200_wide_pair, qqplot = TRUE)
            dev.off()              # Close device and save as PNG
            
            
            #3. Multicollinearity tests (VIF from package 'car')
            vif(lm(SA_LAT.11 ~ SA_LAT.15 + SA_LAT.17, data = HCPD_200_wide_pair))
            
            
            #4. Bartlett's test of sphericity (from package 'psych')    
            #create corr matrix
            # Select the variables you want to include in the correlation matrix
            selected_vars <- HCPD_200_wide_pair[, c("SA_LAT.11", "SA_LAT.15", "SA_LAT.17")]                                         
            
            # Compute the correlation matrix
            cor_matrix <- cor(selected_vars)     
            
            #perform Bartlett's Test of Sphericity ('psych' package)
            cortest.bartlett(cor_matrix, n = nrow(HCPD_200_wide_pair))
            
            
            #5. KMO test of sampling adequacy (package 'psych')     
            KMO(HCPD_200_wide_pair[, c("SA_LAT.11", "SA_LAT.15", "SA_LAT.17")])
            
            
#CFA ('lavaan' package)
            #create corr matrix
            # Select the variables you want to include in the correlation matrix
            selected_vars <- HCPD_200_wide_pair[, c("SA_LAT.11", "SA_LAT.15", "SA_LAT.17")]                                         
            
            # Compute the correlation matrix
            cor_matrix <- cor(selected_vars)     

        #CFA with 3 factors:  
            #Set up model
            cfa_model <- "
            F1 =~ SA_LAT.15 + SA_LAT.17"
            
            #Fit model
            library(lavaan)
            fit_cfa <- cfa(cfa_model, data = HCPD_200_wide_pair)
            
            #visualize model
            library(semPlot)
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_UTTD_CFATree_230530.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            semPaths(fit_cfa2, "std", layout = "tree")
            dev.off()              # Close device and save as PNG
            
            #Check modification indices
            modindices(fit_cfa, sort = TRUE, maximum.number = 5)
            
          #Modify cfa-model using MI suggestions
            cfa_model2 <- "
            F1 =~ SA_LAT.3+ SA_LAT.10 
            F2 =~ SA_LAT.9 + SA_LAT.16 + SA_LAT.12
            F3 =~ SA_LAT.7 + SA_LAT.15
            SA_LAT.15 ~~  SA_LAT.9"
          
            fit_cfa2 <- cfa(cfa_model2, data = UT_TD_SAI_wide)
            semPaths(fit_cfa2, "std", layout = "tree")
            #model is still not positive definite
            
     #How to write up CFA results: 
            #i.	This initial structural model provided fair fit to the data χ2(6) = 71.62, p < .001; confirmatory fit index (CFI) = 0.969; root-mean-square error of approximation (RMSEA) = 0.108; standardized root mean square residual (SRMR) = 0.021. However, the modification indices indicated that one change, that of covarying the anomia at 1967 and anomia at 1971 measures, would be beneficial. Since this change would significantly improve the model fit and made theoretical sense, it was implemented. The final structural model (see Figure 1) was of excellent fit to the data χ2(5) = 6.39, p = .27; CFI = 0.999; RMSEA = 0.017; SRMR = 0.011. The results support the idea that SES is impacting alienation in 1967 (β = -0.55, p < 0.001) and in 1971 (β = -0.19, p < 0.001). Also, alienation in 1967 positively predicts alienation in 1971 (β = 0.58, p < 0.001). Standardized estimates for all parameters are shown in Table 3.
            #ii.	Chi-square:
            #  iii.	The null hypothesis of the Chi-square test of model fit is that the predicted covariance matrix is equivalent to the actual covariance matrix. Using the goodness-of-fit command in Stata 16.1, we rejected the null hypothesis that the predicted covariance matrix is equivalent to the actual covariance matrix (χ2(2) = 1.499, p = 0.473). Thus, this model does not appear to fit the data well. For this model, the degrees of freedom came from the difference between the number of items in the covariance matrix (10) and the number of parameters estimated by the model (8), resulting in 2 degrees of freedom.
            
#--------------------------------WRITE OUT DATASET---------------------------
#Load ALL HCP data (4 sessions)
            HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
            HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
            FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/FD_avg_HCP_ALL_230515.csv")
            DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/DVARS_avg_HCP_ALL_230515.csv")
            TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_ALL/motion_metrics/RemainingVols_HCP_230515.csv")
            
            #Format data
            names(HCP_DEMOS)[1] <- "SUBJID"
            names(HCP_R_DEMOS)[1] <- "SUBJID"
            names(FD_AVG)[1] <- "SUBJID"
            names(DVARS_AVG)[1] <- "SUBJID"
            names(TOT_VOLS)[1] <- "SUBJID"
            
            FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
            DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
            TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
            TOT_VOLS$Percent_Vols <- (TOT_VOLS$Sum_Volumes / (1196*4))*100 #Number of volumes after skip4 = 1196 x4 runs
            
            #Merge HCP datasets
            comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
            comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
            comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
            HCP_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
            
            #Variable for sex, Males=0
            HCP_df$Sex_Bin <- as.factor(ifelse(HCP_df$Gender == "F", 1, ifelse(HCP_df$Gender == "M", 0, NA)))
            
            #Just include relevant columns
            HCP_df <- HCP_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Sex_Bin", "Handedness", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj")]
              #ORRT: ReadEng_Unadj. Language
              #PVT: PicVocab_Unadj. Language.
              #FLANKER: Flanker_Unadj. Attention/Executive function.
              #CCST: CardSort_Unadj. Executive function.
              #LSWMT: ListSort_Unadj. Working memory.
            
            #Create sessions marker
            HCP_df$sessions <- "4"
            
            #Organize covariates
            HCP_df$Age_Centered <- as.numeric(HCP_df$Age_in_Yrs - mean(HCP_df$Age_in_Yrs))
            HCP_df$FD_Centered <- as.numeric(HCP_df$FD_avg - mean(HCP_df$FD_avg))
            HCP_df$Handedness <- as.numeric(HCP_df$Handedness)
            
            #Load HCP 232 NSAR data
            SA_ALL <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/ALL/HCP_ALL_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
            SA_ALL$SUBJID <- gsub("^.{0,4}", "", SA_ALL$SUBJID)
            SA_ALL$Network <- gsub("^.{0,8}", "", SA_ALL$NETWORK)
            
            #Drop Network 0 (medial wall)
            SA_ALL <- subset(SA_ALL, Network!=0)
            
            #Reorder CBIG Networks
            mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
            SA_ALL$NewNetwork <- recode(SA_ALL$Network, !!!mapping)
            
            #Merge HCP NSAR with demos
            HCP_SA_ALL <- merge(SA_ALL, HCP_df, by=c("SUBJID"), all=TRUE)
            
            
          #Load HCP-REP dataset: subjects with 1-3 runs of data matched to the HCP232 dataset
            #Load Demos
            HCP_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/HCP_Unrestricted_Behavioral_230201.csv")       
            HCP_R_DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/HCP_analysis/behavioral_data/RESTRICTED_mpeterson_5_15_2023_13_22_55.csv")
            FD_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/FD_avg_HCP_REP_230616.csv")
            DVARS_AVG <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/DVARS_avg_HCP_REP_230616.csv")
            TOT_VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/Kong2019_parc_fs6_REPLICATION/motion_metrics/RemainingVols_HCP_REP_230616.csv")
            
            #Format data
            names(HCP_DEMOS)[1] <- "SUBJID"
            names(HCP_R_DEMOS)[1] <- "SUBJID"
            names(FD_AVG)[1] <- "SUBJID"
            names(DVARS_AVG)[1] <- "SUBJID"
            names(TOT_VOLS)[1] <- "SUBJID"
            
            FD_AVG$SUBJID <- gsub("^.{0,4}", "", FD_AVG$SUBJID) #remove "sub-" string
            DVARS_AVG$SUBJID <- gsub("^.{0,4}", "", DVARS_AVG$SUBJID) #remove "sub-" string
            TOT_VOLS$SUBJID <- gsub("^.{0,4}", "", TOT_VOLS$SUBJID) #remove "sub-" string
            TOT_VOLS$Percent_Vols <- (TOT_VOLS$Sum_Volumes / (1196*4))*100 #Number of volumes after skip4 = 1196 x4 runs
            
            #Merge HCP datasets
            comb2 <- merge(HCP_DEMOS, FD_AVG, by =c("SUBJID"), all=FALSE)
            comb3 <- merge(comb2, DVARS_AVG, by =c("SUBJID"), all=FALSE)
            comb4 <- merge(comb3, TOT_VOLS, by =c("SUBJID"), all=FALSE)
            HCPR_df <- merge(comb4, HCP_R_DEMOS, by =c("SUBJID"), all=FALSE)
            
            #Variable for sex, Males=0 
            HCPR_df$Sex_Bin <- as.factor(ifelse(HCPR_df$Gender == "F", 1, ifelse(HCPR_df$Gender == "M", 0, NA)))
            
            #Just include relevant variables
            HCPR_df <- HCPR_df[,c("SUBJID", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Sex_Bin", "Handedness", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj")]
                  #ORRT: ReadEng_Unadj. Language
                  #PVT: PicVocab_Unadj. Language.
                  #FLANKER: Flanker_Unadj. Attention/Executive function.
                  #CCST: CardSort_Unadj. Executive function.
                  #LSWMT: ListSort_Unadj. Working memory.            
                  #Create sessions marker
            HCPR_df$sessions <- "1-3"
            
            #Organize covariates
            HCPR_df$Age_Centered <- as.numeric(HCPR_df$Age_in_Yrs - mean(HCPR_df$Age_in_Yrs))
            HCPR_df$FD_Centered <- as.numeric(HCPR_df$FD_avg - mean(HCPR_df$FD_avg))
            HCPR_df$Handedness <- as.numeric(HCPR_df$Handedness)
            
            #Load in REPLCATION NSAR
            SA_ALLR <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/HCP_analysis/network_sa/REPLICATION/HCP_REPLICATION_NETWORK_SA_SUB_NET_LH_RH_230221.csv")
            SA_ALLR$SUBJID <- gsub("^.{0,4}", "", SA_ALLR$SUBJID)
            SA_ALLR$Network <- gsub("^.{0,8}", "", SA_ALLR$NETWORK)
            
            #Drop Network 0 (medial wall)
            SA_ALLR <- subset(SA_ALLR, Network!=0)
            
            #Reorder CBIG Networks
            mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
            SA_ALLR$NewNetwork <- recode(SA_ALLR$Network, !!!mapping)
            
            #Merge HCP AI with demos
            HCPR_AI <- merge(SA_ALLR, HCPR_df, by=c("SUBJID"), all=FALSE)
            
          #JOIN all HCP    
            #Merge HCP and HCPR dataframes
            HCP_ALL <- merge(HCPR_AI, HCP_SA_ALL, by=c("SUBJID", "FD_avg","DVARS_avg", "Percent_Vols", "Sum_Volumes", "Handedness", "Sex_Bin", "Age_in_Yrs", "sessions", "Network", "NewNetwork","NETWORK", "LH_SA", "RH_SA", "Age_Centered", "FD_Centered", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj"), all=TRUE)
            
            #Create SA_LAT
            HCP_ALL$SA_LAT <- (HCP_ALL$RH_SA - HCP_ALL$LH_SA) / (HCP_ALL$LH_SA + HCP_ALL$RH_SA)
            
            #Apply exclusion criteria
            #1. Participants with FD_avg greater than 0.2
            HCP_ALL <- subset(HCP_ALL, FD_avg<.2)
            #2. Participants with avg DVARS greater than 50
            HCP_ALL <- subset(HCP_ALL, DVARS_avg<51)
            #3. % volumes remaining is less than 50% 
            HCP_ALL <- subset(HCP_ALL, Percent_Vols>50)
            #4. Subjects missing handedness
            HCP_ALL <- subset(HCP_ALL, Handedness!="NA")
            
            #Split HCP_ALL into discovery and replication sets
            #1. Random assignment
            subjids <- unique(HCP_ALL$SUBJID) #vector of unique IDs
            set.seed(42) #seed is set for reproducibility
            discovery_prop <- 0.5  # Proportion for discovery dataset
            discovery_subjids <- sample(subjids, size = round(length(subjids) * discovery_prop))
            HCP_ALL$dataset[HCP_ALL$SUBJID %in% discovery_subjids] <- "HCP-DISC"
            HCP_ALL$dataset[HCP_ALL$SUBJID %in% setdiff(subjids, discovery_subjids)] <- "HCP-REP"
            
            #2. Check that two datasets match on covariates, using MatchIt
            HCP_ALL$data_bin <- ifelse(HCP_ALL$dataset=="HCP-DISC", 0, 1) #binarize dataset
            #create matchit model (nearest neighbor, propensity scores)
            m.out1 <- matchit(data_bin ~ Age_in_Yrs + FD_avg + Sex_Bin + Handedness + 
                                Percent_Vols, data = HCP_ALL,
                              method = "nearest", distance = "glm")
            summary(m.out1)
            
            #QQ plots
            #Age
            plot(m.out1, type = "qq", interactive = FALSE,
                 which.xs = ~Age_in_Yrs)
            #Handedness
            plot(m.out1, type = "qq", interactive = FALSE,
                 which.xs = ~Handedness)
            #FD_avg
            plot(m.out1, type = "qq", interactive = FALSE,
                 which.xs = ~FD_avg)
            #Percent_Vols
            plot(m.out1, type = "qq", interactive = FALSE,
                 which.xs = ~Percent_Vols)
            
            #visualize covariates
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPR_Matched_DensityPlots_Covs1_230620.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            plot(m.out1, type = "density", interactive = FALSE,
                 which.xs = ~Age_in_Yrs + Handedness + Percent_Vols)
            dev.off()              # Close device and save as PNG
            
            png("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/png_figures/Study1_HCPR_Matched_DensityPlots_Covs2_230620.png", height = 3.25, width = 3.25, units="in", res= 1200,)
            plot(m.out1, type = "density", interactive = FALSE,
                 which.xs = ~FD_avg + Sex_Bin)
            dev.off()              # Close device and save as PNG
            
#Load HCPD
            #Load data descriptor (Box)
            HCPD_data <- read_excel("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_LS_2.0_subject_completeness.xlsx")
            #Load IDs of 583 participants with REST fMRI data post-preproc
            PARC_IDS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/subjids/REST_ids_231025.txt")
            DVARS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/DVARS_avg_HCPD_ALL_230517.csv")
            FD <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/FD_avg_HCPD_ALL_230517.csv")
            VOLS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/Kong2019_parc_fs6/motion_metrics/RemainingVols_HCPD_230517.csv")
            #EHI
            EHI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_EHI_230616.csv")
            EHI <- EHI[,c("subjectkey", "hcp_handedness_score")]
            #ORRT: ReadEng_Unadj. Language
            ORRT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_ORRT_231025.csv")
            ORRT <- ORRT[,c("subjectkey", "tbx_reading_score")]
            names(ORRT)[2] <- "ReadEng_Unadj"
            #PVT: PicVocab_Unadj. Language.
            PVT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_TPVT_231025.csv")
            PVT <- PVT[,c("subjectkey", "tpvt_uss")]
            names(PVT)[2] <- "PicVocab_Unadj"
            #FLANKER: Flanker_Unadj. Attention/Executive function.
            FLANK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_FLANKER_231025.csv")
            FLANK <- FLANK[,c("subjectkey", "nih_flanker_unadjusted")] #scaled score, but not age-adjusted
            names(FLANK)[2] <- "Flanker_Unadj"
            #CCST: CardSort_Unadj. Executive function.
            CCST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_DCCS_231025.csv")
            CCST <- CCST[,c("subjectkey", "nih_dccs_unadjusted")]
            names(CCST)[2] <- "CardSort_Unadj"
            #LSWMT: ListSort_Unadj. Working memory.            
            LSWMT <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/HCP-D/Participants/HCD_LSWMT_231025.csv")
            LSWMT <- LSWMT[,c("subjectkey", "uss")]
            names(LSWMT)[2] <- "ListSort_Unadj"
            
            #merge with HCPD Data
            HCPD_data <- merge(HCPD_data, EHI, by="subjectkey", all=TRUE)
            HCPD_data <- merge(HCPD_data, ORRT, by="subjectkey", all=TRUE)
            HCPD_data <- merge(HCPD_data, PVT, by="subjectkey", all=TRUE)
            HCPD_data <- merge(HCPD_data, FLANK, by="subjectkey", all=TRUE)
            HCPD_data <- merge(HCPD_data, CCST, by="subjectkey", all=TRUE)
            HCPD_data <- merge(HCPD_data, LSWMT, by="subjectkey", all=TRUE)
            
            #Clean up motion_metrics
            names(DVARS)[1] <- "SUBJID"
            names(FD)[1] <- "SUBJID"
            names(VOLS)[1] <- "SUBJID"
            
            FD$SUBJID <- gsub("^.{0,4}", "", FD$SUBJID) #remove "sub-" string
            DVARS$SUBJID <- gsub("^.{0,4}", "", DVARS$SUBJID) #remove "sub-" string
            VOLS$SUBJID <- gsub("^.{0,4}", "", VOLS$SUBJID) #remove "sub-" string
            
            #Clean up demos dataset
            HCPD_data <- HCPD_data[,c("src_subject_id", "sex", "interview_age", "Full_MR_Compl", "hcp_handedness_score", "ReadEng_Unadj", "PicVocab_Unadj", "CardSort_Unadj", "ListSort_Unadj", "Flanker_Unadj")]
            HCPD_data <- subset(HCPD_data, src_subject_id!="HCA or HCD subject id")
            names(HCPD_data)[1] <- "SUBJID"
            
            #Rename handedness for consistency
            HCPD_data$Handedness <- HCPD_data$hcp_handedness_score
            
            #Age Years (convert from months to years)
            HCPD_data$interview_age <- as.numeric(as.character(HCPD_data$interview_age))
            HCPD_data$Age_in_Yrs <- HCPD_data$interview_age/12
            
            #Create dataset marker
            HCPD_data$dataset <- "HCPD"
            
            #Merge datasets
            HCPD_data <- merge(HCPD_data, PARC_IDS, by =c("SUBJID"), all=FALSE)
            HCPD_data <- merge(HCPD_data, DVARS, by =c("SUBJID"), all=FALSE)
            HCPD_data <- merge(HCPD_data, FD, by =c("SUBJID"), all=FALSE)
            HCPD_data <- merge(HCPD_data, VOLS, by =c("SUBJID"), all=FALSE)
            
            #Exclusions
            #1. Participants with FD_avg greater than 0.2
            HCPD_data <- subset(HCPD_data, FD_avg<.2)
            #2. Participants with avg DVARS greater than 50
            HCPD_data <- subset(HCPD_data, DVARS_avg<51)
            #3. % volumes remaining is less than 50% (474 * 4 = 1896) or 253*6=1518 for ages 5-7
            HCPD_data$Percent_Vols <- ifelse(HCPD_data$Age_in_Yrs < 7, (HCPD_data$Sum_Volumes / (253*6)), (HCPD_data$Sum_Volumes / (474*4)))
            HCPD_data <- subset(HCPD_data, Percent_Vols>.5)
            HCPD_data$Percent_Vols <- HCPD_data$Percent_Vols*100
            #4. Participants missing handedness
            HCPD_data <- subset(HCPD_data, Handedness!="NA")
            
            #subset to relevant variables
            HCPD_data <- HCPD_data[,c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "sex", "Handedness", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj")]
            
            #Load specialization data (SAI/NSAR)
            HCPD_SAI <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study3_Dissertation/HCPD_analysis/network_sa/REST/NETWORK_SA_SUB_NET_LH_RH_230214.csv")
            HCPD_SAI$SUBJID <- gsub("^.{0,4}", "", HCPD_SAI$SUBJID)
            HCPD_SAI$Network <- gsub("^.{0,8}", "", HCPD_SAI$NETWORK)
            
            #Drop Network 0 (medial wall)
            HCPD_SAI <- subset(HCPD_SAI, Network!=0)
            #Reorder CBIG Networks
            mapping <- c("1" = 12, "2" = 6, "3" = 3, "4" = 13, "5" = 5, "6" = 1, "7" = 8, "8" = 7, "9" = 10, "10" = 11, "11" = 15, "12" = 14, "13" = 4, "14" = 2, "15" = 17, "16" = 16, "17" = 9)
            HCPD_SAI$NewNetwork <- recode(HCPD_SAI$Network, !!!mapping)
            
            #Create SA LAT variable
            HCPD_SAI$SA_LAT <- (HCPD_SAI$RH_SA - HCPD_SAI$LH_SA) / (HCPD_SAI$LH_SA + HCPD_SAI$RH_SA)
            
            #Merge in AI, demos
            HCPD_AI <- merge(HCPD_data, HCPD_SAI, by =c("SUBJID"), all=FALSE)
            
            #Organize covariates
            HCPD_AI$Age_Centered <- as.numeric(HCPD_AI$Age_in_Yrs - mean(HCPD_AI$Age_in_Yrs))
            HCPD_AI$FD_Centered <- as.numeric(HCPD_AI$FD_avg - mean(HCPD_AI$FD_avg))
            HCPD_AI$Sex_Bin <- as.factor(ifelse(HCPD_AI$sex == "M", 0, 1)) #recode males as 0 and Females as 1
            HCPD_AI$Handedness <- as.numeric(HCPD_AI$Handedness)
            
#NSD (demos only)
            #Load datasets
            DEMOS <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Data/supercomputer_backup/NSD_analysis/NSD_BIDS/participants.csv")
            FD_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/FD_avg_NSD_REST_230515.csv")
            FD_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/FD_avg_NSD_TASK_230515.csv")
            DVARS_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/DVARS_avg_NSD_REST_230515.csv")
            DVARS_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/DVARS_avg_NSD_TASK_230515.csv")
            TOTAL_REST <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_REST/motion_metrics/RemainingVols_NSD_REST_230515.csv")
            TOTAL_TASK <- read.csv("C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Analysis/Study1_Dissertation/NSD_analysis/Kong2019_parc_all_TASK/motion_metrics/RemainingVols_NSD_TASK_230515.csv")
            
            #Format data
            names(FD_REST)[1] <- "SUBJID"
            names(FD_REST)[2] <- "avg_FD_REST"
            names(FD_TASK)[1] <- "SUBJID"
            names(FD_TASK)[2] <- "avg_FD_TASK"
            names(DVARS_REST)[1] <- "SUBJID"
            names(DVARS_REST)[2] <- "avg_DVARS_REST"
            names(DVARS_TASK)[1] <- "SUBJID"
            names(DVARS_TASK)[2] <- "avg_DVARS_TASK"
            names(TOTAL_REST)[1] <- "SUBJID"
            names(TOTAL_REST)[2] <- "VOLS_REST"
            names(TOTAL_TASK)[1] <- "SUBJID"
            names(TOTAL_TASK)[2] <- "VOLS_TASK"
            
            #Merge datasets
            comb1 <- merge(DEMOS, FD_REST, by =c("SUBJID"), all=FALSE)
            comb2 <- merge(comb1, FD_TASK, by =c("SUBJID"), all=FALSE)
            comb3 <- merge(comb2, DVARS_REST, by =c("SUBJID"), all=FALSE)
            comb4 <- merge(comb3, DVARS_TASK, by =c("SUBJID"), all=FALSE)
            comb5 <- merge(comb4, TOTAL_REST, by =c("SUBJID"), all=FALSE)
            NSD_df <- merge(comb5, TOTAL_TASK, by =c("SUBJID"), all=FALSE)
            
            #Long format (for plotting)
            NSD_FD <- NSD_df %>%
              pivot_longer(cols = starts_with("avg_FD_"),
                           names_to = "dataset", 
                           values_to = "FD_avg")
            NSD_FD$dataset <- gsub("^.{0,7}", "", NSD_FD$dataset) 
            
            NSD_DVARS <- NSD_df %>%
              pivot_longer(cols = starts_with("avg_DVARS_"),
                           names_to = "dataset", 
                           values_to = "DVARS_avg")
            NSD_DVARS$dataset <- gsub("^.{0,10}", "", NSD_DVARS$dataset) 
            
            NSD_TOT <- NSD_df %>%
              pivot_longer(cols = starts_with("VOLS_"),
                           names_to = "dataset", 
                           values_to = "Sum_Volumes")
            NSD_TOT$dataset <- gsub("^.{0,5}", "", NSD_TOT$dataset) 
            
            #Merge datasets
            comb1 <- merge(NSD_FD, NSD_DVARS, by =c("SUBJID", "dataset"), all=FALSE)
            NSD_df2 <- merge(comb1, NSD_TOT, by =c("SUBJID", "dataset"), all=FALSE)
            
            #Create % vols
            NSD_df2$Percent_Vols <- (NSD_df2$Sum_Volumes / (184*12))*100 #Number of volumes after skip4 = 184 x12 runs (same for Task and Rest)
            
            #Tidy up dataset
            NSD_df2 <- NSD_df2[,c("SUBJID", "dataset", "age", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg")]
            names(NSD_df2)[3] <- "Age_in_Yrs"
            
            #Set variables not available to NA
            NSD_df2$Handedness <- NA
            NSD_df2$Sex_Bin <- NA
            NSD_df2$SA_LAT <- NA
            NSD_df2$NewNetwork <- 1
            NSD_df2$RH_SA <- NA
            NSD_df2$LH_SA <- NA
            NSD_df2$Age_Centered <- NA
            NSD_df2$FD_Centered <- NA
            NSD_df2$Sex_Bin <- NA
            NSD_df2$NETWORK <- NA
            NSD_df2$Network <- NA
            NSD_df2$ReadEng_Unadj <- NA
            NSD_df2$PicVocab_Unadj <- NA
            NSD_df2$Flanker_Unadj <- NA
            NSD_df2$CardSort_Unadj <- NA
            NSD_df2$ListSort_Unadj <- NA
            
            
            
#Merge all datasets
  study1 <- merge(HCP_ALL, HCPD_AI, by=c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Handedness","Sex_Bin", "SA_LAT", "NewNetwork", "RH_SA", "LH_SA", "Age_Centered", "FD_Centered", "Sex_Bin", "Network", "NETWORK", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj"), all=TRUE)
  study1 <- merge(study1, NSD_df2, by=c("SUBJID", "dataset", "Age_in_Yrs", "FD_avg", "Sum_Volumes", "Percent_Vols", "DVARS_avg", "Handedness","Sex_Bin", "SA_LAT", "NewNetwork", "RH_SA", "LH_SA", "Age_Centered", "FD_Centered", "Sex_Bin", "Network", "NETWORK", "ReadEng_Unadj", "PicVocab_Unadj", "Flanker_Unadj", "CardSort_Unadj", "ListSort_Unadj"), all=TRUE)
            
            
#For replication and transparency purposes, write out entire datset used for stats
write.csv(study1,"C:/Users/maddy/Box/Autism_Hemispheric_Specialization/Figures/study1_figures/csv_files/study1_HCP_AI_entirety_230630.csv",row.names=FALSE, quote=FALSE)
            
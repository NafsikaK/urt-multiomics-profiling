

###############################################################################
########## APROFESSIONAL SAMPLING ##########################
############# QUESTIONNAIRE DATA ANALYSIS & vISUALIZARION ####################
##############################################################################


#-----
# We have two questions and we used a Likert Scale questionnaire (scale 1-5)

library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggsignif)
library(FSA)
library(boot)
getwd()

# Load the .csv file containing the data 
library(readr)
Q <- read_delim("11-questionnaire_data.csv", 
                                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
#Remove missing (NA) values:
Q1 <- na.omit(Q)
#with the following function you can count distinct values (even if they are characters)
n_distinct(Q1$ID) # 44 participants

#Select for the first question as follows:
Q1_1 <- subset(Q1, Question == "I would characterize  my experience with the…")
# Make a vector for the discomfort levels - regarding the first question:
dis_levels <- c("Very comfortable", "Comfortable",
                  "Neither uncomfortable nor comfortable",
                  "Uncomfortable","Very uncomfortable") 
# wer reverse the order so that it fits nicely in the plot later on:
dis_levels2 <- rev(dis_levels)
  
Q1_1$Likert_Scale <- factor(Q1_1$Likert_Scale)
  # Calculate percentages based on discomfort
# Group by Sample_Type and Likert_Scale, then count occurrences
grouped_data <- Q1_1 %>%
  group_by(Sample_Type, Likert_Scale) %>%
  summarise(n = n(), .groups = 'drop')  # Adding .groups = 'drop' to avoid grouped result warning

# Calculate the ratio
library(scales)
library(ggsignif)
percentData_1 <- grouped_data %>%
  group_by(Sample_Type) %>%
  mutate(ratio = percent(n / sum(n)))
  

sign_Q1 <- data.frame(
  group=c("MLF", "Saliva","MTS", "OPS"),
  value=c(1.05,1.05,1.05,1.05),
  label=c("ns","ns","ns","ns")
)
  # Plot Q1
  percent= c("0", "25", "50", "75", "100")
  sampletypes <- c("Saliva", "MLF", "MTS","OPS","NPS")
  one <- ggplot(Q1_1,aes(x=factor(Sample_Type),fill = factor(Likert_Scale, levels = dis_levels2)))+
    geom_bar(position="fill")+ theme_minimal() +coord_flip ()+
    geom_text(data=percentData_1, aes(y=n,label=ratio), 
              position=position_fill(vjust=0.6)) + 
    # Adding significance labels manually
    geom_text(aes(x = 4.38, y = 1.04, label = "ns"), color = "#53565B", size = 4, vjust = -0.5) +
    geom_text(aes(x =2.38, y = 1.04, label = "ns"), color = "#53565B", size = 4, vjust = -0.5) +
    geom_segment(aes(x=4.8, xend = 4.2, y=1.02, yend = 1.02), size=0.4, color="#53565B")+
    geom_segment(aes(x=2.8, xend = 2.2, y=1.02, yend = 1.02), size=0.4, color="#53565B")+
    scale_fill_brewer(palette = "RdYlBu") +
      labs(
        x = "Sample Type",
        y = "Percentage (%)",
        title = "Q1.How would you characterize your experience with each sample type?",
        subtitle = "N = 44"
      )  +
     scale_y_continuous(labels = percent) + scale_x_discrete(limits = rev(sampletypes))
    # quest1 <- one + 
    #   guides(fill = guide_legend(title="Discomfort Levels", reverse = TRUE))+
    #   theme(
    #     axis.title.x = element_text(size = 14),  # X-axis title size
    #     axis.title.y = element_text(size = 14),  # Y-axis title size
    #     axis.text.x = element_text(size = 12),   # X-axis labels size
    #     axis.text.y = element_text(size = 12), # Y-axis labels size
    #     legend.position = "top"
    #   )
    # quest1
    
    
    quest1 <- one +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        plot.title   = element_text(size = 20, face = "bold"),
        plot.subtitle= element_text(size = 16)
      ) +theme(
        legend.title = element_text(size = 16),  # title
        legend.text  = element_text(size = 14)   # labels
      )+
      guides(fill = guide_legend(title="Discomfort Levels", reverse = TRUE))
    quest1
    # save as png image in specific directory with 600*350 resolution
  
    #Save my plot as .tiff:
    ggsave(filename = "Q1(30-06-2025).png", plot = quest1,
           device = "png", height = 8, width = 15, units = "in",
           dpi = 300)
    
    
    
  
    combined_plot0 <- (quest1 / quest2) 
    
    combined_plot0
    
    #Save my plot as .tiff:
    ggsave(filename = "combined_questionnaire.tiff", plot = combined_plot0,
           device = "tiff", height = 11, width = 17, units = "in",
           dpi = 300)
    
#Add sign.
str(Q1_1)
str(percentData_1)


  ##################
  #Select for the second question
  Q2 <- subset(Q1, Question == "I would participate in a study requiring regular sampling with…")
  # Make a vector for the agreement levels (Q2)
  agr_levels <- c("Strongly Disagree", "Disagree",
                  "Neither agree nor disagree",
                  "Agree","Strongly Agree") 
 
  # Group by Sample_Type and Likert_Scale, then count occurrences
  grouped_data2 <- Q2 %>%
    group_by(Sample_Type, Likert_Scale) %>%
    summarise(n = n(), .groups = 'drop')  # Adding .groups = 'drop' to avoid grouped result warning
  
  # Calculate the ratio
  percentData_2 <- grouped_data2 %>%
    group_by(Sample_Type) %>%
    mutate(ratio = percent(n / sum(n)))
  
  # Plot Q2
  percent= c("0", "25", "50", "75", "100")
  sampletypes <- c("Saliva", "MLF", "MTS","OPS","NPS")
  two <- ggplot(Q2,aes(x=factor(Sample_Type),fill = factor(Likert_Scale, levels = agr_levels)))+
    geom_bar(position="fill")+ theme_minimal() + coord_flip ()+
    geom_text(data=percentData_2, aes(y=n,label=ratio), 
              position=position_fill(vjust=0.6)) + scale_fill_brewer(palette = "RdYlBu") +
    labs(
      x = "Sample Type",
      y = "Percentage (%)",
      title = "Q2.Would you participate in a study requiring regular sampling with the respective sample type?",
      subtitle= "N = 44"
    )  + 
    # Adding significance labels manually
    geom_text(aes(x = 4.38, y = 1.04, label = "ns"), color = "#53565B", size = 4, vjust = -0.5) +
    geom_text(aes(x =2.38, y = 1.04, label = "ns"), color = "#53565B", size = 4, vjust = -0.5) +
    geom_segment(aes(x=4.8, xend = 4.2, y=1.02, yend = 1.02), size=0.4, color="#53565B")+
    geom_segment(aes(x=2.8, xend = 2.2, y=1.02, yend = 1.02), size=0.4, color="#53565B")+
    scale_y_continuous(labels = percent) + scale_x_discrete(limits = rev(sampletypes))
  quest2 <- two + theme_minimal() +
    theme(
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14),
      plot.title   = element_text(size = 20, face = "bold"),
      plot.subtitle= element_text(size = 16)
    ) +theme(
      legend.title = element_text(size = 16),  # title
      legend.text  = element_text(size = 14)   # labels
    )+
    guides(fill = guide_legend(title="Agreement Levels", reverse = TRUE, label.theme = element_text(size = 14)))
  quest2
  # save as png image in specific directory with 600*350 resolution
  #png(file="Q2(10-07-2024).png",
   #   width=900, height=600)
  #quest2
  #dev.off()
  
  #Save my plot as .tiff:
  ggsave(filename = "Q2(30-06-2025).png", plot = quest2,
         device = "png", height = 8, width = 17, units = "in",
         dpi = 300)
  
  # Combine the plots with patchwork
  library(patchwork)
 combined_plot <- (quest1 / quest2) +
   plot_layout(guides = "collect", widths = c(0.8, 0.8)) &
   theme(legend.position = "top")
 combined_plot
##############################


### ----------- Friedman Test -------------------- 
#Given that my data are dependant (repeated measurements on the same persons)
#I need to use the Friedman test instead of the Kruskal-Wallis or the Mann-U Whitney Test

#For all the Sample Types in Question 1:
Q1_FT <- friedman.test(Score ~ Sample_Type | ID, data = Q1_1)
print(Q1_FT) #We see that there is a significant result

#Now, to apply a post-hoc:
# Load necessary library
library(rstatix)
library(coin)
# Perform pairwise comparisons using Wilcoxon signed-rank test with Bonferroni correction
Q1_Post_Hoc <- Q1_1 %>%
  pairwise_wilcox_test(
    Score ~ Sample_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ) 

print(Q1_Post_Hoc)

#For all the Sample Types in Question 2:
Q2_FT <- friedman.test(Score ~ Sample_Type | ID, data = Q2)
print(Q2_FT) #We see that there is a significant result

#Now, to apply a post-hoc:

# Perform pairwise comparisons using Wilcoxon signed-rank test with Bonferroni correction
Q2_Post_Hoc <- Q2 %>%
  pairwise_wilcox_test(
    Score ~ Sample_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ) 

print(Q2_Post_Hoc)





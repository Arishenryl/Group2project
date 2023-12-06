#Group2project.R
#Authors @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
#Purpose - pending
#Date Dec 6th 2023
#Version #1

# Load required libraries
install.packages("ggpubr")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(rstatix)
library(stats)
library(tidyverse)

# Read the dataset from a tsv file
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)

# Display a summary & View the dataset interactively
glimpse(group2dataset) 
view(group2dataset)

#statisitcal analyses 

#T-test
g2.t_testresult = t.test(data=group2dataset, Error_counts ~ Instrument)
summary(g2.t_testresult)

#ANOVA
anovag2_result = aov(Error_counts ~ Position * Variant, data = group2dataset)
summary(anovag2_result)

#Figures

#Boxplot
our_g2colors <- c("#dd1c77", "#3182bd")

ggplot(group2dataset, aes(x = Instrument, y = Error_counts, color = Variant)) +
  geom_boxplot(outlier.shape = NA, fill = "white") +
  geom_jitter(shape = 20) +
  scale_color_manual(values = our_g2colors) +
  labs(x = "Instrument", y = "Error Counts", title = "Error Counts in Spike gene of SARS-CoV-2") +
  facet_wrap(~Variant, ncol = 2) +
  theme(
    title = element_text(face = "bold", size = 12, color = "purple"),
    axis.title = element_text(face = "bold", size = 10, color = "purple"),
    axis.text = element_text(face = "bold", size = 9, color = "darkgray"),
    strip.text.x = element_text(face = "bold", size = 8, color = "black"),
    legend.title = element_text(face = "bold", size = 8, color = "purple"),
    legend.text = element_text(face = "bold", size = 8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)  
  )

#T-test plot
ggplot(group2dataset, aes(x = Instrument, y = Error_counts, fill = Instrument)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21) +
  theme_bw() +
  geom_signif(comparisons = list(c("Illumina", "PacBio")), map_signif_level = TRUE, 
              test = "wilcox.test", test.args = list(exact = FALSE, correct = TRUE)) +
  scale_fill_manual(values = setNames(our_g2colors, unique(group2dataset$Instrument))) +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Liann", color = "blue", alpha = 0.5) +
  theme(
    axis.text = element_text(color = "purple", face = "bold"),  
    axis.title = element_text(color = "purple", face = "bold"),  
    legend.text = element_text(color = "purple", face = "bold"),  
    legend.title = element_text(color = "purple", face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "purple")  
  ) +
  labs(title = "Mean Differences Between Sequencing Techniques")

#ANOVA plot
ggplot(data = meang2_data, aes(x = Position, y = mean_Error_counts, fill = Variant)) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.6) +  # Column dot plot
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Liann", color = "blue", alpha = 0.5) +
    labs(title = "Distribution of Mean Error Counts Along SARS-CoV-2 Spike Gene",
         x = "Position",
         y = "Mean Error Counts") +
    scale_fill_manual(values = setNames(our_g2colors, unique(meang2_data$Variant))) + 
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, color = "purple", face = "bold"), 
        legend.position = "bottom",
        axis.text = element_text(color = "purple", face = "bold"),  
        axis.title = element_text(color = "purple", face = "bold"), 
        legend.text = element_text(color = "purple", face = "bold"),  
        legend.title = element_text(color = "purple", face = "bold")  
    )



















# Possible 3rd hypothesis- Alina & Sidorela
ggplot(group2dataset, aes(x = Variant, y = Error_counts)) +
    
    # Add a boxplot to visualize the distribution of 'Error_counts' for each 'Instrument'
    geom_boxplot(outlier.shape = NA) +
    
    # Add jittered points for each data point to show individual observations
    geom_jitter(shape = 20)

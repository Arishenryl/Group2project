# Finalg2project.R
# Authors: @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
# Purpose: This script aims to conduct a comprehensive analysis of 
# error counts in the Spike gene of SARS-CoV-2 for different sequencing instruments and variants. 
# The statistical analyses include a t-test comparing Illumina and PacBio sequencing, 
# an ANOVA examining differences in error distribution along the SARS-CoV-2 length, 
# and the calculation of mean error counts for future use in the ANOVA plot.
# Date: Dec 6th, 2023
# Version: 1 
# Script Documentation - Antonio & Sidorela

# Load required libraries - Alina
install.packages("ggpubr")  # Install the ggpubr package
library(dplyr)               # Load the dplyr library for data manipulation
library(ggplot2)             # Load the ggplot2 library for data visualization
library(ggpubr)              # Load the ggpubr library for enhancing plots
library(ggsignif)            # Load the ggsignif library for adding annotations to plots
library(rstatix)             # Load the rstatix library for statistical functions in plots
library(stats)               # Load the stats library for basic statistical functions
library(tidyverse)           # Load the tidyverse library for data manipulation and visualization

# Read the dataset from a tsv file - Alina
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)  # group2dataset stores the dataset read for statistical analysis & plot creation

# Display a summary & View the dataset interactively - Saherah
glimpse(group2dataset)  
view(group2dataset)     

# Statistical analyses - Saherah

# T-test: conducted for comparison of error counts between Illumina and PacBio sequencing - Saherah
> pairwise_t_test_result <- pairwise.t.test(group2dataset$Error_counts, group2dataset$Instrument, p.adjust.method = "bonferroni")
> pairwise_t_test_result

# ANOVA: conducted to determine differences in error distribution along SARS-CoV-2 length - Saherah
anovag2_result <- aov(Error_counts ~ Position * Variant, data = group2dataset)  # Perform ANOVA
summary(anovag2_result)  # Display the summary of ANOVA results

# Calculate, filter & store mean data for future use in ANOVA plot - Liann
meang2_data <- group2dataset %>%
  group_by(Position, Variant) %>%
  summarise(mean_Error_counts = mean(Error_counts))  # Calculate mean values

# Figures - Liann

# P-W T-test box plot: box plot that compares mean error counts Illumina and PacBio via P-W t-test results - Liann
ggplot(group2dataset, aes(x = Instrument, y = Error_counts, fill = Instrument)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(shape = 21) +
    theme_bw() +
    stat_compare_means(
        aes(label = ifelse(p < 0.05, "*", "")),
        method = "t.test",
        paired = FALSE,
        comparisons = list(c("Illumina", "PacBio")),
        label.y = max(group2dataset$Error_counts) + 1
    ) +
    scale_fill_manual(values = setNames(c("#dd1c77", "#3182bd"), unique(group2dataset$Instrument))) +
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Liann", color = "blue", alpha = 0.5) +
    theme(
        axis.text = element_text(color = "purple", face = "bold"),  
        axis.title = element_text(color = "purple", face = "bold"),  
        legend.text = element_text(color = "purple", face = "bold"),  
        legend.title = element_text(color = "purple", face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "purple")  
    ) +
    labs(title = "Pairwise Mean Comparison of Error Counts Between Techniques")

#Mean-Variant-Instrument plot: box plot displaying the distribution of mean error counts along S-gene - Liann
ggplot(group2dataset, aes(x = Variant, y = Error_counts, color = Instrument)) +
    geom_boxplot(outlier.shape = NA, fill = "white") +
    geom_jitter(shape = 20) +
    scale_color_manual(values = our_g2colors) +
    labs(x = "Variant", y = "Error Counts", title = "Error Counts in Spike Gene of SARS-CoV-2 Grouped by Instrument and Variant") +
    facet_wrap(~Instrument, ncol = 2) +
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

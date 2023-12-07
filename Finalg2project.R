# Finalg2project.R
# Authors: @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
# Purpose: [Pending]
# Date: Dec 6th, 2023
# Version: 1 - Antonio & Sidorela

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

# Statistical analyses - Saherah & Liann 

# T-test: conducted for comparison of error counts between Illumina and PacBio sequencing - Saherah
g2.t_testresult <- t.test(data = group2dataset, Error_counts ~ Instrument)  # Perform a t-test
summary(g2.t_testresult)  # Display the summary of the t-test results

# ANOVA: conducted to determine differences in error distribution along SARS-CoV-2 length - Saherah
anovag2_result <- aov(Error_counts ~ Position * Variant, data = group2dataset)  # Perform ANOVA
summary(anovag2_result)  # Display the summary of ANOVA results

# Calculate, filter & store mean data for future use in ANOVA plot - Liann
meang2_data <- group2dataset %>%
  group_by(Position, Variant) %>%
  summarise(mean_Error_counts = mean(Error_counts))  # Calculate mean values

# Figures - Liann & Alina

# Boxplot: compares the error counts in the spike gene of two SARS-CoV-2 variants - Alina
our_g2colors <- c("#dd1c77", "#3182bd")  # Define custom colors

boxplotg2_plot <- ggplot(group2dataset, aes(x = Instrument, y = Error_counts, color = Variant)) +
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

# T-test box plot: box plot that compares error counts Illumina and PacBio via t-test results - Liann
tg2_test_plot <- ggplot(group2dataset, aes(x = Instrument, y = Error_counts, fill = Instrument)) +
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

# ANOVA plot: column dot plot displaying the distribution of mean error counts along S-gene via ANOVA results - Liann
ggplot(data = meang2_data, aes(x = Position, y = mean_Error_counts, fill = Variant)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.6) +  # Column dot plot
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "Liann", color = "blue", alpha = 0.5) +
  labs(
    title = "Distribution of Mean Error Counts Along SARS-CoV-2 Spike Gene",
    x = "Position",
    y = "Mean Error Counts"
  ) +
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


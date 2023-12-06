#Group2project.R
#Authors @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
#Purpose - pending
#Date Dec 6th 2023
#Version #1

# Load required libraries
library(tidyverse)
library(ggplot2)
library(stats)

# Read the dataset from a tsv file
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)

# Display a summary & View the dataset interactively
glimpse(group2dataset) 
view(group2dataset)

#plots
# Possible 3rd hypothesis- Alina & Sidorela
ggplot(group2dataset, aes(x = Variant, y = Error_counts)) +
    
    # Add a boxplot to visualize the distribution of 'Error_counts' for each 'Instrument'
    geom_boxplot(outlier.shape = NA) +
    
    # Add jittered points for each data point to show individual observations
    geom_jitter(shape = 20)

#statisitcal analysis 

#T-test

#ANOVA
anovag2_result <- aov(Error_counts ~ Position * Variant, data = group2dataset)
summary(anovag2_result)

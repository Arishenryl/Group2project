#Group2project.R
#authors @Arishenryl, @ashevtsova, @ATRP116,@ellasterkaj,@saherah
#purpose - pending
#date Dec 5th 2023
#Version #1

# Load required libraries
library(tidyverse)
library(ggplot2)

# Read the dataset from a tsv file
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)

# Display a summary & View the dataset interactively
glimpse(group2dataset) 
view(group2dataset)

#Creates a boxplot of the distribution of error counts across different instruments/variants in the dataset - Alina & Sidorela
ggplot(group2dataset, aes(x=Instrument, y=Error_counts, color=Variant)) + geom_boxplot(outlier.shape=NA) + geom_jitter(shape=20)

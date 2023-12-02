#Group2project.R
#Authors @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
#Purpose - pending
#Date Dec 5th 2023
#Version #1

# Load required libraries
library(tidyverse)
library(ggplot2)

# Read the dataset from a tsv file
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)

# Display a summary & View the dataset interactively
glimpse(group2dataset) 
view(group2dataset)

# Creates a boxplot of the distribution of error counts across different instruments/variants in the dataset - Alina & Sidorela
ggplot(group2dataset, aes(x = Instrument, y = Error_counts, color = Variant)) +
  
  # Add a boxplot to visualize the distribution of 'Error_counts' for each 'Instrument'
  geom_boxplot(outlier.shape = NA) +
  
  # Add jittered points for each data point to show individual observations
  geom_jitter(shape = 20)

# Create a lollipop plot representing the Spike Gene Position for different variants and instruments - Liann
ggplot(group2dataset, aes(x = Variant, y = Position, color = Instrument, size = Error_counts)) +
  
  # Add line segments representing the Spike Gene Position
  geom_segment(aes(xend = Variant, yend = 0), alpha = 0.7, size = 1) + 
  
  # Add points at Spike Gene Position on the y-axis
  geom_point(aes(y = 0), alpha = 0.7, size = 3) +  
  
  # Set plot labels and attributes
  labs(
    title = "Spike Gene Position vs Variant",  # Title of the plot
    x = "Variant",                            # Label for x-axis
    y = "Spike Gene Position",                # Label for y-axis
    color = "Instrument",                      # Color legend label
    linewidth = "Error Counts"                # Legend label for size
  ) +
  
  # Use a minimal theme for the plot
  theme_minimal() +
  
  # Create facets (subplots) based on the 'Instrument' variable
  facet_wrap(~Instrument)

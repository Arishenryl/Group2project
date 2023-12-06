#Group2project.R
#Authors @Arishenryl, @ashevtsova, @ATRP116, @ellasterkaj, @saherah
#Purpose - pending
#Date Dec 6th 2023
#Version #1

# Load required libraries
library(tidyverse)
library(ggplot2)
library(stats)
library(ggpubr)
library(rstatix)
install.packages("ggpubr")

# Read the dataset from a tsv file
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)

# Display a summary & View the dataset interactively
glimpse(group2dataset) 
view(group2dataset)

#plots
#1st hypothesis
#boxplot
> our_colors=c("#dd1c77", "#3182bd")
> ggplot(group2dataset, aes(x = Instrument, y = Error_counts, color = Variant)) + geom_boxplot(outlier.shape = NA, fill="white") + geom_jitter(shape = 20) + scale_color_manual(values=our_colors) + labs(x="Instrument", y="Error Counts", title="Error Counts in Spike gene of SARS-CoV-2") + facet_wrap(~Variant, ncol = 2) + theme(title = element_text(face="bold", size=12, color="purple"), axis.title = element_text(face="bold", size=10, color="purple"), axis.text = element_text(face="bold", size=9, color="darkgray"), strip.text.x = element_text(face="bold", size=8, color="black"), legend.title=element_text(face="bold", size=8, color="purple"), legend.text=element_text(face="bold", size=8), legend.position="right" )

#2nd hypothesis 


#statisitcal analysis 

#T-test
t.test(data=group2dataset, Error_counts ~ Instrument)

#ANOVA
anovag2_result <- aov(Error_counts ~ Position * Variant, data = group2dataset)
summary(anovag2_result)








# Possible 3rd hypothesis- Alina & Sidorela
ggplot(group2dataset, aes(x = Variant, y = Error_counts)) +
    
    # Add a boxplot to visualize the distribution of 'Error_counts' for each 'Instrument'
    geom_boxplot(outlier.shape = NA) +
    
    # Add jittered points for each data point to show individual observations
    geom_jitter(shape = 20)

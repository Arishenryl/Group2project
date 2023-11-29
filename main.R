library(tidyverse)
library(ggplot2)
group2dataset <- read_tsv("group2_dataset.tsv", show_col_types = FALSE)
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot( color= "red")

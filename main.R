library(tidyverse)
library(ggplot2)

ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot( color= "red")

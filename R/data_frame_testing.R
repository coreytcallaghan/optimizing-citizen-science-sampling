library(tidyverse)

lev<-readRDS("Data/Modelling data/leverage_results.RDS")
param<-readRDS("Data/Modelling data/parameter_dataset.RDS")


all<-left_join(lev,param)

#should be a lot of 4's 
sort(table(all$SAMPLING_EVENT_IDENTIFIER),decreasing = T)



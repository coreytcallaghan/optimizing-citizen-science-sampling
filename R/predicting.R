library(broom)
library(tidyverse)

load("Data/Modelling data/play_mod_5_km.RData")
load("Data/Spatial data/sydney_grids_5_km.RData")

oneday<-readRDS("Data/Sampling dates 2018/5 km/2018-12-31.RDS")


add_dfbeta<-function(oneday_df,model){
  # appropriate scaling as with the final model
  # fill in values for unsampled grids
  # predict from model
  oneday_df<-augment(model,newdata = oneday_df)
  return(oneday_df) 
}






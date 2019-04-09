library(broom)
library(tidyverse)
library(arm)
library(tidyr)

load("Data/Modelling data/model_5_km_object.RData")

oneday <- readRDS("Data/Sampling dates 2018/5 km/2018-12-31.RDS")


add_dfbeta <- function(grid_size, date){
  
  # read in dataframe for a given date
  df <- readRDS(paste0("Data/Sampling dates 2018/", grid_size, " km/", date, ".RDS"))
  
  # scale the df, according to how the
  # dataframe for modelling was scaled
  df2 <- df %>%
    mutate(s.median_waiting_time=arm::rescale(median_waiting_time)) %>%
    mutate(s.duration=arm::rescale(duration)) %>%
    mutate(s.Number_of_days_sampled=rescale(Number_of_days_sampled)) %>%
    mutate(s.days_since=rescale(days_since)) %>%
    mutate(s.dist_km_nn=rescale(dist_km_nn)) %>%
    mutate(s.neighbor_waiting_time=rescale(neighbor_waiting_time)) %>%
  # fill in the NA values for each of these variables
  # necessary for predicting. Fill in with mean of that column
    replace_na(list(
      s.median_waiting_time=mean(.$s.median_waiting_time, na.rm=TRUE),
      s.duration=mean(.$s.duration, na.rm=TRUE),
      s.Number_of_days_sampled=mean(.$s.Number_of_days_sampled, na.rm=TRUE),
      s.days_since=mean(.$s.days_since, na.rm=TRUE),
      s.dist_km_nn=mean(.$s.dist_km_nn, na.rm=TRUE),
      s.neighbor_waiting_time=mean(.$s.neighbor_waiting_time, na.rm=TRUE)
      ))
  
  model <- readRDS(paste0("Data/Modelling data/model_", grid_size, "_km_object.RDS"))
  
  predicted_df <- augment(model, newdata = df2)
  
  saveRDS(predicted_df, paste0("Data/predicted_leverage/", grid_size, "_km/", date, ".RDS"))
}


dates_2018 <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by=1)


lapply(dates_2018, function(x) {add_dfbeta(x, 5)})
lapply(dates_2018, function(x) {add_dfbeta(x, 10)})
lapply(dates_2018, function(x) {add_dfbeta(x, 25)})
lapply(dates_2018, function(x) {add_dfbeta(x, 50)})









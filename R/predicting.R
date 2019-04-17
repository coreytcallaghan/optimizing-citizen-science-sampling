library(broom)
library(tidyverse)
library(arm)
library(tidyr)

mod_examp <- readRDS("Data/Modelling data/model_5.l_km_object.RDS")

oneday <- readRDS("Data/Sampling dates 2018/5 km/2018-12-31.RDS")


add_dfbeta <- function(grid_size, date){
  
  # read in dataframe for a given date
  df <- readRDS(paste0("Data/Sampling dates 2018/", grid_size, " km/", date, ".RDS"))
  
  # scale the df, according to how the
  # dataframe for modelling was scaled
  df2 <- df %>%
    mutate(l.s.median_waiting_time=arm::rescale(log(median_waiting_time))) %>%
    mutate(l.s.duration=arm::rescale(log(duration))) %>%
    mutate(l.s.Number_of_days_sampled=arm::rescale(log(Number_of_days_sampled))) %>%
    mutate(l.s.days_since=arm::rescale(log(days_since))) %>%
    mutate(l.s.dist_km_nn=arm::rescale(log(dist_km_nn))) %>%
    mutate(l.s.neighbor_waiting_time=arm::rescale(log(neighbor_waiting_time))) %>%
  # fill in the NA values for each of these variables
  # necessary for predicting. Fill in with mean of that column
    replace_na(list(
      l.s.median_waiting_time=mean(.$l.s.median_waiting_time, na.rm=TRUE),
      l.s.duration=mean(.$l.s.duration, na.rm=TRUE),
      l.s.Number_of_days_sampled=mean(.$l.s.Number_of_days_sampled, na.rm=TRUE),
      l.s.days_since=mean(.$l.s.days_since, na.rm=TRUE),
      l.s.dist_km_nn=mean(.$l.s.dist_km_nn, na.rm=TRUE),
      l.s.neighbor_waiting_time=mean(.$l.s.neighbor_waiting_time, na.rm=TRUE)
      ))
  
  model <- readRDS(paste0("Data/Modelling data/model_", grid_size, ".l_km_object.RDS"))
  
  predicted_df <- augment(model, newdata = df2)
  
  saveRDS(predicted_df, paste0("Data/predicted_leverage/", grid_size, "_km/", date, ".RDS"))
}


dates_2018 <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by=1)


lapply(dates_2018, function(x) {add_dfbeta(5, x)})
lapply(dates_2018, function(x) {add_dfbeta(10, x)})
lapply(dates_2018, function(x) {add_dfbeta(25, x)})
lapply(dates_2018, function(x) {add_dfbeta(50, x)})









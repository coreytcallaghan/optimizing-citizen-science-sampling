### This script is to make summary statistics of the predictions
### for the revision of the paper

# packages
library(dplyr)
library(ggplot2)
library(BBmisc)
library(ggcorrplot)
library(corrplot)
library(lme4)
library(standardize)
library(tibble)
library(visreg)
library(arm)
library(summarytools)


params <- readRDS("Data/Modelling data/parameter_dataset.RDS")

grid_50 <- params %>%
  dplyr::filter(grid_size==50) %>%
  rename(median_sampling_interval=median_waiting_time) %>%
  rename(nearest_neighbor_sampling_interval=neighbor_waiting_time) %>%
  rename(days_since_last_sample=days_since) %>%
  rename(number_of_unique_days_sampled=Number_of_days_sampled) %>%
  rename(distance_to_nearest_sampled_grid_km=dist_km_nn) %>%
  rename(sampled_or_not=sampled) %>%
  dplyr::select(1:3, 5:6, 8:9)

view(dfSummary(grid_50))

view(dfSummary(grid_50, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))


grid_25 <- params %>%
  dplyr::filter(grid_size==25) %>%
  rename(median_sampling_interval=median_waiting_time) %>%
  rename(nearest_neighbor_sampling_interval=neighbor_waiting_time) %>%
  rename(days_since_last_sample=days_since) %>%
  rename(number_of_unique_days_sampled=Number_of_days_sampled) %>%
  rename(distance_to_nearest_sampled_grid_km=dist_km_nn) %>%
  rename(sampled_or_not=sampled) %>%
  dplyr::select(1:3, 5:6, 8:9)

view(dfSummary(grid_25))

view(dfSummary(grid_25, plain.ascii = FALSE, style = "grid", 
               graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))


grid_10 <- params %>%
  dplyr::filter(grid_size==10) %>%
  rename(median_sampling_interval=median_waiting_time) %>%
  rename(nearest_neighbor_sampling_interval=neighbor_waiting_time) %>%
  rename(days_since_last_sample=days_since) %>%
  rename(number_of_unique_days_sampled=Number_of_days_sampled) %>%
  rename(distance_to_nearest_sampled_grid_km=dist_km_nn) %>%
  rename(sampled_or_not=sampled) %>%
  dplyr::select(1:3, 5:6, 8:9)

view(dfSummary(grid_10))

view(dfSummary(grid_10, plain.ascii = FALSE, style = "grid", 
               graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))

grid_5 <- params %>%
  dplyr::filter(grid_size==5) %>%
  rename(median_sampling_interval=median_waiting_time) %>%
  rename(nearest_neighbor_sampling_interval=neighbor_waiting_time) %>%
  rename(days_since_last_sample=days_since) %>%
  rename(number_of_unique_days_sampled=Number_of_days_sampled) %>%
  rename(distance_to_nearest_sampled_grid_km=dist_km_nn) %>%
  rename(sampled_or_not=sampled) %>%
  dplyr::select(1:3, 5:6, 8:9)

view(dfSummary(grid_5))

view(dfSummary(grid_5, plain.ascii = FALSE, style = "grid", 
               graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"))




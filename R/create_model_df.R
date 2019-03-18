library(dplyr)
library(BBmisc)
library(tidyr)



# get a dataframe for modelling
setwd("Data/Sampling dates 2018")

file_list <- list.files()


for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- readRDS(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-readRDS(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

setwd('..')
setwd('..')



# read in leverage data
lev <- readRDS("Data/leverage_results.RDS")


lookup <- dataset %>%
  dplyr::select(grid_id, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  distinct()

analysis_data <- dataset %>%
  dplyr::select(-LOCALITY_ID, -SAMPLING_EVENT_IDENTIFIER, -dynamic_date) %>%
  distinct() %>%
  mutate(sampled=gsub("yes", 0, .$sampled)) %>%
  mutate(sampled=gsub("no", 1, .$sampled)) %>%
  mutate(sampled=as.numeric(as.character(.$sampled))) %>%
  mutate(distance_sample=dist_km_nn*sampled) %>%
  mutate(m_w_t=median_waiting_time*duration) %>%
  mutate(m_w_t_n_n=neighbor_waiting_time) %>%
  group_by(OBSERVATION_DATE) %>%
  mutate(norm.distance_sample = normalize(distance_sample, method="range", range=c(0,100))) %>%
  mutate(norm.m_w_t = normalize(m_w_t, method="range", range=c(0,100))) %>%
  mutate(norm.m_w_t_n_n = normalize(m_w_t_n_n, method="range", range=c(0,100))) %>%
  mutate(norm.days_since = normalize(days_since, method="range", range=c(0,100))) %>%
  left_join(., lookup, by=c("grid_id", "OBSERVATION_DATE")) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  left_join(., lev, by=c("SAMPLING_EVENT_IDENTIFIER", "LOCALITY_ID", "OBSERVATION_DATE"))


saveRDS(analysis_data, file="Data/Modelling data/analysis_data.RDS")





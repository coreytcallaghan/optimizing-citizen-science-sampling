library(dplyr)
library(BBmisc)
library(tidyr)



# get a dataframe for modelling 
# for each of the different grid cell sizes

# 50 km first
setwd("Data/Sampling dates 2018/50 km")

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

dataset1 <- dataset %>%
  mutate(grid_size=50)

setwd('..')
setwd('..')
setwd('..')

# 25 km
setwd("Data/Sampling dates 2018/25 km")

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

dataset2 <- dataset %>%
  mutate(grid_size=25)

setwd('..')
setwd('..')
setwd('..')

# 10 km
setwd("Data/Sampling dates 2018/10 km")

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

dataset3 <- dataset %>%
  mutate(grid_size=10)

setwd('..')
setwd('..')
setwd('..')

# 5 km
setwd("Data/Sampling dates 2018/5 km")

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

dataset4 <- dataset %>%
  mutate(grid_size=5)

dataset <- bind_rows(dataset1, dataset2, dataset3, dataset4)

setwd('..')
setwd('..')
setwd('..')

# read in leverage data
saveRDS(dataset, file="Data/Modelling data/parameter_dataset.RDS")





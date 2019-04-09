# An R script which reads in each of the day's sampling
# summary, for each of the different grid cell sizes
# and makes dataframes.
# then combines those dataframes into the 'parameters' df

library(dplyr)
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
  distinct() %>%
  mutate(grid_size=50)

rm(dataset)

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
  distinct() %>%
  mutate(grid_size=25)

rm(dataset)

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
  distinct() %>%
  mutate(grid_size=10)

rm(dataset)

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
  distinct() %>%
  mutate(grid_size=5)

rm(dataset)

setwd('..')
setwd('..')
setwd('..')


# 2 km
setwd("Data/Sampling dates 2018/2 km")

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

dataset5 <- dataset %>%
  distinct() %>%
  mutate(grid_size=2)

# will leave the 2 km grid cell size out for now
# for simplicity as it is a bigger file
params <- bind_rows(dataset1, dataset2, dataset3, dataset4)

setwd('..')
setwd('..')
setwd('..')

# read in leverage data
saveRDS(params, file="Data/Modelling data/parameter_dataset.RDS")





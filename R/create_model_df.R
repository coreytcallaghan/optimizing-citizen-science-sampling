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

dataset <- dataset %>% 
  filter(complete.cases(.))



setwd('..')
setwd('..')



# read in leverage data
lev <- readRDS("Data/leverage_results.RDS")


# final_df 
analysis_data <- dataset %>%
  left_join(., lev, by=c("SAMPLING_EVENT_IDENTIFIER", "LOCALITY_ID", "OBSERVATION_DATE"))






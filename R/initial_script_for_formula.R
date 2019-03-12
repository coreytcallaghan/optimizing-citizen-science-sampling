# An initial R script, attempting to develop a 'formula'
# which can provide value to a given site
# relative to other sites



# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(data.table)
library(geosphere)
library(BBmisc)

# read data in
load("Data/Sampling data/NSW_birds.RData")


# select ten 'locations/sites'
# this is done by just browsing on th eBird 
# website for now
# eventually
# if we get a formula that 'works'
# then we will want to come back to how to define a site
example_sites <- c("L945869", "L2557723", "L8083126", "L1030678", "L915566", 
                   "L1300136", "L5076722", "L2444301", "L3007885", "L5146094")


# get data out for those ten sites
example_data <- NSW_ebird %>%
  dplyr::filter(PROTOCOL_TYPE != "Incidental") %>%
  dplyr::filter(ALL_SPECIES_REPORTED == 1) %>%
  dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
  dplyr::filter(LOCALITY_ID %in% example_sites)


# now need to calculate the following variables for
# each site
# 1.) Whether a site was sampled or not
# 2.) distance from the nearest sampled site (nearest neighbor)
# 3.) The median waiting time between checklists at the site
# 4.) days between first and last observation
# 5.) The median waiting time at the nearest neighbor
# 6.) Days since last sampling event


# 1.) Whether a site was sampled or not
# all sites
all_sites <- NSW_ebird %>%
  dplyr::filter(LOCALITY_ID %in% example_sites) %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() 

# sampled sites
sampled_sites <- example_data %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() %>%
  mutate(sampled = "yes")

sampled_or_not <- all_sites %>%
  left_join(., sampled_sites, by="LOCALITY_ID") %>%
  replace_na(list(sampled="no"))


# 2.) distance from the nearest sampled site
# not sure how well this approach will scale for a large
# number of 'sites'
# but it works for now

# need to call this from the original dataframe because
# in some instances a locality can exist within the original
# data but then drop out when we exclude the lists
# which are incidental 
# and/or not complete!
# this gives the ten example sites
# with unique lat/lngs
unique_points <- NSW_ebird %>%
  dplyr::filter(LOCALITY_ID %in% example_sites) %>%
  dplyr::select(LOCALITY_ID, lat, lng) %>%
  distinct() %>%
  mutate(id=1:nrow(.))

# use RANN::nn2 to find a matrix of nearest neighbors
nearest <- nn2(unique_points[, 2, 3], query=unique_points[, 2, 3])

# turn the ids into a dataframe and select
# only the first two variables (the initial id and the closest neighbor)
nn.ids <- as.data.frame(nearest$nn.idx)[1:2]

# replace 'ids' in the dataframe with
# LOCALITY_ID
nearest_neighbor <- nn.ids
nearest_neighbor[] <- unique_points$LOCALITY_ID[match(unlist(nn.ids), unique_points$id)]

# final df with nearest neighbor in it
nearest_neighbor <- nearest_neighbor %>%  
  rename(LOCALITY_ID = V1) %>%
  rename(NEIGHBOR_ID = V2)

# now calculate the distance between each 
# site and its nearest neighbor
distance_calculation <- nearest_neighbor %>%
  left_join(., unique_points, by="LOCALITY_ID") %>%
  dplyr::select(-id) %>%
  rename(site_lat = lat, site_lng = lng) %>%
  left_join(., rename(unique_points, NEIGHBOR_ID = LOCALITY_ID), by="NEIGHBOR_ID") %>%
  rename(neighbor_lat = lat, neighbor_lng = lng)

setDT(distance_calculation)[ , dist_km := distGeo(matrix(c(site_lng, site_lat), ncol = 2), 
                                matrix(c(neighbor_lng, neighbor_lat), ncol = 2))/1000]

nearest_neighbor <- distance_calculation %>%
  dplyr::select(LOCALITY_ID, NEIGHBOR_ID, dist_km)


# 3.) The median waiting time between checklists at a site
# 4.) days between first and last observation
# Will calculate both of these but probably best to put it
# in a function and lapply over all unique
# locality_ids in the dataframe

get_median_and_duration <- function(LOCALITY) {
  
df <- example_data %>%
    select(LOCALITY_ID, OBSERVATION_DATE) %>%
    distinct()

df <- df %>%
  dplyr::filter(LOCALITY_ID == LOCALITY) %>%
  arrange(OBSERVATION_DATE) %>%
  mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
  mutate(waiting_days = OBSERVATION_DATE - last_sample_date) 

df[df == 0] <- NA

locality_median <- data.frame(LOCALITY_ID = LOCALITY,
                              median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                              duration = 
                                df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)] - df$OBSERVATION_DATE[1]) %>%
  mutate(duration=as.numeric(.$duration)) %>%
  mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 

return(locality_median)

}

# now apply the function over the list of
# locality IDs in the df
# but note that we will remove any localities
# that have only 1 checklist
# so will have to bring those back after
# use the "sampled_sites" df above
N <- example_data %>%
  dplyr::select(LOCALITY_ID) %>%
  group_by(LOCALITY_ID) %>%
  summarise(N=n())

filtered_sites <- example_data %>%
  dplyr::select(LOCALITY_ID) %>%
  group_by(LOCALITY_ID) %>%
  summarise(N=n()) %>%
  dplyr::filter(N>1)

sites <- filtered_sites$LOCALITY_ID

# use lapply to run the above function
list_of_results <- lapply(sites, function(x) {get_median_and_duration(x)})

# turn into a df
median_and_duration <- bind_rows(list_of_results) %>%
  right_join(., N, by="LOCALITY_ID")

# 6.) Days since last sampling event
# this one is kinda weird because it will depend on the date
# for which everything is calculated
# so here I'll just pick a date in January
# which makes it more practical, given that the observations
# only go up until end of January
today_date <- as.Date(c("2019-01-22"), format="%Y-%m-%d")

days_since_last_sample <- example_data %>%
  select(LOCALITY_ID, OBSERVATION_DATE) %>%
  distinct() %>%
  group_by(LOCALITY_ID) %>%
  arrange(desc(OBSERVATION_DATE)) %>%
  slice(1) %>%
  mutate(days_since = today_date-OBSERVATION_DATE) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(.$days_since)) %>%
  dplyr::select(LOCALITY_ID, days_since)


#####################################
#####################################
# combine this all into one dataframe
summary_df <- sampled_or_not %>%
  left_join(., nearest_neighbor, by="LOCALITY_ID") %>%
  left_join(., median_and_duration, by="LOCALITY_ID") %>%
  left_join(., days_since_last_sample, by="LOCALITY_ID")

# add neighbor_median_waiting_time
neighbor_waiting <- summary_df %>%
  dplyr::select(LOCALITY_ID, median_waiting_time) %>%
  rename(NEIGHBOR_ID = LOCALITY_ID) %>%
  rename(neighbor_waiting_time = median_waiting_time)

summary_df <- summary_df %>%
  left_join(., neighbor_waiting, by="NEIGHBOR_ID")


#########################################################################
#########################################################################
# make a big function which returns a summary_df for a set number of sites
# it relies on a list of sites for which to create a summary df
# will just copy and paste the code above necessary to do this
# also add a 'date' portion
# which will allow the 'results' to be calculated for any particular date
#########################################################################
#########################################################################
return_summary_data <- function(list, date="2019-01-22") {
  
  date=as_date(date)
  
  example_data <- NSW_ebird %>%
    dplyr::filter(PROTOCOL_TYPE != "Incidental") %>%
    dplyr::filter(ALL_SPECIES_REPORTED == 1) %>%
    dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
    dplyr::filter(OBSERVATION_DATE <= date) %>%
    dplyr::filter(LOCALITY_ID %in% list)

  all_sites <- NSW_ebird %>%
    dplyr::filter(LOCALITY_ID %in% example_sites) %>%
    dplyr::select(LOCALITY_ID) %>%
    distinct() 

  sampled_sites <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    distinct() %>%
    mutate(sampled = "yes")
  
  sampled_or_not <- all_sites %>%
    left_join(., sampled_sites, by="LOCALITY_ID") %>%
    replace_na(list(sampled="no"))

  unique_points <- NSW_ebird %>%
    dplyr::filter(LOCALITY_ID %in% example_sites) %>%
    dplyr::select(LOCALITY_ID, lat, lng) %>%
    distinct() %>%
    mutate(id=1:nrow(.))

  nearest <- nn2(unique_points[, 2, 3], query=unique_points[, 2, 3])

  nn.ids <- as.data.frame(nearest$nn.idx)[1:2]

  nearest_neighbor <- nn.ids
  nearest_neighbor[] <- unique_points$LOCALITY_ID[match(unlist(nn.ids), unique_points$id)]
  
  nearest_neighbor <- nearest_neighbor %>%  
    rename(LOCALITY_ID = V1) %>%
    rename(NEIGHBOR_ID = V2)
  
  distance_calculation <- nearest_neighbor %>%
    left_join(., unique_points, by="LOCALITY_ID") %>%
    dplyr::select(-id) %>%
    rename(site_lat = lat, site_lng = lng) %>%
    left_join(., rename(unique_points, NEIGHBOR_ID = LOCALITY_ID), by="NEIGHBOR_ID") %>%
    rename(neighbor_lat = lat, neighbor_lng = lng)
  
  setDT(distance_calculation)[ , dist_km := distGeo(matrix(c(site_lng, site_lat), ncol = 2), 
                                                    matrix(c(neighbor_lng, neighbor_lat), ncol = 2))/1000]
  
  nearest_neighbor <- distance_calculation %>%
    dplyr::select(LOCALITY_ID, NEIGHBOR_ID, dist_km)
  
  get_median_and_duration <- function(LOCALITY) {
    
    df <- example_data %>%
      select(LOCALITY_ID, OBSERVATION_DATE) %>%
      distinct()
    
    df <- df %>%
      dplyr::filter(LOCALITY_ID == LOCALITY) %>%
      arrange(OBSERVATION_DATE) %>%
      mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
      mutate(waiting_days = OBSERVATION_DATE - last_sample_date) 
    
    df[df == 0] <- NA
    
    locality_median <- data.frame(LOCALITY_ID = LOCALITY,
                                  median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                                  duration = 
                                    df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)] - df$OBSERVATION_DATE[1]) %>%
      mutate(duration=as.numeric(.$duration)) %>%
      mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 
    
    return(locality_median)
    
  }
  
  N <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    group_by(LOCALITY_ID) %>%
    summarise(N=n())
  
  filtered_sites <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    group_by(LOCALITY_ID) %>%
    summarise(N=n()) %>%
    dplyr::filter(N>1)
  
  sites <- filtered_sites$LOCALITY_ID
  
  list_of_results <- lapply(sites, function(x) {get_median_and_duration(x)})
  
  median_and_duration <- bind_rows(list_of_results) %>%
    right_join(., N, by="LOCALITY_ID")
  
  
  days_since_last_sample <- example_data %>%
    select(LOCALITY_ID, OBSERVATION_DATE) %>%
    distinct() %>%
    group_by(LOCALITY_ID) %>%
    arrange(desc(OBSERVATION_DATE)) %>%
    slice(1) %>%
    mutate(days_since = date-OBSERVATION_DATE) %>%
    ungroup() %>%
    mutate(days_since = as.numeric(.$days_since)) %>%
    dplyr::select(LOCALITY_ID, days_since)

  summary_df <- sampled_or_not %>%
    left_join(., nearest_neighbor, by="LOCALITY_ID") %>%
    left_join(., median_and_duration, by="LOCALITY_ID") %>%
    left_join(., days_since_last_sample, by="LOCALITY_ID")
  
  neighbor_waiting <- summary_df %>%
    dplyr::select(LOCALITY_ID, median_waiting_time) %>%
    rename(NEIGHBOR_ID = LOCALITY_ID) %>%
    rename(neighbor_waiting_time = median_waiting_time)
  
  summary_df <- summary_df %>%
    left_join(., neighbor_waiting, by="NEIGHBOR_ID")
  
  return(summary_df)
}

# test it
list <- c("L945869", "L2557723", "L8083126", "L1030678", "L915566", 
          "L1300136", "L5076722", "L2444301", "L3007885", "L5146094")

site_summary <- return_summary_data(list)



######################################################################
######################################################################
# Now need to develop a formula with which to calculat the 'value' 
# of a given observation
######################################################################
######################################################################
site_summary <- site_summary %>%
  replace_na(list(median_waiting_time = max(.$median_waiting_time, na.rm=TRUE)+(2*sd(.$median_waiting_time, na.rm=TRUE)))) %>%
  replace_na(list(N=0)) %>%
  replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(2*sd(.$days_since, na.rm=TRUE)))) %>%
  replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(2*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
  replace_na(list(duration=max(.$duration, na.rm=TRUE)+(2*sd(.$duration, na.rm=TRUE))))

# make yes/no a multiplier
# 0 for yes and 1 for no
# this means that this variable will 
# drop out if all sites have been sampled
# and means that unsampled sites will be
# more valuable
site_summary <- site_summary %>%
  mutate(sampled=gsub("yes", 0, .$sampled)) %>%
  mutate(sampled=gsub("no", 1, .$sampled)) %>%
  mutate(sampled=as.numeric(as.character(.$sampled)))

# make variables to go into "formula"
formula_df <- data.frame(LOCALITY_ID = site_summary$LOCALITY_ID,
                         distance_sample = site_summary$dist_km*site_summary$sampled,
                         m_w_t = site_summary$median_waiting_time*site_summary$duration,
                         m_w_t_n_n = site_summary$neighbor_waiting_time,
                         days_since = site_summary$days_since)
             
# standardize variables according to the 'scaling' factor
formula_df2 <- formula_df %>%
  mutate(norm.distance_sample = normalize(.$distance_sample, method="range", range=c(0, 25))) %>%
  mutate(norm.m_w_t = normalize(.$m_w_t, method="range", range=c(0,25))) %>%
  mutate(norm.m_w_t_n_n = normalize(.$m_w_t_n_n, method="range", range=c(0,10))) %>%
  mutate(norm.days_since = normalize(.$days_since, method="range", range=c(0,20)))


# value for each locality_id
value <- data.frame(LOCALITY_ID = formula_df2$LOCALITY_ID,
                    value = formula_df2$norm.days_since +
                            formula_df2$norm.distance_sample +
                            formula_df2$norm.m_w_t +
                            formula_df2$norm.m_w_t_n_n)


#######################################################################
#######################################################################
############# let's turn the above into a function
############# so that when passed a df of summarized variables
############# it will return a dataframe of the "value"
############# of each site relative to one another
#######################################################################
#######################################################################

calculate_value <- function(df, distance, median_waiting_time, 
                            median_waiting_time_nearest_neighbor,
                            days_since_last_sample) {
  
  
  site_summary <- df %>%
    replace_na(list(median_waiting_time = max(.$median_waiting_time, na.rm=TRUE)+(2*sd(.$median_waiting_time, na.rm=TRUE)))) %>%
    replace_na(list(N=0)) %>%
    replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(2*sd(.$days_since, na.rm=TRUE)))) %>%
    replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(2*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
    replace_na(list(duration=max(.$duration, na.rm=TRUE)+(2*sd(.$duration, na.rm=TRUE)))) %>%
    mutate(sampled=gsub("yes", 0, .$sampled)) %>%
    mutate(sampled=gsub("no", 1, .$sampled)) %>%
    mutate(sampled=as.numeric(as.character(.$sampled)))
  
  # make variables to go into "formula"
  formula_df <- data.frame(LOCALITY_ID = site_summary$LOCALITY_ID,
                           distance_sample = site_summary$dist_km*site_summary$sampled,
                           m_w_t = site_summary$median_waiting_time*site_summary$duration,
                           m_w_t_n_n = site_summary$neighbor_waiting_time,
                           days_since = site_summary$days_since)
  
  # standardize variables according to the 'scaling' factor
  formula_df_norm <- formula_df %>%
    mutate(norm.distance_sample = normalize(.$distance_sample, method="range", range=c(0, (distance*100)))) %>%
    mutate(norm.m_w_t = normalize(.$m_w_t, method="range", range=c(0, (median_waiting_time*100)))) %>%
    mutate(norm.m_w_t_n_n = normalize(.$m_w_t_n_n, method="range", range=c(0, median_waiting_time_nearest_neighbor*100))) %>%
    mutate(norm.days_since = normalize(.$days_since, method="range", range=c(0, days_since_last_sample*100)))
  
  
  # value for each locality_id
  value <- data.frame(LOCALITY_ID = formula_df_norm$LOCALITY_ID,
                      value = formula_df_norm$norm.days_since +
                        formula_df_norm$norm.distance_sample +
                        formula_df_norm$norm.m_w_t +
                        formula_df_norm$norm.m_w_t_n_n)
  
  return(value)
}


######################################################################
######################################################################
######################################################################
# Now lets play with the function.



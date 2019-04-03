### Create a dataframe for which modelling against the formula can be done
### This means that for everyday throughout 2018 I will calculate what grids have
### been sampled and the various aspects that might be useful in
### calculating where to go sample next
### this dataframe will then be put together with the leverage of every checklist
### to see how well people are sampling
### will do this for every day in 2018
### so that the function is passed a date and then lapply over a list of dates


# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(data.table)
library(geosphere)
library(BBmisc)
library(RANN)
library(nngeo)


# load data
load("Data/Greater_Sydney_data.RData")

load("Data/Sampling data/NSW_birds.RData")

load("Data/Spatial data/sydney_grids_1_km.RData")
load("Data/Spatial data/sydney_grids_2_km.RData")
load("Data/Spatial data/sydney_grids_5_km.RData")
load("Data/Spatial data/sydney_grids_10_km.RData")
load("Data/Spatial data/sydney_grids_25_km.RData")
load("Data/Spatial data/sydney_grids_50_km.RData")



# assign each locality (and thereby sampling event identifier) 
# a grid cell throughout Sydney
# will be necessary for merging back later
# because the calculations will be on a grid scale
# get list of unique sites (LOCALITY_ID)
sites <- GS_observations %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  distinct()


# convert points into sf points
# so that they can be used with the polygons
crs <- 4326

sites_sf <- st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), 
                      crs = crs, agr = "constant")

# use intersect to return a list of points and the associated
# polygon they belong to - uses integer ids for rows and columns
# note that it looks like 29 points aren't assigned to a LGA
# not worried about this for now, but something to come back to in the future
assigned_points <- as.data.frame(st_intersects(sites_sf, sydney_grids_10_km))

# join the original dataframe with the intersected df
# then remove the row and col ids
# provides a lookup table
# joining locality_id and grid_id
sites_and_grids <- sites %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  rename(grid_id = col.id) %>%
  dplyr::select(-row.id)

# get a dataframe of unique grids
unique_grids <- sydney_grids_10_km %>%
  st_set_geometry(NULL) %>%
  mutate(id=1:nrow(.)) %>%
  mutate(id = as.character(as.integer(.$id)))

date_specific_summary <- function(forecast_date, dataset, grid_size) {

  
# use intersect to return a list of points and the associated
# polygon they belong to - uses integer ids for rows and columns
assigned_points <- as.data.frame(st_intersects(sites_sf, dataset))

# join the original dataframe with the intersected df
# then remove the row and col ids
# provides a lookup table
# joining locality_id and grid_id
sites_and_grids <- sites %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  rename(grid_id = col.id) %>%
  dplyr::select(-row.id)
  
# get a dataframe of unique grids
unique_grids <- dataset %>%
  st_set_geometry(NULL) %>%
  mutate(id=1:nrow(.)) %>%
  mutate(id = as.character(as.integer(.$id)))  

  
  
# ensure the date is being treated as a date object
# necessary for filtering later on
dynamic_date <- as.Date(forecast_date)

# list of all potential grids in the dataset
# all potential sites
grids <- data.frame(grid_id=dataset$grid_id)

# filter to all data at a date
# this cuts the entire dataset off to whatever
# date (forecast date) the function is passed
all_data_sampled <- GS_observations %>%
  dplyr::filter(OBSERVATION_DATE < dynamic_date)

# filter to all grids that have been sampled
# at the date of the function
grids_sampled <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() %>%
  left_join(., sites_and_grids) %>%
  distinct(grid_id) %>%
  mutate(sampled = "yes")

# a list of all grids and whether they have been sampled up until that date
sampled_or_not <- grids %>%
  left_join(., grids_sampled, by="grid_id") %>%
  replace_na(list(sampled="no"))
  

# distance from the nearest sampled site
# first find the nearest neighbor
# based on each grid's centroid
# do the procedure above but for only sampled centroids
# so this takes all grids (by their centroids)
# and only does a nearest neighbor to find the nearest sampled grid
# eventually this would turn into all grids being sampled and
# all grids would have the same nearest distance to a sampled grid
sampled_grids <- grids_sampled$grid_id

# convert grid centroids to a sf object
grids_sampled_sf <- unique_grids %>%
  st_as_sf(coords=c("lon", "lat"), crs=st_crs(dataset))

# subsample the gridded map of sydney
# to those grids which have been sampled
dataset.2 <- dataset %>%
  dplyr::filter(grid_id %in% sampled_grids)

# use the st_nn function from nngeo
# to find the nearest neighbor for a sampled grid
nearest_df_sampled <- as.data.frame(t(as.data.frame(st_nn(grids_sampled_sf, dataset.2, k=2))))

# id lookup
id_lookup <- dataset.2 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(grid_id) %>%
  mutate(id_lookup=1:nrow(.)) %>%
  mutate(id_lookup=as.character(as.integer(.$id_lookup)))

# rather complicated way to create a dataframe!
# probably a much better way!
nearest_df_sampled2 <- nearest_df_sampled %>%
  mutate(id = 1:nrow(.)) %>%
  mutate(id=as.character(as.integer(.$id))) %>%
  rename(id_lookup = V1) %>%
  mutate(id_lookup=as.character(as.integer(.$id_lookup))) %>%
  left_join(., id_lookup, by="id_lookup") %>%
  rename(n1=grid_id) %>%
  dplyr::select(-id_lookup) %>%
  rename(id_lookup=V2) %>%
  mutate(id_lookup=as.character(as.integer(.$id_lookup))) %>%
  left_join(., id_lookup, by="id_lookup") %>%
  rename(n2=grid_id) %>%
  rename(grid_id=id) %>%
  dplyr::select(-id_lookup) %>%
  dplyr::select(-n1) %>%
  rename(nn=n2) %>%
  mutate(grid_id = as.integer(as.character(.$grid_id))) %>%
  left_join(., unique_grids, by="grid_id") %>%
  rename(grid_lat=lat) %>%
  rename(grid_lon=lon) %>%
  dplyr::select(-id) %>%
  left_join(., rename(unique_grids, nn=grid_id), by="nn") %>%
  rename(nn_lat=lat) %>%
  rename(nn_lon=lon) %>%
  dplyr::select(-id)

# calculate the distance between each grid and its nearest sampled neighbor
setDT(nearest_df_sampled2)[ , dist_km_nn := distGeo(matrix(c(grid_lon, grid_lat), ncol = 2), 
                                                     matrix(c(nn_lon, nn_lat), ncol = 2))/1000]

###########################################################################
###########################################################################

# 3.) The median waiting time between checklists at a site
# 4.) days between first and last observation
# Will calculate both of these but probably best to put it
# in a function and lapply over all unique
# locality_ids in the dataframe

get_median_and_duration <- function(grid) {
  
  # get all data from dataframe
  # and then build a dataframe
  # that calculation will be built upon
  df <- all_data_sampled %>%
    dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
    distinct() %>%
    left_join(., sites_and_grids, by="LOCALITY_ID") %>%
    dplyr::filter(grid_id == grid) %>%
    distinct(OBSERVATION_DATE, .keep_all = TRUE) %>%
    arrange(OBSERVATION_DATE) %>%
    mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
    mutate(waiting_days = OBSERVATION_DATE - last_sample_date) %>%
    mutate(waiting_days = as.numeric(.$waiting_days))
  
  # turn first waiting days
  # to 1 (cause it will always be zero)
  df$waiting_days[1] <- NA
  # force all '0's to 0.1 as we want the median waiting time to
  # be a positive number (I think)
  #df[df == 0] <- 0.1
  
  # create dataframe
  grid_median <- data.frame(grid_id = grid,
                                median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                                duration = (df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)]) - (df$OBSERVATION_DATE[1])) %>%
    mutate(duration=as.numeric(.$duration)) %>%
    mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 
  
  return(grid_median)
  
}

# now apply the function over the list of
# grid ids in the df
# but note that we will remove any localities
# that have only 1 checklist
# so will have to bring those back after
# use the "sampled_sites" df above
N <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
  distinct() %>%
  left_join(., sites_and_grids, by="LOCALITY_ID") %>%
  group_by(grid_id) %>%
  distinct(OBSERVATION_DATE, .keep_all=TRUE) %>%
  summarise(N=length(unique(OBSERVATION_DATE))) %>%
  mutate(grid_id = as.character(as.integer(.$grid_id)))

filtered_grids <- N %>%
  dplyr::filter(N>1)

grids_list <- filtered_grids$grid_id

# use lapply to run the above function
list_of_results <- lapply(grids_list, function(x) {get_median_and_duration(x)})

# turn into a df
median_and_duration <- bind_rows(list_of_results) %>%
  right_join(., N, by="grid_id")

# 6.) Days since last sampling event
days_since_last_sample <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
  distinct() %>%
  left_join(., sites_and_grids, by="LOCALITY_ID") %>%
  distinct(OBSERVATION_DATE, .keep_all=TRUE) %>%
  group_by(grid_id) %>%
  arrange(desc(OBSERVATION_DATE)) %>%
  slice(1) %>%
  mutate(days_since = dynamic_date-OBSERVATION_DATE) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(.$days_since)) %>%
  dplyr::select(grid_id, days_since)


##################################################
#############################################
#################################################
######### Join them together to a summary df
##############################################

summary_df <- sampled_or_not %>%
  mutate(grid_id = as.character(as.integer(.$grid_id))) %>%
  left_join(., median_and_duration, by="grid_id") %>%
  mutate(grid_id = as.integer(as.character(.$grid_id))) %>%
  left_join(., days_since_last_sample, by="grid_id") %>%
  left_join(., nearest_df_sampled2, by="grid_id") %>%
  dplyr::select(-grid_lon, -grid_lat, -nn_lon, -nn_lat) %>%
  rename(Number_of_days_sampled = N)
  

# add neighbor_median_waiting_time
neighbor_waiting <- summary_df %>%
  dplyr::select(grid_id, median_waiting_time) %>%
  rename(nn = grid_id) %>%
  rename(neighbor_waiting_time = median_waiting_time)

# combine back together
# and normalize every grid cell
# for those that are NA I
# make them a constant (1 sd above the mean)
summary_df <- summary_df %>%
  left_join(., neighbor_waiting, by="nn") %>%
  mutate(dynamic_date = dynamic_date) #%>%
  #replace_na(list(median_waiting_time=max(.$median_waiting_time, na.rm=TRUE)+(1*sd(.$median_waiting_time, na.rm=TRUE))))%>%
  #replace_na(list(N=0)) %>%
  #replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(1*sd(.$days_since, na.rm=TRUE)))) %>%
  #replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(1*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
  #replace_na(list(duration=max(.$duration, na.rm=TRUE)+(1*sd(.$duration, na.rm=TRUE))))

# now select all eBird checklists which 
# were submitted on the day of forecast date
# because these are the checklists which would
# be regressed against the possible grid cells in the matrix
# sampling on that day that took place
sampling <- GS_observations %>%
  dplyr::filter(OBSERVATION_DATE <= dynamic_date) %>%
  dplyr::select(LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  left_join(., sites_and_grids, by="LOCALITY_ID") %>%
  dplyr::select(grid_id, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  #mutate(OBSERVATION_DATE = as.character(as.Date(.$OBSERVATION_DATE))) %>%
  distinct() %>%
  mutate(year=year(OBSERVATION_DATE)) %>%
  mutate(month=month(OBSERVATION_DATE)) %>%
  mutate(day=day(OBSERVATION_DATE)) %>%
  dplyr::filter(year==2018) %>%
  dplyr::filter(month==month(dynamic_date)) %>%
  dplyr::filter(day==day(dynamic_date))

final_df <- summary_df %>%
  left_join(., sampling, by="grid_id") %>%
  dplyr::select(-year, -month, -day)

saveRDS(final_df, file=paste0("Data/Sampling dates 2018/", grid_size, " km/", dynamic_date, ".RDS"))

}

# now run a for loop for every date
# and this should save the results out for every day
# date sequence

dates_2018 <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by=1)


lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_50_km, 50)})
lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_25_km, 25)})
lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_10_km, 10)})
lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_5_km, 5)})
lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_2_km, 2)})
lapply(dates_2018, function(x) {date_specific_summary(x, sydney_grids_1_km, 1)})



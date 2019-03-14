### Create a dataframe for which modelling against the formula


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

load("Data/Spatial data/sydney_grids.RData")



# assign each locality (and thereby sampling event identifier) 
# a grid cell throughout Sydney
sydney_grids <- sydney_grids %>%
  rename(grid_id = id)

# get list of unique sites
sites <- GS_observations %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  distinct()


# convert points from FrogID into sf points
# so that they can be used with the polygons
crs <- "+proj=longlat +ellps=GRS80 +no_defs"

sites_sf <- st_as_sf(sites, coords = c("LONGITUDE", "LATITUDE"), 
                      crs = crs, agr = "constant")

# use intersect to return a list of points and the associated
# polygon they belong to - uses integer ids for rows and columns
# note that it looks like 29 points aren't assigned to a LGA
# not worried about this for now, but something to come back to in the future
assigned_points <- as.data.frame(st_intersects(sites_sf, sydney_grids))

# join the original dataframe with the intersected df
# then remove the row and col ids
sites_and_grids <- sites %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  rename(grid_id = col.id) %>%
  dplyr::select(-row.id)


date_specific_summary <- function(forecast_date) {

# do it for a single date first
dynamic_date <- as.Date(forecast_date)


# list of all potential grids in the dataset
# all potential sites
grids <- data.frame(grid_id=sydney_grids$grid_id)

# filter to all data at a date
all_data_sampled <- GS_observations %>%
  dplyr::filter(OBSERVATION_DATE < dynamic_date)

# filter to all sites at a date
grids_sampled <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() %>%
  left_join(., sites_and_grids) %>%
  distinct(grid_id) %>%
  mutate(sampled = "yes")

sampled_or_not <- grids %>%
  left_join(., grids_sampled, by="grid_id") %>%
  replace_na(list(sampled="no"))
  

# distance from the nearest sampled site
# first find the nearest neighbor
# based on each grid's centroid
unique_grids <- sydney_grids %>%
  st_set_geometry(NULL) %>%
  mutate(id=1:nrow(.)) %>%
  mutate(id = as.character(as.integer(.$id)))

grid_points_sf <- st_as_sf(unique_grids, coords=c("lon", "lat"), crs=st_crs(sydney_grids))

nearest_df <- as.data.frame(t(as.data.frame(st_nn(grid_points_sf, sydney_grids, k=9))))

nearest_df2 <- nearest_df %>%
  rename(grid_id = V1) %>%
  rename(n1 = V2) %>%
  rename(n2 = V3) %>%
  rename(n3 = V4) %>%
  rename(n4 = V5) %>%
  rename(n5 = V6) %>%
  rename(n6 = V7) %>%
  rename(n7 = V8) %>%
  rename(n8 = V9)


# assess how many of the nearest neighbors have been sampled
nearest_sampled <- nearest_df2 %>%
  left_join(., rename(sampled_or_not, n1=grid_id), by="n1") %>%
  rename(sampled_n1 = sampled) %>%
  dplyr::select(-n1) %>%
  left_join(., rename(sampled_or_not, n2=grid_id), by="n2") %>%
  rename(sampled_n2 = sampled) %>%
  dplyr::select(-n2) %>%
  left_join(., rename(sampled_or_not, n3=grid_id), by="n3") %>%
  rename(sampled_n3 = sampled) %>%
  dplyr::select(-n3) %>%
  left_join(., rename(sampled_or_not, n4=grid_id), by="n4") %>%
  rename(sampled_n4 = sampled) %>%
  dplyr::select(-n4) %>%
  left_join(., rename(sampled_or_not, n5=grid_id), by="n5") %>%
  rename(sampled_n5 = sampled) %>%
  dplyr::select(-n5) %>%
  left_join(., rename(sampled_or_not, n6=grid_id), by="n6") %>%
  rename(sampled_n6 = sampled) %>%
  dplyr::select(-n6) %>%
  left_join(., rename(sampled_or_not, n7=grid_id), by="n7") %>%
  rename(sampled_n7 = sampled) %>%
  dplyr::select(-n7) %>%
  left_join(., rename(sampled_or_not, n8=grid_id), by="n8") %>%
  rename(sampled_n8 = sampled) %>%
  dplyr::select(-n8)

# redo the procedure above but for only sampled centroids
sampled_grids <- grids_sampled$grid_id

grids_sampled_sf <- unique_grids %>%
  st_as_sf(coords=c("lon", "lat"), crs=st_crs(sydney_grids))

sydney_grids2 <- sydney_grids %>%
  dplyr::filter(grid_id %in% sampled_grids)

nearest_df_sampled <- as.data.frame(t(as.data.frame(st_nn(grids_sampled_sf, sydney_grids2, k=2))))

id_lookup <- sydney_grids2 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(grid_id) %>%
  mutate(id_lookup=1:nrow(.)) %>%
  mutate(id_lookup=as.character(as.integer(.$id_lookup)))


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
  
  df <- all_data_sampled %>%
    dplyr::select(LOCALITY_ID, OBSERVATION_DATE, SAMPLING_EVENT_IDENTIFIER) %>%
    distinct() %>%
    left_join(., sites_and_grids) %>%
    dplyr::filter(grid_id == grid) %>%
    arrange(OBSERVATION_DATE) %>%
    mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
    mutate(waiting_days = OBSERVATION_DATE - last_sample_date) %>%
    mutate(waiting_days = as.numeric(.$waiting_days))
  
  df$waiting_days[1] <- NA
  df[df == 0] <- 0.1
  
  grid_median <- data.frame(grid_id = grid,
                                median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                                duration = (df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)]) - (df$OBSERVATION_DATE[1])) %>%
    mutate(duration=as.numeric(.$duration)) %>%
    mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 
  
  return(grid_median)
  
}

# now apply the function over the list of
# locality IDs in the df
# but note that we will remove any localities
# that have only 1 checklist
# so will have to bring those back after
# use the "sampled_sites" df above
N <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
  distinct() %>%
  left_join(., sites_and_grids) %>%
  group_by(grid_id) %>%
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
  left_join(., sites_and_grids) %>%
  group_by(grid_id) %>%
  arrange(desc(OBSERVATION_DATE)) %>%
  slice(1) %>%
  mutate(days_since = dynamic_date-OBSERVATION_DATE) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(.$days_since)) %>%
  dplyr::select(grid_id, days_since)


# get median sampling for the nearest neighbors for every grid (nearest 8 grids)
#get_median_and_duration_neighbors <- function(grid) {
  
#  neighbors_list <- nearest_df2 %>%
#    dplyr::filter(grid_id == grid) %>%
#    dplyr::select(2:9) %>%
#    t() %>%
#    .[,1]
  
#  df <- all_data_sampled %>%
#    dplyr::select(LOCALITY_ID, OBSERVATION_DATE, SAMPLING_EVENT_IDENTIFIER) %>%
#    distinct() %>%
#    left_join(., sites_and_grids) %>%
#    dplyr::filter(grid_id %in% neighbors_list) %>%
#    arrange(OBSERVATION_DATE) %>%
#    mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
#    mutate(waiting_days = OBSERVATION_DATE - last_sample_date) %>%
#    mutate(waiting_days = as.numeric(.$waiting_days))
  
#  df$waiting_days[1] <- NA
  
#  grid_median_neighbor <- data.frame(grid_id = grid,
#                           median_waiting_time_n = median(df$waiting_days, na.rm=TRUE),
#                            duration_n = (df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)]) - (df$OBSERVATION_DATE[1])) %>%
#   mutate(duration=as.numeric(.$duration)) %>%
#    mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 
  
#  return(grid_median_neighbor)
  
#}

#N <- all_data_sampled %>%
#  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
#  distinct() %>%
#  left_join(., sites_and_grids) %>%
#  group_by(grid_id) %>%
#  summarise(N=length(unique(OBSERVATION_DATE))) %>%
#  mutate(grid_id = as.character(as.integer(.$grid_id)))

#filtered_grids <- N %>%
#  dplyr::filter(N>1)

#grids_list <- filtered_grids$grid_id

# use lapply to run the above function
#list_of_results_n <- lapply(grids_list, function(x) {get_median_and_duration_neighbors(x)})

# turn into a df
#median_and_duration_neighbor <- bind_rows(list_of_results_n) %>%
#  right_join(., N, by="grid_id")




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
  dplyr::select(-grid_lon, -grid_lat, -nn_lon, -nn_lat)
  

# add neighbor_median_waiting_time
neighbor_waiting <- summary_df %>%
  dplyr::select(grid_id, median_waiting_time) %>%
  rename(nn = grid_id) %>%
  rename(neighbor_waiting_time = median_waiting_time)

summary_df <- summary_df %>%
  left_join(., neighbor_waiting, by="nn") %>%
  mutate(dynamic_date = dynamic_date) %>%
  replace_na(list(median_waiting_time=max(.$median_waiting_time, na.rm=TRUE)+(1*sd(.$median_waiting_time, na.rm=TRUE))))%>%
  replace_na(list(N=0)) %>%
  replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(1*sd(.$days_since, na.rm=TRUE)))) %>%
  replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(1*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
  replace_na(list(duration=max(.$duration, na.rm=TRUE)+(1*sd(.$duration, na.rm=TRUE))))


final_df <- summary_df %>%
  left_join(., sites_and_grids, by="grid_id") %>%
  inner_join(., distinct(dplyr::select(all_data_sampled, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER)), by="LOCALITY_ID")

return(final_df)

}



test1 <- date_specific_summary("2018-04-23")



test2 <- date_specific_summary("2018-06-28")

test3 <- date_specific_summary("2018-02-04")


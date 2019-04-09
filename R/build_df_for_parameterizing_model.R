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


test <- sydney_grids %>%
  left_join(., sites_and_grids, by="grid_id") %>%
  left_join(., GS_observations, by="LOCALITY_ID") %>%
  group_by(grid_id) %>%
  summarise(N=length(unique(SAMPLING_EVENT_IDENTIFIER))) 





date_specific_summary <- function(forecast_date) {

# do it for a single date first
dynamic_date <- as.Date(forecast_date)


# list of all sampled sites in the dataset
# all potential sites
sites <- GS_observations %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  distinct()

# filter to all data at a date
all_data_sampled <- GS_observations %>%
  dplyr::filter(OBSERVATION_DATE < dynamic_date)

# filter to all sites at a date
sites_sampled <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() %>%
  mutate(sampled = "yes")

sampled_or_not <- sites %>%
  left_join(., sites_sampled, by="LOCALITY_ID") %>%
  replace_na(list(sampled="no"))
  

# distance from the nearest sampled site
unique_sites <- sites %>%
  mutate(id=1:nrow(.)) %>%
  mutate(id = as.character(as.integer(.$id)))

# use RANN::nn2 to find a matrix of nearest neighbors
nearest <- nn2(unique_sites[, 2, 3], query=unique_sites[, 2, 3])

# turn the ids into a dataframe and select
# only the first two variables (the initial id and the closest neighbor)
nn.ids <- as.data.frame(nearest$nn.idx)[2] %>%
  tibble::rownames_to_column(var="id") %>%
  left_join(., dplyr::select(unique_sites, id, LOCALITY_ID), by="id") %>%
  dplyr::select(-id) %>%
  rename(id = V2) %>%
  mutate(id = as.character(as.integer(.$id))) %>%
  left_join(., dplyr::select(rename(unique_sites, NEIGHBOR_ID=LOCALITY_ID), NEIGHBOR_ID, id), by="id") %>%
  dplyr::select(-id)

# replace 'ids' in the dataframe with
# LOCALITY_ID
#nearest_neighbor <- nn.ids
#nearest_neighbor[] <- unique_sites$LOCALITY_ID[match(unlist(nn.ids), unique_sites$id)]
#nearest_neighbor <- nearest_neighbor %>%  
#  rename(LOCALITY_ID = V1) %>%
#  rename(NEIGHBOR_ID = V2)

distance_calculation <- nn.ids %>%
  left_join(., unique_sites, by="LOCALITY_ID") %>%
  dplyr::select(-id) %>%
  rename(site_lat = LATITUDE, site_lng = LONGITUDE) %>%
  left_join(., rename(unique_sites, NEIGHBOR_ID = LOCALITY_ID), by="NEIGHBOR_ID") %>%
  rename(neighbor_lat = LATITUDE, neighbor_lng = LONGITUDE)

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
  
  df <- all_data_sampled %>%
    dplyr::filter(LOCALITY_ID == LOCALITY) %>%
    dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
    distinct() %>%
    arrange(OBSERVATION_DATE) %>%
    mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
    mutate(waiting_days = OBSERVATION_DATE - last_sample_date) 
  
  df[df == 0] <- NA
  
  locality_median <- data.frame(LOCALITY_ID = LOCALITY,
                                median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                                duration = (df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)]) - (df$OBSERVATION_DATE[1])) %>%
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
N <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
  group_by(LOCALITY_ID) %>%
  summarise(N=length(unique(OBSERVATION_DATE)))

filtered_sites <- N %>%
  dplyr::filter(N>1)

sites_list <- filtered_sites$LOCALITY_ID

# use lapply to run the above function
list_of_results <- lapply(sites_list, function(x) {get_median_and_duration(x)})

# turn into a df
median_and_duration <- bind_rows(list_of_results) %>%
  right_join(., N, by="LOCALITY_ID")

# 6.) Days since last sampling event
days_since_last_sample <- all_data_sampled %>%
  dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
  distinct() %>%
  group_by(LOCALITY_ID) %>%
  arrange(desc(OBSERVATION_DATE)) %>%
  slice(1) %>%
  mutate(days_since = date-OBSERVATION_DATE) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(.$days_since)) %>%
  dplyr::select(LOCALITY_ID, days_since)


summary_df <- sampled_or_not %>%
  left_join(., median_and_duration, by="LOCALITY_ID") %>%
  left_join(., days_since_last_sample, by="LOCALITY_ID") %>%
  left_join(., nearest_neighbor, by="LOCALITY_ID")
  

# add neighbor_median_waiting_time
neighbor_waiting <- summary_df %>%
  dplyr::select(LOCALITY_ID, median_waiting_time) %>%
  rename(NEIGHBOR_ID = LOCALITY_ID) %>%
  rename(neighbor_waiting_time = median_waiting_time)

summary_df <- summary_df %>%
  left_join(., neighbor_waiting, by="NEIGHBOR_ID") %>%
  mutate(dynamic_date = date)


final_df <- summary_df %>%
  inner_join(., distinct(dplyr::select(all_data_sampled, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER)), by="LOCALITY_ID")

return(final_df)

}



test1 <- date_specific_summary("2018-04-23")



test2 <- date_specific_summary("2018-06-28")

test3 <- date_specific_summary("2018-02-04")


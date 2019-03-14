# stuff to save for now
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


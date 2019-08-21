## This is the second part of the 
## script, following on from "part1---how_similar_is_Sydney_grid_distribution.R"
## which gets all data from within 50 km buffer of a point
## and then turns this into a grid with 5 km grids
## and then calculates the number of eBird checklists within a grid
## and exports this dataframe out
## the end goal is to show how Sydney compares with the distribution of other parts of
## Australia
## ideally, we want to show that the distribution for Sydney is similar to that of 
## other cities
## this would imply that the results of our analysis are more generalizable.
## This part reads in data already collected from
## bigquery for different cities
## and then makes a 'grid' using SF for those data

# packages
library(sf)
library(dplyr)

# Australia outline
aus <- st_read("Data/Spatial data/Australia_outline/aust_cd66states.shp") 

st_crs(aus) <- 4326

# city dataframe
city_dataframe <- data.frame(city=c("Sydney", "Brisbane", "Melbourne", "Adelaide",
                                    "Perth", "Canberra", "Dubbo", "Alice Springs",
                                    "Darwin", "Cairns", "Katherine", "Longreach",
                                    "Rockhampton", "Townsville", "Broome"),
                             lat=c(-33.86785, -27.46794, -37.814, -34.92866,
                                   -31.95224, -35.28346, -32.24295, -23.69748,
                                   -12.46113, -16.92304, -14.46517, -23.439493,
                                   -23.38032, -19.26639, -17.95538),
                             lng=c(151.20732, 153.02809, 144.96332, 138.59863,
                                   115.8614, 149.12807, 148.60484, 133.88362,
                                   130.84185, 145.76625, 132.26347, 144.251389,
                                   150.50595, 146.80569, 122.23922))


calculate_grid_checklist_distribution <- function(city_of_interest) {
  
# use Sydney as an example
df <- city_dataframe %>%
  dplyr::filter(city==city_of_interest)

# create 50 km buffer around the city
centroid_sf <- df %>%
  slice(1) %>%
  st_as_sf(., coords=c("lng", "lat"),
           crs=4326, agr="constant") 

buffer <- centroid_sf %>%
  st_transform(32756) %>%
  st_buffer(., dist=50000) %>%
  st_transform(crs=4326)


grids <- buffer %>%
  st_transform(32756) %>%
  st_make_grid(., cellsize=c(5000, 5000),
               crs=32756, what='polygons') %>%
  st_intersection(st_transform(buffer, 32756)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>%
  st_transform(4326)



# read in data
dat <- readRDS(paste0("Data/city_data/", city_of_interest, "-fifty-data.RDS"))

points_sf <- st_as_sf(dat, coords=c("LONGITUDE", "LATITUDE"), 
                      crs=4326, agr = "constant")

assigned_points <- as.data.frame(st_intersects(points_sf, grids))

# join the original dataframe with the intersected df
# then remove the row and col ids
# provides a lookup table
# joining locality_id and grid_id
lists_and_grids <- points_sf %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  rename(grid_id = col.id) %>%
  dplyr::select(-row.id)

st_geometry(lists_and_grids) <- NULL

summary_df <- lists_and_grids %>%
  group_by(grid_id) %>%
  summarise(N=n()) %>%
  mutate(city=city_of_interest)

saveRDS(summary_df, paste0("Data/other_cities_analysis/summary data/", city_of_interest, "-grid_summary.RDS"))

}


cities_list <- city_dataframe$city


lapply(cities_list, function(x) {calculate_grid_checklist_distribution(x)})




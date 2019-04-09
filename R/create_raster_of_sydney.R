# create a raster of the study area

library(sf)
library(dplyr)


# load shapefile
sydney <- st_read("Data/Spatial data/greater_sydney_shape/sydney.shp") %>%
  st_transform(32756)


# function to make a grid over Sydney region
# grid size is defined in meters
get_gridded_region_function <- function(grid_size) {
  
  sydney_grids <- sydney %>%
    st_make_grid(., cellsize=c(grid_size, grid_size),
                 crs=32756, what='polygons') %>%
    st_intersection(sydney) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(id = row_number())
  
  
  centroids <- st_centroid(sydney_grids)
  centroids <- st_transform(centroids, 4326)
  sydney_grids <- st_transform(sydney_grids, 4326)
  
  coords <- do.call(rbind, st_geometry(centroids)) %>%
    as_tibble() %>%
    setNames(c("lon", "lat"))
  
  sydney_grids <- bind_cols(sydney_grids, coords) %>%
    rename(grid_id = id)
  
  return(sydney_grids)
  
}

# 50 km grids
sydney_grids_50_km <- get_gridded_region_function(grid_size=50000)

save(sydney_grids_50_km, file = "Data/Spatial data/sydney_grids_50_km.RData")


# 25 km grids
sydney_grids_25_km <- get_gridded_region_function(grid_size=25000)

save(sydney_grids_25_km, file = "Data/Spatial data/sydney_grids_25_km.RData")


# 10 km grids
sydney_grids_10_km <- get_gridded_region_function(grid_size=10000)

save(sydney_grids_10_km, file = "Data/Spatial data/sydney_grids_10_km.RData")


# 5 km grids
sydney_grids_5_km <- get_gridded_region_function(grid_size=5000)

save(sydney_grids_5_km, file = "Data/Spatial data/sydney_grids_5_km.RData")


# 2 km grids
sydney_grids_2_km <- get_gridded_region_function(grid_size=2000)

save(sydney_grids_2_km, file = "Data/Spatial data/sydney_grids_2_km.RData")


# 1 km grids
sydney_grids_1_km <- get_gridded_region_function(grid_size=1000)

save(sydney_grids_1_km, file = "Data/Spatial data/sydney_grids_1_km.RData")






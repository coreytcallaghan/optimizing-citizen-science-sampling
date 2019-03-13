# create a raster of the study area

library(sf)
library(raster)

# load shapefile
sydney <- st_read("Data/Spatial data/greater_sydney_shape/sydney.shp")


# create raster
r1 <- raster(nrows=50, ncols=50, xmn=149.9719, xmx=151.6305, ymn=-34.33117, ymx=-32.99607)


sydney_raster <- rasterize(sydney, r1)




# here is just a grid which stays as sf object - should work okay?
sydney_grids <- sydney %>%
  st_make_grid(n=c(25, 25)) %>%
  st_intersection(sydney) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>%
  mutate(centroid=st_centroid())


centroids <- st_centroid(sydney_grids)


coords <- do.call(rbind, st_geometry(centroids)) %>%
  as_tibble() %>%
  setNames(c("lon", "lat"))

sydney_grids <- bind_cols(sydney_grids, coords)


save(sydney_grids, file = "Data/Spatial data/sydney_grids.RData")









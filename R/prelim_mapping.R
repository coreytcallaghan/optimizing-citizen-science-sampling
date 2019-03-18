# prelim plotting

library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

# load data
analysis_data <- readRDS("Data/Modelling data/analysis_data.RData")

load("Data/Spatial data/sydney_grids.RData")

lev_results <- readRDS("Data/leverage_results.RDS")

sites_grids_lookup <- readRDS("Data/sites_and_grids_lookup.RDS")


sydney_grids %>%
  st_set_geometry(NULL) %>%
  rename(grid_id = id) %>%
  left_join(., analysis_data, by="grid_id") %>%
  group_by(grid_id) %>%
  summarise(mean=mean(DATE_dfbetas, na.rm=TRUE),
            min=min(DATE_dfbetas, na.rm=TRUE),
            max=max(DATE_dfbetas, na.rm=TRUE),
            quantile_.1=quantile(DATE_dfbetas, 0.1, na.rm=TRUE)) %>%
  gather(., key="summary", value="value", mean, min, max, quantile_.1) %>%
  left_join(., rename(sydney_grids, grid_id=id), by="grid_id") %>%
  ggplot()+
  geom_sf(aes(fill=value))+
  facet_wrap(~summary)+
  theme_classi
  

sydney_grids %>%
  st_set_geometry(NULL) %>%
  rename(grid_id = id) %>%
  left_join(., sites_grids_lookup, by="grid_id") %>%
  left_join(., lev_results, by="LOCALITY_ID") %>%
  mutate(YEAR = year(OBSERVATION_DATE)) %>%
  group_by(grid_id, YEAR) %>%
  summarise(mean=mean(DATE_dfbetas, na.rm=TRUE),
            min=min(DATE_dfbetas, na.rm=TRUE),
            max=max(DATE_dfbetas, na.rm=TRUE),
            quantile_.1=quantile(DATE_dfbetas, 0.1, na.rm=TRUE)) %>%
  gather(., key="summary", value="value", mean, min, max, quantile_.1) %>%
  left_join(., rename(sydney_grids, grid_id=id), by="grid_id") %>%
  ggplot()+
  geom_sf(aes(fill=value))+
  facet_wrap(YEAR~summary)+
  theme_classic()






# An R script to get all eBird data subsetted to the greater sydney region
# also filtered to get rid of 'bad' checklists
# it takes a long time for these filtering of the MariaDB to be done
# so go get a coffee or something (or a beer!!)
# This won't work unless you have access to the MariaDB

library(sf)
library(dbplyr)
library(dplyr, quietly = T, warn.conflicts = F)
library(RMariaDB)
get_con  <- function () {
  dbConnect(RMariaDB::MariaDB(), host = 'KESTREL', dbname='ebird',user = Sys.getenv('userid'), password = Sys.getenv('pwd'))
} 

sites <- tbl(con, "sites")
samples <- tbl(con, "samples")
species <- tbl(con, "species")
checklists <- tbl(con, "checklists")

NSW_species <- sites %>% 
  filter(STATE_PROVINCE == "New South Wales") %>% 
  left_join(., checklists, by="LOCALITY_ID") %>% 
  inner_join(., samples, by="SAMPLING_EVENT_IDENTIFIER") %>% 
  collect(n=Inf)

NSW_sites <- sites %>%
  filter(STATE_PROVINCE == "New South Wales") %>%
  collect(n=Inf)

species_all <- species %>%
  select(TAXONOMIC_ORDER, CATEGORY, COMMON_NAME, SCIENTIFIC_NAME) %>%
  collect(n=Inf)

## assign each observation to a Greater Capital City Statistical Areas (GCCSA)
## This step requires download of shapefile from
## here http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument )
## we then read in the shapefile and assign each point to a particular GCCSA
## this will be used for later visualizations and analyses


# read in the shapefile
aus_gccsa <- st_read("Data/Spatial data/gccsa_2016_aust_shape/GCCSA_2016_AUST.shp")

# turn observations into sf object
# only for unique lists
points_sf <- st_as_sf(NSW_sites, coords = c("LONGITUDE", "LATITUDE"), crs=st_crs(aus_gccsa)) 

# create metadata df
# used to link the dfs back together later
gccsa_metadata <- data.frame(gccsa_name=aus_gccsa[[2]], col.id=1:34)

assigned_points_gccsa <- as.data.frame(st_intersects(points_sf, aus_gccsa))

# join the original dataframe with the intersected df
# then remove the row and col ids
NSW_sites2 <- NSW_sites %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points_gccsa, by="row.id") %>%
  left_join(., gccsa_metadata, by="col.id") %>%
  dplyr::select(-row.id, -col.id)


# get lists from greater sydney only
GS_observations <- NSW_sites2 %>%
  dplyr::filter(gccsa_name == "Greater Sydney") %>%
  dplyr::select(LOCALITY_ID) %>%
  left_join(., NSW_species, by="LOCALITY_ID") %>%
  # filter out any observations before 2010
  dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
  # join with species' names
  left_join(., species_all, by="TAXONOMIC_ORDER")

save(GS_observations, file="Data/Greater_Sydney_data.RData")






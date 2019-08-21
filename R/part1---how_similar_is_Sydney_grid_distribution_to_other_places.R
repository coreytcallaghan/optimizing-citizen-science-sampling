## This is a function
## which gets all unique sites from with 50 km of a point (i.e., a city)
## and then gets all eBird checklists within that 50 km buffer
## and exports this dataframe out
## Then I will use "part2---how_similar_is_Sydney_grid_distributon.R" script
## to show how Sydney compares with the distribution of other parts of
## Australia
## ideally, we want to show that the distribution for Sydney is similar to that of 
## other cities
## this would imply that the results of our analysis are more generalizable.

## packages
library(readr)
library(DBI)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)


# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "ebird",
                      project="ebird-database",
                      billing="ebird-database")

# create ebird table
ebird <- tbl(con, 'ebird_qa')

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


# function to return data from BigQuery

get_data_function <- function(X) {
  

df <- city_dataframe %>%
  slice(X)


fifty_km_site_list <- dbGetQuery(con, 
                                 paste0('SELECT
                                        DISTINCT
                                        LOCALITY_ID
                                        FROM
                                        (SELECT
                                        LOCALITY_ID,
                                        ST_GeogPoint(LONGITUDE,LATITUDE) AS p
                                        FROM
                                        ebird.ebird_qa
                                        WHERE
                                        ST_DWithin(ST_GeogPoint(LONGITUDE, LATITUDE),
                                        ST_GeogPoint(', df$lng, ',', df$lat, '),
                                        50000))')
                                 )

saveRDS(fifty_km_site_list, paste0("Data/other_cities_analysis/", df$city, "-fifty-sites.RDS"))



get_raw_data_function <- function(buffer) {
  
  filter_list <- readRDS(paste0("Data/other_cities_analysis/", df$city, "-", buffer, "-sites.RDS")) %>%
    .$LOCALITY_ID
  
  dat <- ebird %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY_ID, 
                  LATITUDE, LONGITUDE, OBSERVATION_DATE) %>%
    dplyr::filter(LOCALITY_ID %in% filter_list) %>%
    dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
    dplyr::filter(OBSERVATION_DATE <= "2018-12-31") %>%
    collect(n=Inf) %>%
    distinct()
  
  saveRDS(dat, paste0("Data/city_data/", df$city, "-", buffer, "-data.RDS"))
  
}


get_raw_data_function("fifty")


}

list <- seq(1, 15, by=1)

lapply(list, function(x){get_data_function(x)})





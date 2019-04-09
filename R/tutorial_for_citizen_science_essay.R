# An R script to make stuff for a tutorial of sorts on how things 
# are calculated to parameterize the value of a BSE

# packages
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)
library(geosphere)
library(BBmisc)
library(RANN)
library(ggsn)
library(readr)


# load shapefile
sydney <- st_read("Data/Spatial data/greater_sydney_shape/sydney.shp")

# read data in
load("Data/Sampling data/NSW_birds.RData")


# select ten 'locations/sites'
# this is done by just browsing on the eBird website
example_sites <- c("L945869", "L2557723", "L8083126", "L1030678", "L915566", 
                   "L1300136", "L5076722", "L2444301", "L3007885", "L5146094")


# get data out for those ten sites
example_data <- NSW_ebird %>%
  dplyr::filter(PROTOCOL_TYPE != "Incidental") %>%
  dplyr::filter(ALL_SPECIES_REPORTED == 1) %>%
  dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
  dplyr::filter(LOCALITY_ID %in% example_sites)

# Make a plot of those ten locations in space
sites_coords <- NSW_ebird %>%
  dplyr::filter(LOCALITY_ID %in% example_sites) %>%
  dplyr::select(LOCALITY_ID, lat, lng) %>%
  distinct()

ggplot()+
  geom_sf(data=sydney, fill="lightblue")+
  geom_point(data=sites_coords, aes(x=lng, y=lat), color="firebrick3", size=2.3)+
  coord_sf(xlim = c(151.05, 151.3), ylim = c(-33.97, -33.77))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'transparent'))

ggsave(filename = "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/inset.png",
       height=4, width=4, units="in")

ggplot()+
  geom_sf(data=sydney, fill="lightblue")+
  geom_point(data=sites_coords, aes(x=lng, y=lat), color="firebrick3", size=2.3)+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'transparent'))+
  blank()+
  north(sydney)+
  scalebar(sydney, dist=4, dist_unit="km", transform=TRUE, model="WGS84")

ggsave(filename = "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/big_sydney.png",
       height=7, width=9, units="in")


#########################################################################
#########################################################################
# make a big function which returns a summary_df for a set number of sites
# it relies on a list of sites for which to create a summary df
# will just copy and paste the code above necessary to do this
# also add a 'date' portion
# which will allow the 'results' to be calculated for any particular date
#########################################################################
#########################################################################
return_summary_data <- function(list, date="2018-11-22") {
  
  date=as_date(date)
  
  example_data <- NSW_ebird %>%
    dplyr::filter(PROTOCOL_TYPE != "Incidental") %>%
    dplyr::filter(ALL_SPECIES_REPORTED == 1) %>%
    dplyr::filter(OBSERVATION_DATE >= "2010-01-01") %>%
    dplyr::filter(OBSERVATION_DATE <= date) %>%
    dplyr::filter(LOCALITY_ID %in% list)
  
  all_sites <- NSW_ebird %>%
    dplyr::filter(LOCALITY_ID %in% example_sites) %>%
    dplyr::select(LOCALITY_ID) %>%
    distinct() 
  
  sampled_sites <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    distinct() %>%
    mutate(sampled = "yes")
  
  sampled_or_not <- all_sites %>%
    left_join(., sampled_sites, by="LOCALITY_ID") %>%
    replace_na(list(sampled="no"))
  
  unique_points <- NSW_ebird %>%
    dplyr::filter(LOCALITY_ID %in% example_sites) %>%
    dplyr::select(LOCALITY_ID, lat, lng) %>%
    distinct() %>%
    mutate(id=1:nrow(.))
  
  nearest <- nn2(unique_points[, 2, 3], query=unique_points[, 2, 3])
  
  nn.ids <- as.data.frame(nearest$nn.idx)[1:2]
  
  nearest_neighbor <- nn.ids
  nearest_neighbor[] <- unique_points$LOCALITY_ID[match(unlist(nn.ids), unique_points$id)]
  
  nearest_neighbor <- nearest_neighbor %>%  
    rename(LOCALITY_ID = V1) %>%
    rename(NEIGHBOR_ID = V2)
  
  distance_calculation <- nearest_neighbor %>%
    left_join(., unique_points, by="LOCALITY_ID") %>%
    dplyr::select(-id) %>%
    rename(site_lat = lat, site_lng = lng) %>%
    left_join(., rename(unique_points, NEIGHBOR_ID = LOCALITY_ID), by="NEIGHBOR_ID") %>%
    rename(neighbor_lat = lat, neighbor_lng = lng)
  
  setDT(distance_calculation)[ , dist_km := distGeo(matrix(c(site_lng, site_lat), ncol = 2), 
                                                    matrix(c(neighbor_lng, neighbor_lat), ncol = 2))/1000]
  
  nearest_neighbor <- distance_calculation %>%
    dplyr::select(LOCALITY_ID, NEIGHBOR_ID, dist_km)
  
  get_median_and_duration <- function(LOCALITY) {
    
    df <- example_data %>%
      dplyr::select(LOCALITY_ID, OBSERVATION_DATE) %>%
      distinct()
    
    df <- df %>%
      dplyr::filter(LOCALITY_ID == LOCALITY) %>%
      arrange(OBSERVATION_DATE) %>%
      mutate(last_sample_date = c(.$OBSERVATION_DATE[1], .$OBSERVATION_DATE[1:(length(.$OBSERVATION_DATE)-1)])) %>%
      mutate(waiting_days = OBSERVATION_DATE - last_sample_date) 
    
    df[df == 0] <- NA
    
    locality_median <- data.frame(LOCALITY_ID = LOCALITY,
                                  median_waiting_time = median(df$waiting_days, na.rm=TRUE),
                                  duration = 
                                    df$OBSERVATION_DATE[length(df$OBSERVATION_DATE)] - df$OBSERVATION_DATE[1]) %>%
      mutate(duration=as.numeric(.$duration)) %>%
      mutate(median_waiting_time=as.numeric(.$median_waiting_time)) 
    
    return(locality_median)
    
  }
  
  N <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    group_by(LOCALITY_ID) %>%
    summarise(N=n())
  
  filtered_sites <- example_data %>%
    dplyr::select(LOCALITY_ID) %>%
    group_by(LOCALITY_ID) %>%
    summarise(N=n()) %>%
    dplyr::filter(N>1)
  
  sites <- filtered_sites$LOCALITY_ID
  
  list_of_results <- lapply(sites, function(x) {get_median_and_duration(x)})
  
  median_and_duration <- bind_rows(list_of_results) %>%
    right_join(., N, by="LOCALITY_ID")
  
  
  days_since_last_sample <- example_data %>%
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
    left_join(., nearest_neighbor, by="LOCALITY_ID") %>%
    left_join(., median_and_duration, by="LOCALITY_ID") %>%
    left_join(., days_since_last_sample, by="LOCALITY_ID")
  
  neighbor_waiting <- summary_df %>%
    dplyr::select(LOCALITY_ID, median_waiting_time) %>%
    rename(NEIGHBOR_ID = LOCALITY_ID) %>%
    rename(neighbor_waiting_time = median_waiting_time)
  
  summary_df <- summary_df %>%
    left_join(., neighbor_waiting, by="NEIGHBOR_ID")
  
  return(summary_df)
}

# test it
list <- c("L945869", "L2557723", "L8083126", "L1030678", "L915566", 
          "L1300136", "L5076722", "L2444301", "L3007885", "L5146094")

site_summary <- return_summary_data(list)

write_csv(site_summary, "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/site_summary.csv")

######################################################################
######################################################################
# Now need to develop a formula with which to calculat the 'value' 
# of a given observation
######################################################################
######################################################################
site_summary <- site_summary %>%
  replace_na(list(median_waiting_time = max(.$median_waiting_time, na.rm=TRUE)+(1*sd(.$median_waiting_time, na.rm=TRUE)))) %>%
  replace_na(list(N=0)) %>%
  replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(1*sd(.$days_since, na.rm=TRUE)))) %>%
  replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(1*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
  replace_na(list(duration=max(.$duration, na.rm=TRUE)+(1*sd(.$duration, na.rm=TRUE))))

write_csv(site_summary, "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/site_summary2.csv")


# make yes/no a multiplier
# 0 for yes and 1 for no
# this means that this variable will 
# drop out if all sites have been sampled
# and means that unsampled sites will be
# more valuable
site_summary <- site_summary %>%
  mutate(sampled=gsub("yes", 0, .$sampled)) %>%
  mutate(sampled=gsub("no", 1, .$sampled)) %>%
  mutate(sampled=as.numeric(as.character(.$sampled)))

# make variables to go into "formula"
formula_df <- data.frame(LOCALITY_ID = site_summary$LOCALITY_ID,
                         distance_sample = site_summary$dist_km*site_summary$sampled,
                         m_w_t = site_summary$median_waiting_time*site_summary$duration,
                         m_w_t_n_n = site_summary$neighbor_waiting_time,
                         days_since = site_summary$days_since)

# standardize variables according to the 'scaling' factor
formula_df2 <- formula_df %>%
  mutate(norm.distance_sample = normalize(.$distance_sample, method="range", range=c(0, 25))) %>%
  mutate(norm.m_w_t = normalize(.$m_w_t, method="range", range=c(0,25))) %>%
  mutate(norm.m_w_t_n_n = normalize(.$m_w_t_n_n, method="range", range=c(0,25))) %>%
  mutate(norm.days_since = normalize(.$days_since, method="range", range=c(0,25)))

write_csv(formula_df2, "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/site_summary3.csv")


# value for each locality_id
value <- data.frame(LOCALITY_ID = formula_df2$LOCALITY_ID,
                    value = formula_df2$norm.days_since +
                      formula_df2$norm.distance_sample +
                      formula_df2$norm.m_w_t +
                      formula_df2$norm.m_w_t_n_n)


# make map of values in space to present to a user
values_sites_coords <- value %>%
  left_join(., sites_coords, by="LOCALITY_ID")

ggplot()+
  geom_sf(data=sydney, fill="lightblue")+
  geom_point(data=values_sites_coords, aes(x=lng, y=lat, color=value), size=2.3)+
  coord_sf(xlim = c(151.05, 151.3), ylim = c(-33.97, -33.77))+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'transparent'))+
  scale_colour_gradient(low = "gray60", high = "gray1")

ggsave(filename = "C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/value_map.png",
       height=6, width=6, units="in")




# put all of the above into a loop to calculate a number of different maps 
# to make a gif showing the change in value through time
list_of_sites <- c("L945869", "L2557723", "L8083126", "L1030678", "L915566", 
          "L1300136", "L5076722", "L2444301", "L3007885", "L5146094")

dates <- c("2018-01-01", "2018-01-15", "2018-02-01", "2018-02-15", "2018-03-01", "2018-03-15",
           "2018-04-01", "2018-04-15", "2018-05-01", "2018-05-15", "2018-06-01", "2018-06-15",
           "2018-07-01", "2018-07-15", "2018-08-01", "2018-08-15", "2018-09-01", "2018-09-15",
           "2018-10-01", "2018-10-15", "2018-11-01", "2018-11-15", "2018-12-01", "2018-12-15")
  
for (i in dates) {

  site_summary <- return_summary_data(list_of_sites, date=i)
 
  ######################################################################
  ######################################################################
  # Now need to develop a formula with which to calculat the 'value' 
  # of a given observation
  ######################################################################
  ######################################################################
  site_summary <- site_summary %>%
    replace_na(list(median_waiting_time = max(.$median_waiting_time, na.rm=TRUE)+(1*sd(.$median_waiting_time, na.rm=TRUE)))) %>%
    replace_na(list(N=0)) %>%
    replace_na(list(days_since=max(.$days_since, na.rm=TRUE)+(1*sd(.$days_since, na.rm=TRUE)))) %>%
    replace_na(list(neighbor_waiting_time=max(.$neighbor_waiting_time, na.rm=TRUE)+(1*sd(.$neighbor_waiting_time, na.rm=TRUE)))) %>%
    replace_na(list(duration=max(.$duration, na.rm=TRUE)+(1*sd(.$duration, na.rm=TRUE))))
 
  
  # make yes/no a multiplier
  # 0 for yes and 1 for no
  # this means that this variable will 
  # drop out if all sites have been sampled
  # and means that unsampled sites will be
  # more valuable
  site_summary <- site_summary %>%
    mutate(sampled=gsub("yes", 0, .$sampled)) %>%
    mutate(sampled=gsub("no", 1, .$sampled)) %>%
    mutate(sampled=as.numeric(as.character(.$sampled)))
  
  # make variables to go into "formula"
  formula_df <- data.frame(LOCALITY_ID = site_summary$LOCALITY_ID,
                           distance_sample = site_summary$dist_km*site_summary$sampled,
                           m_w_t = site_summary$median_waiting_time*site_summary$duration,
                           m_w_t_n_n = site_summary$neighbor_waiting_time,
                           days_since = site_summary$days_since)
  
  # standardize variables according to the 'scaling' factor
  formula_df2 <- formula_df %>%
    mutate(norm.distance_sample = normalize(.$distance_sample, method="range", range=c(0, 25))) %>%
    mutate(norm.m_w_t = normalize(.$m_w_t, method="range", range=c(0,25))) %>%
    mutate(norm.m_w_t_n_n = normalize(.$m_w_t_n_n, method="range", range=c(0,25))) %>%
    mutate(norm.days_since = normalize(.$days_since, method="range", range=c(0,25)))

  # value for each locality_id
  value <- data.frame(LOCALITY_ID = formula_df2$LOCALITY_ID,
                      value = formula_df2$norm.days_since +
                        formula_df2$norm.distance_sample +
                        formula_df2$norm.m_w_t +
                        formula_df2$norm.m_w_t_n_n)
  
  
  # make map of values in space to present to a user
  values_sites_coords <- value %>%
    left_join(., sites_coords, by="LOCALITY_ID")
  
  ggplot()+
    geom_sf(data=sydney, fill="lightblue")+
    geom_point(data=values_sites_coords, aes(x=lng, y=lat, size=value), color="firebrick3")+
    scale_size_continuous(breaks=c(30, 40, 50, 60, 70))+
    coord_sf(xlim = c(151.05, 151.3), ylim = c(-33.97, -33.77))+
    xlab("")+
    ylab("")+
    theme_bw()+
    theme(panel.grid.major = element_line(colour = 'transparent'))+
    ggtitle(paste0(i))
  
  ggsave(filename = paste0("C:/Users/CTC/Desktop/citizen_science_value/Submissions/PLoS Biology/Revision 1/Appendix/gif_figures/", i, ".png"),
         height=6, width=6, units="in")
  
  
}








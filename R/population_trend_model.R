## A function to apply for every species to assess 'population trends'

# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(mgcv)
library(lme4)
library(car)
library(readr)


# load data
load("Data/Greater_Sydney_data.RData")

clements <- read_csv("Data/Clements-Checklist-v2018-August-2018.csv") %>%
  rename(COMMON_NAME = `English name`)

# filter the dataset
# remove any incomplete checklists
GS_observations <- GS_observations %>%
  dplyr::filter(ALL_SPECIES_REPORTED == 1) %>%
  dplyr::filter(CATEGORY %in% c("species", "issf", "domestic")) %>%
  dplyr::filter(COMMON_NAME != "Indian Peafowl (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Mallard (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Muscovy Duck (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Graylag Goose (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Cockatiel (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "African Collared-Dove (Domestic type or Ringe") %>%
  dplyr::filter(COMMON_NAME != "Budgerigar (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Red Junglefowl (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Helmeted Guineafowl (Domestic type)") %>%
  dplyr::filter(COMMON_NAME != "Domestic goose sp. (Domestic type)") %>%
  left_join(., clements, by="COMMON_NAME") %>%
  dplyr::filter(family != "Stercorariidae (Skuas and Jaegers)") %>%
  dplyr::filter(family != "Alcidae (Auks, Murres, and Puffins)") %>%
  dplyr::filter(family != "Diomedeidae (Albatrosses)") %>%
  dplyr::filter(family != "Oceanitidae (Southern Storm-Petrels)") %>%
  dplyr::filter(family != "Hydrobatidae (Northern Storm-Petrels)") %>%
  dplyr::filter(family != "Procellariidae (Shearwaters and Petrels)") %>%
  dplyr::filter(family != "Fregatidae (Frigatebirds)") %>%
  dplyr::filter(family != "Sulidae (Boobies and Gannets)") %>%
  dplyr::filter(PROTOCOL_TYPE != "Incidental") %>%
  dplyr::filter(PROTOCOL_TYPE != "Historical") %>%
  dplyr::filter(PROTOCOL_TYPE != "Nocturnal Flight Call Count") %>%
  dplyr::filter(PROTOCOL_TYPE != "Banding") %>%
  dplyr::mutate(hour=hour(TIME_OBSERVATIONS_STARTED)) %>%
  dplyr::filter(hour >= 5 & hour <= 20) %>%
  dplyr::filter(DURATION_MINUTES >= 5) %>%
  dplyr::filter(DURATION_MINUTES <= 240) %>%
  dplyr::filter(EFFORT_DISTANCE_KM <= 5) %>%
  dplyr::filter(EFFORT_AREA_HA <= 500)


# A list of all sites
localities <- GS_observations %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  distinct()

# Number of species on a list
list_species <- GS_observations %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(Number_species=length(unique(SCIENTIFIC_NAME)))

# all checklists and their dates
checklists_dates <- GS_observations %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE) %>%
  distinct()


min_date <- checklists_dates %>%
  arrange(OBSERVATION_DATE) %>%
  slice(1) %>%
  .$OBSERVATION_DATE

max_date <- checklists_dates %>%
  arrange(desc(OBSERVATION_DATE)) %>%
  slice(1) %>%
  .$OBSERVATION_DATE

date_sequence <- data.frame(OBSERVATION_DATE = seq.Date(min_date, max_date, by=1)) %>%
  mutate(DATE_CONTINUOUS=1:nrow(.))

# get a model to run for each species in the dataset
model_leverage_predict_function <- function(species) {
  
  message(paste0("Analyzing ", species))
  
  df <- GS_observations %>%
    dplyr::filter(COMMON_NAME == species) %>%
    mutate(Year=year(OBSERVATION_DATE)) %>%
    mutate(occurrence = 1) %>%
    left_join(., list_species, by="SAMPLING_EVENT_IDENTIFIER")
  
  
  species_obs <- df %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, occurrence)
  
  # create dataframe for a model
  mod_data <- GS_observations %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, LOCALITY_ID,
                  COUNTY, LOCALITY_TYPE, PROTOCOL_TYPE, PROTOCOL_CODE,
                  DURATION_MINUTES, EFFORT_DISTANCE_KM, OBSERVER_ID) %>%
    distinct() %>%
    left_join(., date_sequence, by="OBSERVATION_DATE") %>%
    left_join(., list_species, by="SAMPLING_EVENT_IDENTIFIER") %>%
    left_join(., species_obs, by="SAMPLING_EVENT_IDENTIFIER") %>%
    arrange(desc(occurrence)) %>%
    tidyr::fill(COMMON_NAME) %>%
    tidyr::fill(SCIENTIFIC_NAME) %>%
    replace_na(list(occurrence=0)) %>%
    mutate(Year = as.factor(year(OBSERVATION_DATE))) %>%
    mutate(COUNTY = as.factor(as.character(.$COUNTY))) %>%
    mutate(LOCALITY_ID = as.factor(as.character(.$LOCALITY_ID))) %>%
    mutate(OBSERVER_ID = as.factor(as.character(.$OBSERVER_ID))) %>%
    dplyr::filter(Number_species >=4)
  
  
  mod <- glm(occurrence ~  DATE_CONTINUOUS + COUNTY + offset(Number_species),
             family=binomial(link="logit"), data=mod_data)
  
  infl.mod <- influence(mod)
  
  dfbetas.mod <- dfbetas(mod, infl=infl.mod)
  
  DATE_betas <- as.data.frame(dfbetas.mod) %>%
    dplyr::select(DATE_CONTINUOUS)
  
  leverage_df <- data.frame(SAMPLING_EVENT_IDENTIFIER=mod_data$SAMPLING_EVENT_IDENTIFIER,
                            cooks_dist=cooks.distance(mod),
                            DATE_dfbetas=DATE_betas$DATE_CONTINUOUS,
                            DATE_CONTINUOUS=mod_data$DATE_CONTINUOUS,
                            Number_species=mod_data$Number_species,
                            COUNTY=mod_data$COUNTY,
                            Year=mod_data$Year) %>%
    mutate(COMMON_NAME = species)
  
  return(leverage_df)
}


species_N <- GS_observations %>%
  group_by(SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  left_join(., select(rename(clements, SCIENTIFIC_NAME = `scientific name`), COMMON_NAME, SCIENTIFIC_NAME), by="SCIENTIFIC_NAME")


list_of_species <- species_N %>%
  dplyr::filter(N>50) %>%
  .$COMMON_NAME



list_of_model_results <- parallel::mclapply(list_of_species, function(x) {model_leverage_predict_function(x)})


df_of_model_results <- bind_rows(list_of_model_results)

leverage_for_each_checklist <- df_of_model_results %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(cooks_dist=mean(cooks_dist),
            DATE_dfbetas=sum(abs(DATE_dfbetas)))

leverage_results <- GS_observations %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, LOCALITY_ID,
                COUNTY, LOCALITY_TYPE, PROTOCOL_TYPE, PROTOCOL_CODE,
                DURATION_MINUTES, EFFORT_DISTANCE_KM, OBSERVER_ID) %>%
  distinct() %>%
  left_join(., leverage_for_each_checklist, by="SAMPLING_EVENT_IDENTIFIER")

leverage_results %>%
  arrange(desc(DATE_dfbetas)) -> leverage_results


saveRDS(leverage_results, file = "Data/leverage_results.RDS")

filter(GS_observations,SAMPLING_EVENT_IDENTIFIER==leverage_results$SAMPLING_EVENT_IDENTIFIER[1]) -> best

filter(df_of_model_results,SAMPLING_EVENT_IDENTIFIER==leverage_results$SAMPLING_EVENT_IDENTIFIER[1]) -> best










mod2 <- mgcv::gam(occurrence ~  s(DATE_CONTINUOUS) + s(N) + as.factor(Year) + 
                    s(COUNTY, bs="re"), data=mod_data)


plot(mod2, page=1)

fref <- fitted(mod2)

f <- function(index) {
  dat <- example_df[-index, ]
  m <- mgcv::gam(occurrence ~  s(DATE_CONTINUOUS) + s(N) + as.factor(Year) + 
              s(COUNTY, bs="re"), data=example_df)
  
  # sum of squared differences between this fit and reference model
  # based on full dataset
  sum( (fref[-index] - fitted(m))^2 )
}

influence <- sapply(1:100, f)
















mod <- glm(occurrence ~  DATE_CONTINUOUS + N + as.factor(Year) + COUNTY, data=example_df)

library(car)

cooks.distance(mod)

plot(mod)


cooks.distance(mod)





library(raster)
r1 <- raster(nrows=40, ncols=60, xmn=min(checks$long), xmx=max(checks$long),ymn=min(checks$lat),ymx=max(checks$lat))

?rasterize
GS_observations %>% group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(lat=median(LATITUDE),long=median(LONGITUDE)) %>%
  left_join(leverage_for_each_checklist)-> checks

checks<-filter(checks,!is.na(leverage))

xy <- cbind(checks$long, checks$lat)

rr<-rasterize(xy,r1,field=log10(checks$leverage))
plot(rr)

theme_set(theme_bw())
library(rasterVis)
library(viridis)
gplot(rr) + geom_tile(aes(fill = value)) +
  scale_fill_viridis()+
  coord_equal()

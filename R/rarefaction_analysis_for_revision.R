### an R script to investigate the
### the relationship between the number of checklists
### and the R2 of a glm for a population trend
## Now export some images to make a gif with
## to show how the standard error can change through time

species <- "Noisy Miner"

# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(mgcv)
library(lme4)
library(car)
library(readr)
library(rsq)
library(sf)


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

sampling_many_times <- function(run_number) {

sampling_model_function <- function(percent) {

species_model_function <- function(species) {

df <- GS_observations %>%
  dplyr::filter(COMMON_NAME == species) %>%
  mutate(Year=year(OBSERVATION_DATE)) %>%
  mutate(occurrence = 1) %>%
  left_join(., list_species, by="SAMPLING_EVENT_IDENTIFIER")

species_obs <- df %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, occurrence)

mod_data <- GS_observations %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, OBSERVATION_DATE, LOCALITY_ID,
                COUNTY, LOCALITY_TYPE, PROTOCOL_TYPE, PROTOCOL_CODE,
                DURATION_MINUTES, EFFORT_DISTANCE_KM, OBSERVER_ID,
                LATITUDE, LONGITUDE) %>%
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

N <- round(nrow(mod_data)*.01*percent, digits=0)

mod_data2 <- mod_data %>%
  sample_n(N)


mod <- glm(occurrence ~  DATE_CONTINUOUS + COUNTY + offset(Number_species),
           family=binomial(link="logit"), data=mod_data2)

summary_data <- data.frame(aic=mod$aic,
                           deviance=mod$deviance) %>%
  mutate(COMMON_NAME=species) %>%
  mutate(unique_localities=length(unique(mod_data$LOCALITY_ID))) %>%
  mutate(number_of_observers=length(unique(mod_data$OBSERVER_ID))) %>%
  mutate(rsq=rsq(mod)) %>%
  mutate(slope_date=as.data.frame(summary(mod)$coefficients)[1][2,]) %>%
  mutate(mod_converged=mod$converged)

return(summary_data)

}

species_N <- GS_observations %>%
  group_by(SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  left_join(., select(rename(clements, SCIENTIFIC_NAME = `scientific name`), COMMON_NAME, SCIENTIFIC_NAME), by="SCIENTIFIC_NAME") %>%
  arrange(desc(N))

list_of_species <- species_N %>%
  dplyr::filter(N>100) %>%
  slice(1:10) %>%
  .$COMMON_NAME

list_of_model_results <- parallel::mclapply(list_of_species, function(x) {species_model_function(x)})

df_results <- bind_rows(list_of_model_results) %>%
  mutate(percent_of_sample=percent)

}


sample_size = seq(10, 100, by=5)

list_of_models <- parallel::mclapply(sample_size, function(x) {sampling_model_function(x)})

mid_level_results <- bind_rows(list_of_models) %>%
  mutate(permutation=run_number)

saveRDS(mid_level_results, paste0("Data/permutation_sensitivity/top_ten_species/", run_number, ".RDS"))

}


list_of_runs <- c(1:100)

final_results <- parallel::mclapply(list_of_runs[1:50], function(x) {sampling_many_times(x)})

dataframe_of_results <- bind_rows(final_results)


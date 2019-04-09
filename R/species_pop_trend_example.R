# An example species of a population trend model in time
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
  summarise(N=length(unique(SCIENTIFIC_NAME)))

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




# pick a species
species <- "Noisy Miner"

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
  dplyr::filter(N >=4)


mod <- glm(occurrence ~  DATE_CONTINUOUS,
           family=binomial(link="logit"), data=mod_data)

new_dat <- date_sequence$DATE_CONTINUOUS


predicted <- data.frame(prob=predict(mod, list(DATE_CONTINUOUS=new_dat), type="response"),
                        DATE_CONTINUOUS=date_sequence$DATE_CONTINUOUS,
                        date=date_sequence$OBSERVATION_DATE)

ggplot()+
  geom_line(data=predicted, aes(x=date, y=prob))

NM <- ggplot(data=mod_data, aes(x=OBSERVATION_DATE, y=occurrence))+
  geom_smooth(method="glm", method.args=list(family=binomial(link="logit")))+
  theme_bw()+
  xlab("")+
  ylab("Probability of occurrence")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks=element_line(color="black"))+
  theme(panel.grid.major.y=element_blank())

date_sequence2 <- date_sequence %>%
  mutate(Month=month(OBSERVATION_DATE)) %>%
  mutate(Day=day(OBSERVATION_DATE)) %>%
  dplyr::filter(Day==1)


for (i in unique(as.character(as.Date(date_sequence2$OBSERVATION_DATE)))) {
  
  mod_data2 <- mod_data %>%
    dplyr::filter(OBSERVATION_DATE < i)
  
  print(nrow(mod_data2))
  
  ggplot(data=mod_data2, aes(x=OBSERVATION_DATE, y=occurrence))+
    geom_smooth(method="glm", method.args=list(family=binomial(link="logit")), color="red")+
    ggtitle(paste0(i, ":  N=", nrow(mod_data2), " observations"))+
    xlab("Date")+
    ylab("Probability of occurrence")+
    ylim(0, 1)+
    theme_bw()+
    theme(panel.grid.major=element_blank())+
    theme(panel.grid.minor=element_blank())+
    theme(axis.text=element_text(color="black"))
  
  ggsave(filename = paste0("C:/Users/CTC/Desktop/gif_species/", i, ".png"))
  
}
  
  
# pick a species
species <- "Crested Pigeon"

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
  dplyr::filter(N >=4)


mod <- glm(occurrence ~  DATE_CONTINUOUS,
           family=binomial(link="logit"), data=mod_data)

new_dat <- date_sequence$DATE_CONTINUOUS


predicted <- data.frame(prob=predict(mod, list(DATE_CONTINUOUS=new_dat), type="response"),
                        DATE_CONTINUOUS=date_sequence$DATE_CONTINUOUS,
                        date=date_sequence$OBSERVATION_DATE)

ggplot()+
  geom_line(data=predicted, aes(x=date, y=prob))

CP <- ggplot(data=mod_data, aes(x=OBSERVATION_DATE, y=occurrence))+
  geom_smooth(method="glm", method.args=list(family=binomial(link="logit")))+
  theme_bw()+
  xlab("Date")+
  ylab("")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks=element_line(color="black"))+
  theme(panel.grid.major.y=element_blank())


# pick a species
species <- "Hardhead"

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
  dplyr::filter(N >=4)


mod <- glm(occurrence ~  DATE_CONTINUOUS,
           family=binomial(link="logit"), data=mod_data)

new_dat <- date_sequence$DATE_CONTINUOUS


predicted <- data.frame(prob=predict(mod, list(DATE_CONTINUOUS=new_dat), type="response"),
                        DATE_CONTINUOUS=date_sequence$DATE_CONTINUOUS,
                        date=date_sequence$OBSERVATION_DATE)

ggplot()+
  geom_line(data=predicted, aes(x=date, y=prob))

HH <- ggplot(data=mod_data, aes(x=OBSERVATION_DATE, y=occurrence))+
  geom_smooth(method="glm", method.args=list(family=binomial(link="logit")))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks=element_line(color="black"))+
  theme(panel.grid.major.y=element_blank())




# pick a species
species <- "Masked Lapwing"

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
  dplyr::filter(N >=4)


mod <- glm(occurrence ~  DATE_CONTINUOUS,
           family=binomial(link="logit"), data=mod_data)

new_dat <- date_sequence$DATE_CONTINUOUS


predicted <- data.frame(prob=predict(mod, list(DATE_CONTINUOUS=new_dat), type="response"),
                        DATE_CONTINUOUS=date_sequence$DATE_CONTINUOUS,
                        date=date_sequence$OBSERVATION_DATE)

ggplot()+
  geom_line(data=predicted, aes(x=date, y=prob))

ML <- ggplot(data=mod_data, aes(x=OBSERVATION_DATE, y=occurrence))+
  geom_smooth(method="glm", method.args=list(family=binomial(link="logit")))+
  theme_bw()+
  xlab("Date")+
  ylab("Probability of occurrence")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks=element_line(color="black"))+
  theme(panel.grid.major.y=element_blank())



## Put the four together into one plot
library(patchwork)


NM + HH + ML + CP + plot_layout(ncol=2)

ggsave("Figures/glm_examples.png", width=8, height=7, units="in")

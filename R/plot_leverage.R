## An R script to plot predicted leverage
## Will do it for 5 km grid cell size first

library(dplyr)
library(sf)
library(ggplot2)
library(wesanderson)

# 5 km first
setwd("Data/predicted_leverage/5_km")

file_list <- list.files()


for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- readRDS(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-readRDS(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

setwd('..')
setwd('..')
setwd('..')

load("Data/Spatial data/sydney_grids_5_km.RData")


leverage_5 <- dataset %>%
  distinct() %>%
  mutate(Value=exp(.fitted)) %>%
  group_by(grid_id, dynamic_date) %>%
  sample_n(1) %>%
  ungroup() 



df <- leverage_5 %>%
  dplyr::filter(dynamic_date==date)

map_df <- sydney_grids_5_km %>%
  right_join(., df, by="grid_id")

plasma_pal <- c("red", viridis::plasma(n = 6))

ggplot()+
  geom_sf(data=map_df, aes(fill=Value))+
  theme_bw()+
  theme(panel.grid.major=element_line(color="transparent"))+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  scale_fill_gradientn(colours=pal, name="Value", trans="log")
  
# potential figure for paper
example_dates <- c("2018-01-01", "2018-03-01", "2018-05-01",
                   "2018-07-01", "2018-09-01", "2018-11-01")

df <- leverage_5 %>%
  dplyr::filter(as.character(as.Date(dynamic_date)) %in% example_dates)

map_df <- sydney_grids_5_km %>%
  right_join(., df, by="grid_id") %>%
  st_simplify()


ggplot()+
  geom_sf(data=map_df, aes(fill=Value))+
  theme_bw()+
  theme(panel.grid.major=element_line(color="transparent"))+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  facet_wrap(~dynamic_date)+
  scale_fill_gradientn(colours=plasma_pal, name="Value", trans="log", 
                       breaks=c(0.01, 0.05, 0.15, 0.5, 3, 25), labels=c(0.01, 0.05, 0.15, 0.5, 3, 25),
                       limits=c(0.01, 30))

ggsave("Figures/example_maps.png", width=8, height=6, units="in")

# write a for loop to export figures to make a gif
dates_2018 <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by=1)



for (i in dates_2018) {
  
  df <- leverage_5 %>%
    dplyr::filter(dynamic_date==i)
  
  map_df <- sydney_grids_5_km %>%
    right_join(., df, by="grid_id")
  
  plot <- ggplot()+
    geom_sf(data=map_df, aes(fill=Value))+
    theme_bw()+
    theme(panel.grid.major=element_line(color="transparent"))+
    theme(axis.text=element_blank())+
    theme(axis.ticks=element_blank())+
    facet_wrap(~dynamic_date)+
    scale_fill_gradientn(colours=plasma_pal, name="Value", trans="log", 
                         breaks=c(0.01, 0.05, 0.15, 0.5, 3, 25), labels=c(0.01, 0.05, 0.15, 0.5, 3, 25),
                         limits=c(0.01, 30))
  
  ggsave(plot, filename=paste0("C:/Users/CTC/Desktop/map_gif2/", i, ".png"), dpi=150)
}










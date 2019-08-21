### This file plots the results of part 1 and part 2
### which looks to compare the distribution between
### Sydney and other cities and their checklists per grids

# packages
library(dplyr)
library(purrr)

setwd("Data/other_cities_analysis/summary data")

# data
df <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()


ggplot(df, aes(x=N))+
  geom_histogram(color="black", fill="green", bins=50)+
  scale_x_log10()+
  facet_wrap(~city, ncol=3)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of checklists in a grid")+
  ylab("Number of grid cells")

setwd('..')
setwd('..')
setwd('..')

ggsave("Figures/city_checklist_per_grid_comparison.png", width=6, height=8, units="in")

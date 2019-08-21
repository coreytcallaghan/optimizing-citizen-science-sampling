## This script makes funnel plots
## and assesses funnel plots for each species


# packages
library(ggplot2)
library(dplyr)
library(purrr)


setwd("Data/permutation_sensitivity/top_ten_species")

df_one_to_ten <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

setwd("..")


setwd("eleven_to_twenty_species")

df_eleven_to_twenty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

setwd("..")

setwd("twentyone_to_thirty_species")

df_twentyone_to_thirty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()


setwd("..")


data <- bind_rows(df_one_to_ten,
                  df_eleven_to_twenty,
                  df_twentyone_to_thirty)

# remove all models which did not converge!
data2 <- data %>%
  dplyr::filter(mod_converged=="TRUE") %>%
  group_by(COMMON_NAME) %>%
  mutate(upper_quartile=quantile(slope_date, 0.75)) %>%
  mutate(lower_quartile=quantile(slope_date, 0.25)) %>%
  mutate(mean_slope=mean(slope_date)) %>%
  mutate(sd_slope=sd(slope_date)) %>%
  mutate(greater_than_2_sd_top=ifelse(slope_date >= mean_slope+sd_slope+sd_slope, "TRUE", "FALSE")) %>%
  mutate(lesser_than_2_sd_bottom=ifelse(slope_date <=mean_slope-sd_slope-sd_slope, "TRUE", "FALSE")) %>%
  mutate(within_2_sd = ifelse(greater_than_2_sd_top == "TRUE" | lesser_than_2_sd_bottom == "TRUE", "FALSE", "TRUE"))

ggplot(data, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter()+
  facet_wrap(~COMMON_NAME, scales="free")







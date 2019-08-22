## This script makes funnel plots
## and assesses funnel plots for each species


# packages
library(ggplot2)
library(dplyr)
library(purrr)
library(quantreg)
library(forcats)


setwd("Data/permutation_sensitivity/top_ten_species")

df_one_to_ten <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

group1 <- df_one_to_ten %>%
  dplyr::filter(mod_converged=="TRUE") %>%
  group_by(COMMON_NAME) %>%
  mutate(upper_quartile=quantile(slope_date, 0.75)) %>%
  mutate(lower_quartile=quantile(slope_date, 0.25)) %>%
  mutate(mean_slope=mean(slope_date)) %>%
  mutate(sd_slope=sd(slope_date)) %>%
  mutate(greater_than_2_sd_top=ifelse(slope_date >= mean_slope+sd_slope+sd_slope, "TRUE", "FALSE")) %>%
  mutate(lesser_than_2_sd_bottom=ifelse(slope_date <=mean_slope-sd_slope-sd_slope, "TRUE", "FALSE")) %>%
  mutate(within_2_sd = ifelse(greater_than_2_sd_top == "TRUE" | lesser_than_2_sd_bottom == "TRUE", "FALSE", "TRUE")) %>%
  dplyr::filter(within_2_sd == "TRUE") %>%
  # special filter for Welcome Swallow - with one weird data point
  dplyr::filter(slope_date > -10^9)

ggplot(group1, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")


setwd("..")
setwd("..")
setwd("..")

ggsave("Figures/revision_power_1.png", width=6, height=8, units="in")


setwd("Data/permutation_sensitivity/eleven_to_twenty_species")

df_eleven_to_twenty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

group2 <- df_eleven_to_twenty %>%
  dplyr::filter(mod_converged=="TRUE") %>%
  group_by(COMMON_NAME) %>%
  mutate(upper_quartile=quantile(slope_date, 0.75)) %>%
  mutate(lower_quartile=quantile(slope_date, 0.25)) %>%
  mutate(mean_slope=mean(slope_date)) %>%
  mutate(sd_slope=sd(slope_date)) %>%
  mutate(greater_than_2_sd_top=ifelse(slope_date >= mean_slope+sd_slope+sd_slope, "TRUE", "FALSE")) %>%
  mutate(lesser_than_2_sd_bottom=ifelse(slope_date <=mean_slope-sd_slope-sd_slope, "TRUE", "FALSE")) %>%
  mutate(within_2_sd = ifelse(greater_than_2_sd_top == "TRUE" | lesser_than_2_sd_bottom == "TRUE", "FALSE", "TRUE")) %>%
  dplyr::filter(within_2_sd == "TRUE")

ggplot(group2, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

setwd("..")
setwd("..")
setwd("..")

ggsave("Figures/revision_power_2.png", width=6, height=8, units="in")


setwd("Data/permutation_sensitivity/twentyone_to_thirty_species")

df_twentyone_to_thirty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

group3 <- df_twentyone_to_thirty %>%
  dplyr::filter(mod_converged=="TRUE") %>%
  group_by(COMMON_NAME) %>%
  mutate(upper_quartile=quantile(slope_date, 0.75)) %>%
  mutate(lower_quartile=quantile(slope_date, 0.25)) %>%
  mutate(mean_slope=mean(slope_date)) %>%
  mutate(sd_slope=sd(slope_date)) %>%
  mutate(greater_than_2_sd_top=ifelse(slope_date >= mean_slope+sd_slope+sd_slope, "TRUE", "FALSE")) %>%
  mutate(lesser_than_2_sd_bottom=ifelse(slope_date <=mean_slope-sd_slope-sd_slope, "TRUE", "FALSE")) %>%
  mutate(within_2_sd = ifelse(greater_than_2_sd_top == "TRUE" | lesser_than_2_sd_bottom == "TRUE", "FALSE", "TRUE")) %>%
  dplyr::filter(within_2_sd == "TRUE")

ggplot(group3, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

setwd("..")
setwd("..")
setwd("..")

ggsave("Figures/revision_power_3.png", width=6, height=8, units="in")




setwd("Data/permutation_sensitivity/ninetyone_to_hundred_species")

df_ninetyone_to_hundred <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

group10 <- df_ninetyone_to_hundred %>%
  dplyr::filter(mod_converged=="TRUE") %>%
  group_by(COMMON_NAME) %>%
  mutate(upper_quartile=quantile(slope_date, 0.75)) %>%
  mutate(lower_quartile=quantile(slope_date, 0.25)) %>%
  mutate(mean_slope=mean(slope_date)) %>%
  mutate(sd_slope=sd(slope_date)) %>%
  mutate(greater_than_1_sd_top=ifelse(slope_date >= mean_slope+(sd_slope*0.5), "TRUE", "FALSE")) %>%
  mutate(lesser_than_1_sd_bottom=ifelse(slope_date <= mean_slope-(sd_slope*0.5), "TRUE", "FALSE")) %>%
  mutate(within_1_sd = ifelse(greater_than_1_sd_top == "TRUE" | lesser_than_1_sd_bottom == "TRUE", "FALSE", "TRUE")) %>%
  dplyr::filter(within_1_sd == "TRUE")

ggplot(group10, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

setwd("..")
setwd("..")
setwd("..")

ggsave("Figures/revision_power_10.png", width=6, height=8, units="in")






data <- bind_rows(group1,
                  group2,
                  group3)



idk <- data %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(max=max(slope_date),
            min=min(slope_date)) %>%
  mutate(range=max-min) %>%
  mutate(max_range=max(range)) %>%
  mutate(sd_range=sd(range)) %>%
  mutate(mean_range=mean(range)) %>%
  mutate(median_range=median(range)) %>%
  mutate(percent_range=round((range/max_range)*100)) %>%
  mutate(Number_checklists=round(0.01*percent_of_sample*25995)) %>%
  mutate(fifty_percent_max_range=ifelse(percent_range <=50, "TRUE", "FALSE")) %>%
  mutate(eighty_percent_max_range=ifelse(percent_range <= 80, "TRUE", "FALSE")) %>%
  mutate(median_range_met=ifelse(range <= median_range, "TRUE", "FALSE")) %>%
  mutate(mean_range_met=ifelse(range <= mean_range, "TRUE", "FALSE"))


idk %>%
  dplyr::select(COMMON_NAME, max_range) %>%
  distinct() %>%
  arrange(desc(max_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=max_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("Max range of slope estimates")

idk %>%
  dplyr::select(COMMON_NAME, sd_range) %>%
  distinct() %>%
  arrange(desc(sd_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=sd_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("SD range of slope estimates")

idk %>%
  dplyr::select(COMMON_NAME, mean_range) %>%
  distinct() %>%
  arrange(desc(mean_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=mean_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("Mean range of slope estimates")

idk %>%
  arrange(COMMON_NAME, desc(percent_range), fifty_percent_max_range) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(fifty_percent_max_range=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number checklists necessary for 50% reduction in range of slope estimates")

idk %>%
  arrange(COMMON_NAME, desc(percent_range), median_range_met) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(median_range_met=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number of checklists for the median range to be met")

idk %>%
  arrange(COMMON_NAME, desc(percent_range), mean_range_met) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(mean_range_met=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number of checklists for the mean range to be met")







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
  mutate(within_2_sd = ifelse(greater_than_2_sd_top == "TRUE" | lesser_than_2_sd_bottom == "TRUE", "FALSE", "TRUE")) %>%
  dplyr::filter(within_2_sd == "TRUE")

ggplot(data2, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter()+
  facet_wrap(~COMMON_NAME, scales="free")







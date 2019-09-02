## This script makes funnel plots
## and assesses funnel plots for each species


# packages
library(ggplot2)
library(dplyr)
library(purrr)
library(quantreg)
library(forcats)

# set wd for a given ten species set
setwd("Data/permutation_sensitivity/top_ten_species")

# read in multiple RDSs
df_one_to_ten <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

# move wd back up to original level
setwd("..")
setwd("..")
setwd("..")

# create dataset
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

# make plot of slope estimates versus sample size
ggplot(group1, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

# export plot
ggsave("Figures/revision_power_1.png", width=6, height=8, units="in")

# get the best estimate slope (using as much data as possible)
# for each species
species_best_estimate1 <- group1 %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_best_estimate=mean(slope_date)) %>%
  arrange(COMMON_NAME, desc(percent_of_sample)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  dplyr::select(-percent_of_sample)

# append these slope estimates to the dataframe
difference_df1 <- group1 %>%
  left_join(., species_best_estimate1, by="COMMON_NAME") %>%
  mutate(relative_difference=mean_best_estimate-slope_date) %>%
  mutate(absolute_difference=abs(mean_best_estimate-slope_date))


ggplot(difference_df1, aes(x=percent_of_sample, y=relative_difference))+
  geom_jitter(col="grey",alpha=0.4)+
  facet_wrap(~COMMON_NAME, ncol=2)+
  ylim(-0.005, 0.003)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Difference from best slope estimate")+
  xlab("Percentage of total possible checklists")+
  geom_quantile(quantiles=c(0.05,0.95))

ggsave("Figures/slope_difference_1.png", width=6, height=8, units="in")


####################################################################
####################################################################
setwd("Data/permutation_sensitivity/eleven_to_twenty_species")

df_eleven_to_twenty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

# move wd back up to original level
setwd("..")
setwd("..")
setwd("..")

# create dataset
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

# make plot of slope estimates versus sample size
ggplot(group2, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

# export plot
ggsave("Figures/revision_power_2.png", width=6, height=8, units="in")

# get the best estimate slope (using as much data as possible)
# for each species
species_best_estimate2 <- group2 %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_best_estimate=mean(slope_date)) %>%
  arrange(COMMON_NAME, desc(percent_of_sample)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  dplyr::select(-percent_of_sample)

# append these slope estimates to the dataframe
difference_df2 <- group2 %>%
  left_join(., species_best_estimate2, by="COMMON_NAME") %>%
  mutate(relative_difference=mean_best_estimate-slope_date) %>%
  mutate(absolute_difference=abs(mean_best_estimate-slope_date))


ggplot(difference_df2, aes(x=percent_of_sample, y=relative_difference))+
  geom_jitter(col="grey",alpha=0.4)+
  facet_wrap(~COMMON_NAME, ncol=2)+
  ylim(-0.005, 0.003)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Difference from best slope estimate")+
  xlab("Percentage of total possible checklists")+
  geom_quantile(quantiles=c(0.05,0.95))

ggsave("Figures/slope_difference_2.png", width=6, height=8, units="in")



###################################################################
###################################################################
setwd("Data/permutation_sensitivity/twentyone_to_thirty_species")

df_twentyone_to_thirty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

# move wd back up to original level
setwd("..")
setwd("..")
setwd("..")

# create dataset
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
  
# make plot of slope estimates versus sample size
ggplot(group3, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

# export plot
ggsave("Figures/revision_power_3.png", width=6, height=8, units="in")

# get the best estimate slope (using as much data as possible)
# for each species
species_best_estimate3 <- group3 %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_best_estimate=mean(slope_date)) %>%
  arrange(COMMON_NAME, desc(percent_of_sample)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  dplyr::select(-percent_of_sample)

# append these slope estimates to the dataframe
difference_df3 <- group3 %>%
  left_join(., species_best_estimate3, by="COMMON_NAME") %>%
  mutate(relative_difference=mean_best_estimate-slope_date) %>%
  mutate(absolute_difference=abs(mean_best_estimate-slope_date))


ggplot(difference_df3, aes(x=percent_of_sample, y=relative_difference))+
  geom_jitter(col="grey",alpha=0.4)+
  facet_wrap(~COMMON_NAME, ncol=2)+
  ylim(-0.005, 0.003)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Difference from best slope estimate")+
  xlab("Percentage of total possible checklists")+
  geom_quantile(quantiles=c(0.05,0.95))

ggsave("Figures/slope_difference_3.png", width=6, height=8, units="in")


###################################################################
###################################################################
setwd("Data/permutation_sensitivity/thirtyone_to_forty_species")

df_thirtyone_to_forty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

# move wd back up to original level
setwd("..")
setwd("..")
setwd("..")

# create dataset
group4 <- df_thirtyone_to_forty %>%
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
  
# make plot of slope estimates versus sample size
ggplot(group4, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

# export plot
ggsave("Figures/revision_power_4.png", width=6, height=8, units="in")

# get the best estimate slope (using as much data as possible)
# for each species
species_best_estimate4 <- group4 %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_best_estimate=mean(slope_date)) %>%
  arrange(COMMON_NAME, desc(percent_of_sample)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  dplyr::select(-percent_of_sample)

# append these slope estimates to the dataframe
difference_df4 <- group4 %>%
  left_join(., species_best_estimate4, by="COMMON_NAME") %>%
  mutate(relative_difference=mean_best_estimate-slope_date) %>%
  mutate(absolute_difference=abs(mean_best_estimate-slope_date))


ggplot(difference_df4, aes(x=percent_of_sample, y=relative_difference))+
  geom_jitter(col="grey",alpha=0.4)+
  facet_wrap(~COMMON_NAME, ncol=2)+
  ylim(-0.005, 0.003)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Difference from best slope estimate")+
  xlab("Percentage of total possible checklists")+
  geom_quantile(quantiles=c(0.05,0.95))

ggsave("Figures/slope_difference_4.png", width=6, height=8, units="in")


###################################################################
###################################################################
setwd("Data/permutation_sensitivity/fortyone_to_fifty_species")

df_fortyone_to_fifty <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

# move wd back up to original level
setwd("..")
setwd("..")
setwd("..")

# create dataset
group5 <- df_fortyone_to_fifty %>%
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
  
# make plot of slope estimates versus sample size
ggplot(group5, aes(x=percent_of_sample, y=slope_date))+
  geom_jitter(col="grey",alpha=0.4)+
  geom_quantile(quantiles=c(0.05,0.95))+
  facet_wrap(~COMMON_NAME, scales="free", ncol=2)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Model slope")+
  xlab("Percentage of total possible checklists")

# export plot
ggsave("Figures/revision_power_5.png", width=6, height=8, units="in")

# get the best estimate slope (using as much data as possible)
# for each species
species_best_estimate5 <- group5 %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_best_estimate=mean(slope_date)) %>%
  arrange(COMMON_NAME, desc(percent_of_sample)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  dplyr::select(-percent_of_sample)

# append these slope estimates to the dataframe
difference_df5 <- group5 %>%
  left_join(., species_best_estimate5, by="COMMON_NAME") %>%
  mutate(relative_difference=mean_best_estimate-slope_date) %>%
  mutate(absolute_difference=abs(mean_best_estimate-slope_date))


ggplot(difference_df5, aes(x=percent_of_sample, y=relative_difference))+
  geom_jitter(col="grey",alpha=0.4)+
  facet_wrap(~COMMON_NAME, ncol=2)+
  ylim(-0.005, 0.003)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Difference from best slope estimate")+
  xlab("Percentage of total possible checklists")+
  geom_quantile(quantiles=c(0.05,0.95))

ggsave("Figures/slope_difference_5.png", width=6, height=8, units="in")






data <- bind_rows(group1,
                  group2,
                  group3,
                  group4,
                  group5)


difference_data <- bind_rows(difference_df1,
                             difference_df2,
                             difference_df3,
                             difference_df4,
                             difference_df5)






idk <- difference_data %>%
  group_by(COMMON_NAME, percent_of_sample) %>%
  summarise(mean_diff=mean(absolute_difference)) %>%
  mutate(max_diff=max(mean_diff)) %>%
  mutate(percent_diff=round((mean_diff/max_diff)*100)) %>%
  mutate(fifty_percent_max_diff=ifelse(percent_diff <=50, "TRUE", "FALSE")) %>%
  mutate(Number_checklists=round(0.01*percent_of_sample*25995))


idk %>%
  arrange(COMMON_NAME, desc(percent_diff), fifty_percent_max_diff) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(fifty_percent_max_diff=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram(bins=50)+
  ylab("Count")+
  xlab("Number checklists necessary for 50% reduction in difference from best estimate")+
  theme(axis.text=element_text(color="black"))+
  theme_classic()+
  theme(axis.text=element_text(color="black"))


ggsave("Figures/fifty_percent_reduction_in_absolute_difference.png", height=5.5, width=6.2, units="in")

  
idk %>%
  arrange(COMMON_NAME, desc(percent_diff), fifty_percent_max_diff) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(fifty_percent_max_diff=="TRUE") %>%
  slice(1) %>%
  summary()
  
  
  
  
  


idk2 <- data %>%
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





idk2 %>%
  dplyr::select(COMMON_NAME, max_range) %>%
  distinct() %>%
  arrange(desc(max_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=max_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("Max range of slope estimates")+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/max_range_of_slope_estimates.png", height=5.5, width=4.8, units="in")



























idk2 %>%
  dplyr::select(COMMON_NAME, sd_range) %>%
  distinct() %>%
  arrange(desc(sd_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=sd_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("SD range of slope estimates")+
  theme(axis.text=element_text(color="black"))

idk2 %>%
  dplyr::select(COMMON_NAME, mean_range) %>%
  distinct() %>%
  arrange(desc(mean_range)) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=mean_range))+
  coord_flip()+
  theme_classic()+
  geom_point()+
  xlab("")+
  ylab("Mean range of slope estimates")+
  theme(axis.text=element_text(color="black"))

idk2 %>%
  arrange(COMMON_NAME, desc(percent_range), fifty_percent_max_range) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(fifty_percent_max_range=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number checklists necessary for 50% reduction in range of slope estimates")+
  theme(axis.text=element_text(color="black"))

idk2 %>%
  arrange(COMMON_NAME, desc(percent_range), median_range_met) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(median_range_met=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number of checklists for the median range to be met")+
  theme(axis.text=element_text(color="black"))

idk2 %>%
  arrange(COMMON_NAME, desc(percent_range), mean_range_met) %>%
  group_by(COMMON_NAME) %>%
  dplyr::filter(mean_range_met=="TRUE") %>%
  slice(1) %>%
  ggplot(., aes(x=Number_checklists))+
  geom_histogram()+
  ylab("Count")+
  xlab("Number of checklists for the mean range to be met")+
  theme(axis.text=element_text(color="black"))














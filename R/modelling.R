### Modelling script

# packages
library(dplyr)
library(ggplot2)
library(BBmisc)
library(ggcorrplot)
library(corrplot)
library(lme4)
library(standardize)
library(tibble)
library(visreg)
library(arm)

# load data
lev <- readRDS("Data/Modelling data/leverage_results.RDS")

params <- readRDS("Data/Modelling data/parameter_dataset.RDS")

analysis_data <- params %>%
  left_join(., lev) %>%
  mutate(
    sampled_numeric=case_when(
    sampled=="yes" ~ 1,
    sampled=="no" ~ 0)
    )

# make df for modelling
hist(analysis_data$DATE_dfbetas, breaks=30)

hist(log(analysis_data$DATE_dfbetas), breaks=30)

# rescale variables
analysis_data <- analysis_data %>%
  group_by(grid_size) %>%
  mutate(s.median_waiting_time=arm::rescale(median_waiting_time)) %>%
  mutate(s.duration=arm::rescale(duration)) %>%
  mutate(s.Number_of_days_sampled=rescale(Number_of_days_sampled)) %>%
  mutate(s.days_since=rescale(days_since)) %>%
  mutate(s.dist_km_nn=rescale(dist_km_nn)) %>%
  mutate(s.neighbor_waiting_time=rescale(neighbor_waiting_time))

# look at correlation among predictor variables
cor_object5 <- analysis_data %>%
  dplyr::filter(grid_size==5) %>%
  dplyr::select(s.median_waiting_time, s.duration, s.Number_of_days_sampled, 
                s.days_since, s.dist_km_nn, s.neighbor_waiting_time) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object5[2:7])))+
  ggtitle("5 km grid size")

# look at correlation among predictor variables
cor_object10 <- analysis_data %>%
  dplyr::filter(grid_size==10) %>%
  dplyr::select(s.median_waiting_time, s.duration, s.Number_of_days_sampled, 
                s.days_since, s.dist_km_nn, s.neighbor_waiting_time) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object10[2:7])))+
  ggtitle("10 km grid size")


# look at correlation among predictor variables
cor_object25 <- analysis_data %>%
  dplyr::filter(grid_size==25) %>%
  dplyr::select(s.median_waiting_time, s.duration, s.Number_of_days_sampled, 
                s.days_since, s.dist_km_nn, s.neighbor_waiting_time) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object25[2:7])))+
  ggtitle("25 km grid size")


# look at correlation among predictor variables
cor_object50 <- analysis_data %>%
  dplyr::filter(grid_size==50) %>%
  dplyr::select(s.median_waiting_time, s.duration, s.Number_of_days_sampled, 
                s.days_since, s.dist_km_nn, s.neighbor_waiting_time) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object50[2:6])))+
  ggtitle("50 km grid size")



## Based on all that I think we should get rid of 'duration'
## as it appears to be the most correlated with both median waiting time
## and correlated with days since
### Run a model for each of the grid sizes

# 5 km grid
analysis_data.5 <- analysis_data %>% dplyr::filter(grid_size==5)
# see about median sampling interval
mod5 <- lm(log(DATE_dfbetas) ~ s.median_waiting_time + s.Number_of_days_sampled +
            s.days_since + s.dist_km_nn + s.neighbor_waiting_time, data=analysis_data.5)

par(mfrow=c(2, 2))
plot(mod5)

summary(mod5)
anova(mod5)


estimates_5 <- as.data.frame(mod5$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `mod5$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(mod5))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(mod5))[,2]) %>%
  mutate(p_value=summary(mod5)$coefficients[,4]) %>%
  mutate(grid_size=5)


# 10 km grid
analysis_data.10 <- analysis_data %>% dplyr::filter(grid_size==10)
# see about median sampling interval
mod10 <- lm(log(DATE_dfbetas) ~ s.median_waiting_time + s.Number_of_days_sampled +
             s.days_since + s.dist_km_nn + s.neighbor_waiting_time, data=analysis_data.10)

par(mfrow=c(2, 2))
plot(mod10)

summary(mod10)
anova(mod10)

estimates_10 <- as.data.frame(mod10$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `mod10$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(mod10))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(mod10))[,2]) %>%
  mutate(p_value=summary(mod10)$coefficients[,4]) %>%
  mutate(grid_size=10)

# 25 km grid
analysis_data.25 <- analysis_data %>% dplyr::filter(grid_size==25)
# see about median sampling interval
mod25 <- lm(log(DATE_dfbetas) ~ s.median_waiting_time + s.Number_of_days_sampled +
              s.days_since + s.dist_km_nn + s.neighbor_waiting_time, data=analysis_data.25)

par(mfrow=c(2, 2))
plot(mod25)

summary(mod25)
anova(mod25)

estimates_25 <- as.data.frame(mod25$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `mod25$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(mod25))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(mod25))[,2]) %>%
  mutate(p_value=summary(mod25)$coefficients[,4]) %>%
  mutate(grid_size=25)

# 50 km grid
analysis_data.50 <- analysis_data %>% dplyr::filter(grid_size==50)
# see about median sampling interval
mod50 <- lm(log(DATE_dfbetas) ~ s.median_waiting_time + s.Number_of_days_sampled +
              s.days_since + s.dist_km_nn, data=analysis_data.50)

par(mfrow=c(2, 2))
plot(mod50)

summary(mod50)
anova(mod50)

estimates_50 <- as.data.frame(mod50$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `mod50$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(mod50))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(mod50))[,2]) %>%
  mutate(p_value=summary(mod50)$coefficients[,4]) %>%
  mutate(grid_size=50)


param_new_names <- data.frame(params=c("(Intercept)", "s.median_waiting_time", "s.days_since", 
                                       "s.Number_of_days_sampled", "s.dist_km_nn", "s.neighbor_waiting_time"),
                              new_names=c("Intercept", "Median sampling interval", "Days since last sample", 
                                          "Number of unique days sampled", "Distance to nearest sampled grid (km)", 
                                          "Nearest neighbor sampling interval"))
# plot standardized parameter estimates
bind_rows(estimates_5, estimates_10, estimates_25, estimates_50) %>%
  mutate(significant=case_when(
    p_value < 0.5 ~ "Significant",
    p_value > 0.5 ~ "Not-significant"
  )) %>%
  left_join(., param_new_names, by="params") %>%
  dplyr::filter(new_names != "Intercept") %>%
  ggplot(., aes(x=new_names, y=standard_estimate))+
  geom_hline(yintercept=0, color="red", size=1.2)+
  geom_errorbar(aes(ymin=lwr_conf, ymax=upr_conf, width=0.3))+
  geom_point()+
  scale_x_discrete(labels=function(x) stringr::str_wrap(x, width=20))+
  coord_flip()+
  facet_wrap(~grid_size)+
  theme_bw()+
  xlab("")+
  ylab("Standardized parameter estimate")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.ticks=element_line(color="black"))+
  theme(panel.grid.major.y=element_blank())


ggsave(filename="Figures/param_estimates.png", width=6, height=5, units="in")

# explore visreg plots
par(mfrow=c(2, 2))
visreg(standard5, "z.median_waiting_time")
visreg(standard10, "z.median_waiting_time")
visreg(standard25, "z.median_waiting_time")
visreg(standard50, "z.median_waiting_time")



par(mfrow=c(2, 2))
visreg(standard5, "z.days_since")
visreg(standard10, "z.days_since")
visreg(standard25, "z.days_since")
visreg(standard50, "z.days_since")



# write out each model object
saveRDS(mod5, file="Data/Modelling data/model_5_km_object.RDS")
saveRDS(mod10, file="Data/Modelling data/model_10_km_object.RDS")
saveRDS(mod25, file="Data/Modelling data/model_25_km_object.RDS")
saveRDS(mod50, file="Data/Modelling data/model_50_km_object.RDS")




















# start with some very simple linear models
# see whether or not a grid was sampled is important
mod <- lm(log(DATE_dfbetas) ~ sampled, data=filter(analysis_data, grid_size==5))

par(mfrow=c(2, 2))
plot(mod)

summary(mod)













stand_mod <- standardize(log(DATE_dfbetas) ~ median_waiting_time + duration + Number_of_days_sampled +
                           days_since + dist_km_nn + neighbor_waiting_time + (1|grid_size), data=analysis_data)



mod <- lmer(stand_mod$formula, stand_mod$data)














# OLD STUFF!!
# try a linear model first
# no transform
mod <- lm(DATE_dfbetas ~ norm.distance_sample + norm.days_since + norm.m_w_t_n_n + norm.m_w_t, data=analysis_data)

par(mfrow=c(2, 2))
plot(mod)

summary(mod)

# doesn't look good!
# try a log-transform of response
mod2 <- lm(log(DATE_dfbetas) ~ norm.distance_sample + norm.days_since + norm.m_w_t_n_n + norm.m_w_t, data=analysis_data)

par(mfrow=c(2, 2))
plot(mod2)

summary(mod2)


# try a glm?
mod3 <- glm(DATE_dfbetas ~ norm.distance_sample + norm.days_since + norm.m_w_t_n_n + norm.m_w_t, 
            family  = Gamma(link = "log"),
            data=analysis_data)

par(mfrow=c(2, 2))
plot(mod3)

summary(mod3)


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

# look at correlation among predictor variables
cor_object <- cor(dplyr::select(analysis_data, median_waiting_time, 
                                duration, Number_of_days_sampled, days_since,
                                dist_km_nn, neighbor_waiting_time))
ggcorrplot(cor_object)


# start with some very simple linear models
# see whether or not a grid was sampled is important
mod <- lm(log(DATE_dfbetas) ~ sampled, data=filter(analysis_data, grid_size==5))

par(mfrow=c(2, 2))
plot(mod)

summary(mod)

# 5 km grid
analysis_data.5 <- analysis_data %>% dplyr::filter(grid_size==5)
# see about median sampling interval
mod5 <- lm(log(DATE_dfbetas) ~ median_waiting_time + duration + Number_of_days_sampled +
            days_since + dist_km_nn + neighbor_waiting_time, data=analysis_data.5)

par(mfrow=c(2, 2))
plot(mod5)

summary(mod5)
anova(mod5)


standard5 <- arm::standardize(mod5)
summary(standard5)


estimates_5 <- as.data.frame(standard5$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `standard5$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(standard5))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(standard5))[,2]) %>%
  mutate(p_value=summary(standard5)$coefficients[,4]) %>%
  mutate(grid_size=5)


# 10 km grid
analysis_data.10 <- analysis_data %>% dplyr::filter(grid_size==10)
# see about median sampling interval
mod10 <- lm(log(DATE_dfbetas) ~ median_waiting_time + duration + Number_of_days_sampled +
             days_since + dist_km_nn + neighbor_waiting_time, data=analysis_data.10)

par(mfrow=c(2, 2))
plot(mod10)

summary(mod10)
anova(mod10)


standard10 <- arm::standardize(mod10)
summary(standard10)


estimates_10 <- as.data.frame(standard10$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `standard10$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(standard10))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(standard10))[,2]) %>%
  mutate(p_value=summary(standard10)$coefficients[,4]) %>%
  mutate(grid_size=10)

# 25 km grid
analysis_data.25 <- analysis_data %>% dplyr::filter(grid_size==25)
# see about median sampling interval
mod25 <- lm(log(DATE_dfbetas) ~ median_waiting_time + duration + Number_of_days_sampled +
              days_since + dist_km_nn + neighbor_waiting_time, data=analysis_data.25)

par(mfrow=c(2, 2))
plot(mod25)

summary(mod25)
anova(mod25)


standard25 <- arm::standardize(mod25)
summary(standard25)


estimates_25 <- as.data.frame(standard25$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `standard25$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(standard25))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(standard25))[,2]) %>%
  mutate(p_value=summary(standard25)$coefficients[,4]) %>%
  mutate(grid_size=25)

# 50 km grid
analysis_data.50 <- analysis_data %>% dplyr::filter(grid_size==50)
# see about median sampling interval
mod50 <- lm(log(DATE_dfbetas) ~ median_waiting_time + duration + Number_of_days_sampled +
              days_since + dist_km_nn + neighbor_waiting_time, data=analysis_data.50)

par(mfrow=c(2, 2))
plot(mod50)

summary(mod50)
anova(mod50)


standard50 <- arm::standardize(mod50)
summary(standard50)


estimates_50 <- as.data.frame(standard50$coefficients) %>%
  rownames_to_column(var="params") %>%
  rename(standard_estimate = `standard50$coefficients`) %>%
  mutate(lwr_conf=as.data.frame(confint(standard50))[,1]) %>%
  mutate(upr_conf=as.data.frame(confint(standard50))[,2]) %>%
  mutate(p_value=summary(standard50)$coefficients[,4]) %>%
  mutate(grid_size=50)



# plot standardized parameter estimates
bind_rows(estimates_5, estimates_10, estimates_25, estimates_50) %>%
  mutate(significant=case_when(
    p_value < 0.5 ~ "Significant",
    p_value > 0.5 ~ "Not-significant"
  )) %>%
  ggplot(., aes(x=params, y=standard_estimate, shape=significant))+
  geom_point()+
  geom_errorbar(aes(ymin=lwr_conf, ymax=upr_conf))+
  coord_flip()+
  facet_wrap(~grid_size)+
  theme_bw()+
  geom_hline(yintercept=0, color="red")+
  xlab("")+
  ylab("Standardized parameter estimate")

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


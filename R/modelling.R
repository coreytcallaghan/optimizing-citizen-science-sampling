### Modelling script

# packages
library(dplyr)
library(ggplot2)
library(BBmisc)
library(ggcorrplot)
library(corrplot)


# load data
analysis_data <- readRDS("Data/Modelling data/analysis_data.RDS")

load("Data/Spatial data/sydney_grids.RData")

lev_results <- readRDS("Data/leverage_results.RDS")

sites_grids_lookup <- readRDS("Data/sites_and_grids_lookup.RDS")

# make df for modelling
hist(analysis_data$DATE_dfbetas, breaks=30)

hist(log(analysis_data$DATE_dfbetas), breaks=30)

# look at correlation among predictor variables
cor_object <- cor(analysis_data[14:17])
ggcorrplot(cor_object)
  
# try a linear model first
# no transform
mod <- lm(DATE_dfbetas ~ norm.distance_sample + norm.days_since + norm.m_w_t_n_n + norm.m_w_t, data=analysis_data)

par(mfrow=c(2, 2))
plot(mod)

summary(mod)

# doesn't look good!
# try a log-transform of response
mod2 <- lm(log(DATE_dfbetas+0.001) ~ norm.distance_sample + norm.days_since + norm.m_w_t_n_n + norm.m_w_t, data=analysis_data)

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


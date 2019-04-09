# AN R script to visualize how leverage 
# behaves with a suite of ebird specific traits


library(ggplot2)
library(dplyr)

lev <- readRDS("Data/leverage_results.RDS")

# date versus leverage
ggplot(lev, aes(x=OBSERVATION_DATE, y=DATE_dfbetas))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

# cook's distance versus dfbeta
ggplot(lev, aes(x=log(DATE_dfbetas), y=log(cooks_dist)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

# number of species versus dfbeta
ggplot(lev, aes(x=Number_species, y=DATE_dfbetas))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

# duration on a checklist versus dfbeta
ggplot(lev, aes(x=DURATION_MINUTES, y=DATE_dfbetas))+
  geom_point()+
  geom_smooth(method="gam")+
  theme_bw()
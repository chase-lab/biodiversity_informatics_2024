

library(tidyverse)

# Data found [here](https://datadryad.org/stash/dataset/doi:10.5061/dryad.rj849k6)

 rest_data <- read.csv("~/Downloads/latdata.csv")

# Let's explore the hierarchy of these data
 head(rest_data)

 # How many papers?
 rest_data %>% select(Citation) %>% distinct()

 # Life forms
 rest_data %>% select(LifeFormBroad) %>% distinct()

 # Trophic levels
 rest_data %>% select(TrophLevel) %>% distinct()

 rest_data %>% select( LifeFormBroad, TrophLevel) %>% distinct()

# response variables
 rest_data %>% select(RV) %>% distinct()

 # disturbances, restoration activities and habitats
 rest_data %>% select(DisturbCat) %>% distinct()
 rest_data %>% select(DisturbCat, AllActivities, ActiveRestor, Control) %>% distinct()


 rest_data %>% select(HabitatCat) %>% distinct()
 rest_data %>% select(HabitatCat, DisturbCat) %>% distinct()


nrow(rest_data)

 ggplot(data=rest_data, aes(x = fct_infreq(DisturbCat))) +
   geom_bar(aes(color = DisturbCat), fill="white", stat='count') +
   theme_classic()










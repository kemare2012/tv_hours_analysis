library(tidyverse)
library(here)

tv_hours_table<-gss_cat %>% 
  filter(age >=30) %>% 
  group_by(marital) %>% 
  summarise(mean_tv_hours = mean(tvhours, na.rm = T))

write.csv(tv_hours_table, here("TV_Hours_by_Marital.csv"))


library(tidyverse)
library(here)
library(statsr)

# Basic table of average TV hours by marital status
tv_hours_table<-gss_cat %>% 
  filter(age >=30) %>% 
  group_by(marital) %>% 
  summarise(mean_tv_hours = mean(tvhours, na.rm = T))

# Create a csv from table
write.csv(tv_hours_table, here("TV_Hours_by_Marital.csv"))

# Plot the results
ggplot(tv_hours_table, aes(x = marital, y = mean_tv_hours))+ 
  geom_bar(stat="identity", fill="cornflowerblue")+
  theme_classic() +
  labs(x = "Marital Status",
       y = "Avg TV hours",
       title = "Average Daily TV-Watching by Marital Status",
       caption = "Source: General Social Survey")+
  coord_flip()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# Table Showing TV hours by Religious Affiliation
relig_summary_table <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

#Plotting tv and religion
ggplot(relig_summary_table, aes(x = relig, y = tvhours, fill=relig))+ 
  geom_bar(stat="identity")+
  theme_classic() +
  labs(x = "Religion",
       y = "Avg TV hours",
       title = "Average Daily TV-Watching by Religious Affiliation",
       caption = "Source: General Social Survey")+
  theme(legend.position = "none")

# Create a Horizontal Chart and Reorder Variables using reorder
ggplot(relig_summary_table, aes(x = reorder(relig, tvhours), y = tvhours))+ 
  geom_bar(stat="identity", fill='steelblue')+
  theme_classic() +
  labs(x = "Religion",
       y = "Avg TV hours",
       title = "Average Daily TV-Watching by Religious Affiliation",
       caption = "Source: General Social Survey")+
  coord_flip()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# Party Affiliation - Combine groups using fct_collapse
party_summary_table<-gss_cat %>% 
  mutate(
    partyid = fct_collapse(partyid,
                           "Other" = c("No answer", "Don't know", "Other party"),
                           "Republican" = c("Strong republican", "Not str republican"),
                           "Independent" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "Democrat" = c("Not str democrat", "Strong democrat")
    )
  ) %>% 
  count(partyid)

#Plotting the data
ggplot(party_summary_table, aes(x = reorder(partyid, n), y = n))+ 
  geom_bar(stat="identity", fill='steelblue')+
  theme_classic() +
  labs(x = "Party Affiliation",
       y = "Number",
       title = "Number of Persons by Party Affiliation",
       caption = "Source: General Social Survey")+
  coord_flip()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

# Calculate Percentages by Party
proportion_table<-party_summary_table  %>% 
  mutate(
    prop = (n / sum(n))*100
  )

#Plotting the data for Population Percentage
ggplot(proportion_table, aes(x = reorder(partyid, prop), y = prop))+ 
  geom_bar(stat="identity", fill='steelblue')+
  theme_classic() +
  labs(x = "Party",
       y = "Percentage of Population",
       title = "Percentage of Population by Party Affiliation",
       caption = "Source: General Social Survey")

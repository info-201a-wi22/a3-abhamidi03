# Author - Atreya Bhamidi
# Date - March 3rd, 2022

library(tidyverse)
library(ggplot2)
library(dplyr)
library(leaflet)
library(maps)

# Loading dataset

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary Information:

# 1. Total Asian and Pacific Islander prison population in 2016 
# 138831/153811 values missing

asian_pacific_prison_pop_2016 <- incarceration_data %>%
  select(year, aapi_prison_pop) %>%
  filter(year == 2016) %>%
  summarize(aapi_pop_16 = sum(aapi_prison_pop, na.rm = TRUE)) %>%
  pull(aapi_pop_16)

#2. Total prison population in 2016 and proportion of Asian 
# and Pacific Islander prison population 

total_prison_pop_2016 <- incarceration_data %>%
  select(year, total_prison_pop) %>%
  filter(year == 2016) %>%
  summarize(total_pop_16 = sum(total_prison_pop, na.rm = TRUE)) %>%
  pull(total_pop_16)

aapi_prison_ratio = asian_pacific_prison_pop_2016/total_prison_pop_2016

aapi_prison_percent = aapi_prison_ratio * 100

#3.  Average of Asian and Pacific Islander prison population from the 
# earliest to the latest year recorded in the dataset

sum_aapi_full <- incarceration_data %>%
  summarize(aapi_sum = sum(aapi_prison_pop, na.rm = TRUE)) %>%
  pull(aapi_sum)

aapi_avg = sum_aapi_full/(2018 - 1970)

#4. Location and year of maximum AAPI prison population 


aapi_subset <- incarceration_data %>%
  select(yfips, year, fips, state, county_name, total_pop, aapi_pop_15to64,
        total_jail_pop, total_prison_pop, aapi_prison_pop, aapi_jail_pop, 
        aapi_male_prison_pop, aapi_female_prison_pop, aapi_jail_pop_rate) %>%
  mutate("county_location" = paste0(county_name, ", ", state))



max_aapi_prison_county <- aapi_subset %>%
  arrange(desc(aapi_prison_pop)) %>%
  slice_head() %>%
  pull(county_location)

max_aapi_year <- aapi_subset %>%
  arrange(desc(aapi_prison_pop)) %>%
  slice_head() %>%
  pull(year)

#5. Location with maximum AAPI 15-64 population in that same year (2015)

max_aapi_county <- aapi_subset %>%
  filter(year == 2015) %>%
  arrange(desc(aapi_pop_15to64)) %>%
  slice_head() %>%
  pull(county_location)


# Converting summary data into a dataframe

Statistic <- c("Total Asian and Pacific Islander prison population in 2016 ", 
                   "Total prison population in 2016",
                   "Proportion of Asian and Pacific Islander prison population",
                   "Average of Asian and Pacific Islander prison population from 1970 to 2018",
                   "Location with the highest AAPI prison population",
                   "Year during which this highest AAPI prison population was recorded",
                   "Location with maximum AAPI population in that year")

Value <- c(asian_pacific_prison_pop_2016, total_prison_pop_2016, aapi_prison_ratio,
                    aapi_avg, max_aapi_prison_county, max_aapi_year, max_aapi_county)
                    
                  
                   
summary_vars <- data.frame(Statistic, Value)


                   
# Trends over Time - comparing AAPI prison population rates over time for the 9 counties with the highest AAPI population (as measured in 2018)
# Honolulu County is dropped as there are no recorded values of AAPI prison population rates



counties_max_aapi_pop <- aapi_subset %>%
  filter(year == max(year)) %>%
  arrange(desc(aapi_pop_15to64)) %>%
  slice(1:10) %>%
  pull(county_location)

  
trends_df <- aapi_subset %>%
  select(year, county_location, aapi_jail_pop_rate) %>%
  filter(county_location %in% counties_max_aapi_pop) %>%
  filter(!is.na(aapi_jail_pop_rate)) %>%
  rename(County = county_location)
  
trend_location_no_na <- unique(trends_df[2])


trend_graph <- ggplot(trends_df) +
  geom_line(aes(x = year, y = aapi_jail_pop_rate, group = County, color = County)) +
  labs(title = "AAPI Jail Population Proportion in top 9 AAPI-Populated Counties", x = "Year", y = "Jail Proportion")
  


# Variable comparison chart - Investigating the relationship between AAPI population and prison population in King County, where the UW is located, from 1990 to 2016.

var_comp_df <- aapi_subset %>%
  select(year, county_location, aapi_pop_15to64, aapi_prison_pop) %>%
  filter(county_location == "King County, WA") %>%
  filter(!is.na(aapi_pop_15to64)) %>%
  filter(!is.na(aapi_prison_pop))

var_comp_graph <- ggplot(var_comp_df) +
  geom_smooth(aes(x = aapi_pop_15to64, y = aapi_prison_pop)) +
  labs(title = "AAPI Population vs Prison Population, King County 1990 - 2016", x = "AAPI Population", y = "AAPI Prison Population")

# Map - Tracking AAPI Prison Population rates in counties across the United States in 2018

map_df <- aapi_subset %>%
  filter(year == 2018) %>%
  select(fips, county_name, aapi_jail_pop_rate)%>%
  filter(!is.na(aapi_jail_pop_rate))
  
county_df <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

aapi_map_df <- county_df %>%
  left_join(map_df, by = "fips")

blank_theme <- theme_bw() +
  theme(axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())


aapi_jail_pop_rate_2018 <- ggplot(aapi_map_df) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = aapi_jail_pop_rate),
               color = "black", size = 0.2) + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(aapi_map_df$aapi_jail_pop_rate)), 
                        na.value = "white", low = "green", high = "red") +
  blank_theme +
  labs(title = "AAPI Jail Population Rate in the USA in 2018")


# ################################################################ --------
# This script analyses the wind data at the 5sites
# steps on changing the date 
#
# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)

# Loading the SACTN data --------------------------------------------------
load("~/Honours/R assignment/Honours.P/SACTNdaily_v4.1.Rdata")
temp <- as_tibble(SACTNdaily_v4.1)
temp.mean <- temp %>%
  group_by(site, src) %>%
  mutate(zeroed.temp = temp - mean(temp, na.rm = TRUE)) %>%
  ungroup()
temp.mean


temp.mean$year <- format(temp.mean$date,"%Y")
temp.mean$month <- format(temp.mean$date,"%b")
temp.mean$month <- factor(temp.mean$month, levels = c("Jan","Feb","Mar","Apr",
                                                      "May","Jun","Jul","Aug",
                                                      "Sep","Oct","Nov","Dec"))

after2001 <- filter(temp.mean, date >= as.Date("2001-12-31"))

site_list <- after2001 %>%
  group_by(site) %>%
  summarise(count = length(unique(src))) %>%
  filter(count > 1) %>%
  ungroup()

# extract only locations with multiple time series

site_dates <- after2001 %>%
  filter(site %in% site_list$site) %>%
  group_by(site, src) %>%
  mutate(start.date = min(date),
         end.date = max(date)) %>%
  group_by(site) %>%
  mutate(min.start.date = max(start.date),
         max.end.date = min(end.date)) %>%
  ungroup()

min_max_time <- site_dates %>%
  group_by(site, src) %>%
  filter(date >= min.start.date,
         date <= max.end.date) %>%
  ungroup()

site_list_final <- min_max_time %>%
  group_by(site) %>%
  summarise(count = length(unique(src))) %>%
  filter(count > 1) %>%
  ungroup()

final_time <- min_max_time %>%
  filter(site %in% site_list_final$site)

year <- final_time %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date)) %>%
  filter(site %in% c("Ballito", "Hout Bay", "Mossel Bay", "Sodwana", "Knysna")) %>%
  filter(year == 2005 & month != "Dec")

year_bonus <- final_time %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date)) %>%
  filter(site %in% c("Ballito", "Hout Bay", "Mossel Bay", "Sodwana", "Knysna")) %>%
  filter(year == 2004 & month == "Dec")

year_clean <- rbind(year, year_bonus)


# working with the wind data for the various sites
# Loading the wind data for the sites
# need to do this for all five sites

# Loading the data --------------------------------------------------------

Ballito_2005 <- read_csv("Wind data/Ballito_2005.csv")
Hout_Bay_2005 <- read_csv("Wind data/Hout Bay_2005.csv")
Knysna_2005 <- read_csv("Wind data/Knysna_2005.csv")
Mossel_Bay_2005 <- read_csv("Wind data/Mossel Bay_2005.csv")
Sodwana_2005 <- read_csv("Wind data/Sodwana_2005.csv")


# Ballito -----------------------------------------------------------------

Ballito_2005 <- read_csv("Wind data/Ballito_2005.csv") %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd), 
              mean_dir = mean(wndDir))

Ballito_2005$year <- format(Ballito_2005$date,"%Y")
Ballito_2005$month <- format(Ballito_2005$date,"%b")
Ballito_2005$month <- factor( Ballito_2005$month, levels = c("Jan","Feb","Mar","Apr",
                                                                            "May","Jun","Jul","Aug",
                                                                            "Sep","Oct","Nov","Dec"))
  
sites_wind_B <- merge(Ballito_2005, year_clean, by = "site")

# Create function ---------------------------------------------------------
#{}logic

new_function <- function(df){
  df_2 <- df + 1
  return(df_2)
}

new_function(20)

#directory with wind files
#path "

dir("Wind data/")

load_wind <- function(wind_file){
  wind_2005 <- read_csv(wind_file) %>% 
    mutate(date = as.Date(as.character(date), format = "%Y%m%d"), 
           site = sapply(strsplit(sapply(strsplit(as.character(wind_file), "/"), "[[", 2), "_"), "[[", 1))   %>% 
    group_by(site, date) %>% 
    summarise(mean_speed = mean(wndSpd), 
              mean_dir = mean(wndDir))
  
  wind_2005$year <- format(wind_2005$date,"%Y")
  wind_2005$month <- format(wind_2005$date,"%b")
  wind_2005$month <- factor( wind_2005$month, levels = c("Jan","Feb","Mar","Apr",
                                                               "May","Jun","Jul","Aug",
                                                               "Sep","Oct","Nov","Dec"))
  return(wind_2005)
}

test <- load_wind("Wind data/Ballito_2005.csv")

# feed as vector
test2 <- map_dfr(dir("Wind data/", full.names = TRUE), load_wind)

# merging wind and temp ---------------------------------------------------

wind_temp <- left_join(test2, select(year_clean, date, site, temp))

# scatter
ggplot(wind_temp, aes(x = mean_speed, y = temp, colour = site)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)





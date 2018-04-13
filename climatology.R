# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)

# Loading SACTN data ------------------------------------------------------
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

# Climatology -------------------------------------------------------------
#climatologies- always use raw data
# Getting the climatology (using the daily data)- monthly

temp_monthly <- year_clean %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            range_temp = range(temp, na.rm = TRUE)[2]-range(temp, na.rm = TRUE)[1],
            sd_temp = sd(temp, na.rm = TRUE)) %>%
  filter(date %in% c("Jan","Feb","Mar","Apr",
                     "May","Jun","Jul","Aug",
                     "Sep","Oct","Nov","Dec")) %>% 
  ungroup()

# Annual climatology 

temp_annually <- year_clean %>% 
  # mutate(date = lubridate::year(date)) %>%
  mutate(date = "Annual") %>%
  group_by(site, date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            range_temp = range(temp, na.rm = TRUE)[2]-range(temp, na.rm = TRUE)[1],
            sd_temp = sd(temp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(site) %>% 
  select(site, date, everything())

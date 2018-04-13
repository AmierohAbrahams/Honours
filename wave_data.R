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


# Creating a function -----------------------------------------------------
load_wave <- function(wave_file){
  # tester
  # wave_file <- "wave/Ballito_dir_wave.csv"
  #
  wave_data <- read_csv(wave_file) %>% 
    mutate(date = lubridate::ymd(date),
           site = sapply(strsplit(sapply(strsplit(as.character(wave_file), "/"), 
                                         "[[", 2), "_"), "[[", 1)) #%>% 
    # group_by(site, date) %>% 
    return(wave_data)
}

# tab- project folder and finds- tab in "
# test <- map_dfr(dir("wave/", full.names = TRUE)[1], load_wave)

dir_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "dir"), load_wave)
wavedir_temp <- left_join(dir_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

per_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "per"), load_wave)
waveper_temp <- left_join(per_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

swh_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "swh"), load_wave)
waveswh_temp <- left_join(swh_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

wind_wave <- map_dfr(dir("wave/", full.names = TRUE, pattern = "wind"), load_wave)
wavewind_temp <- left_join(wind_wave, select(year_clean, date, site, temp)) %>% 
  filter(date >= as.Date("2005-01-01"))

final_wave <- left_join(wavedir_temp,waveper_temp,waveswh_temp,wavewind_temp, 
                        select(year_clean, date, site, temp))




#Seasons:Monthly
dirs_temps_monthly <- dir_wave %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(dir_min = min(dir, na.rm = TRUE),
            dir_max = max(dir, na.rm = TRUE),
            dir_range = range(dir, na.rm = TRUE)[2]-range(dir, na.rm = TRUE)[1],
            dir_median = median(dir, na.rm = TRUE), 
            dirw_min = min(dirw, na.rm = TRUE),
            dirw_max = max(dirw, na.rm = TRUE),
            dirw_range = range(dirw, na.rm = TRUE)[2]-range(dirw, na.rm = TRUE)[1],
            dirw_median = median(dirw, na.rm = TRUE)) %>%
  filter(date %in% c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>% 
  ungroup()

#Seasons:Annually

dirs_annual <- dir_wave %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(site, date) %>% 
  summarise(dir_min = min(dir, na.rm = T),
            dir_max = max(dir, na.rm = T),
            dir_range = range(dir, na.rm = T)[2]-range(dir, na.rm = T)[1],
            dir_median = median(dir, na.rm = T), 
            dirw_min = min(dirw, na.rm = T),
            dirw_max = max(dirw, na.rm = T),
            dirw_range = range(dirw, na.rm = T)[2]-range(dirw, na.rm = T)[1],
            dirw_median = median(dirw, na.rm = T)) %>% 
  ungroup() %>% 
  select(-date) %>% 
  group_by(site) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  mutate(date = "Annually") %>% 
  select(site, date, everything())



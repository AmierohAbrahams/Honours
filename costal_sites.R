#####################################################################
####5sites along the coast
####divided by seasons
####focusing on the year 2005 only
####Excluding Port Nolloth
######################################################################

# Loading the packages ----------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)

# Loading the data --------------------------------------------------------
load("~/Honours/R assignment/Honours.P/SACTNdaily_v4.1.Rdata")
# for each combination of site and src, calculate the mean temperature and subtract it
# from the daily temperatures
temp <- as_tibble(SACTNdaily_v4.1)
temp.mean <- temp %>%
  group_by(site, src) %>%
  mutate(zeroed.temp = temp - mean(temp, na.rm = TRUE)) %>%
  ungroup()
temp.mean

# Adds a column for the year
temp.mean$year <- format(temp.mean$date,"%Y")

#Adds a column for the month
temp.mean$month <- format(temp.mean$date,"%b")

#
temp.mean$month <- factor(temp.mean$month, levels = c("Jan","Feb","Mar","Apr",
                                                      "May","Jun","Jul","Aug",
                                                      "Sep","Oct","Nov","Dec"))
# All the sites after 2001

after2001 <- filter(temp.mean, date >= as.Date("2001-12-31"))

# create an index by site and source
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
# seasonality
summer <- c("Dec", "Jan", "Feb")

summer1 <- year_clean %>%
  filter(month %in% summer) %>%
  mutate(season = "summer")

autumn <- c("Mar", "Apr", "May")

autumn1 <- year_clean %>%
  filter(month %in% autumn) %>%
  mutate(season = "autumn")

winter <- c("Jun","Jul", "Aug")

winter1 <- year_clean %>%
  filter(month %in% winter) %>%
  mutate(season = "winter")

spring <- c("Sep", "Oct", "Nov")

spring1 <- year_clean %>%
  filter(month %in% spring) %>%
  mutate(season = "spring")

# Binding the four seasons found in the year 2005
# Use rbind as this combines the data bellow eachother where c bind - binds the data next to eachother
all1 <- rbind(summer1, autumn1, winter1, spring1)

##### Faceting by season

sites <- c("Ballito", "Knysna","Sodwana", "Mossel Bay", "Hout Bay")

Summer <- summer1 %>%
  filter(site %in% sites)

Su1 <- ggplot(Summer, aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", size = 0.4))

Spring <- spring1 %>%
  filter(site %in% sites)

Sp1 <- ggplot(Spring, aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", size = 0.4))

Winter <- winter1 %>%
  filter(site %in% sites)

Wi1 <- ggplot(Winter, aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", size = 0.4))

Autumn <- autumn1 %>%
  filter(site %in% sites)

Au1 <- ggplot(Autumn, aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", size = 0.4))

final_graph <- ggarrange(Su1, Au1, Wi1, Sp1, ncol = 4, nrow = 1, common.legend = TRUE, align = "hv")
final_graph
ggsave(final_graph, filename = "final_graph.png")




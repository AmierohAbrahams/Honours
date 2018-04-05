# ################################################################ --------
# The purpose of this script is to find the sites that overlap in time
# 8 of the sites overlap
# Only working with sites that overlap in the year 2005 and 2016
# Calculating mean temp and subtracting it from the daily temp

# Loading the packages ----------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)

# Loading the data --------------------------------------------------------

load("SACTNdaily_v4.1.Rdata")


# Renaming the dataset ----------------------------------------------------

temp <- as_tibble(SACTNdaily_v4.1)

# for each combination of site and src, calculate the mean temperature and subtract it
# from the daily temperatures

temp.mean <- temp %>%
  group_by(site, src) %>%
  mutate(zeroed.temp = temp - mean(temp, na.rm = TRUE)) %>%
  ungroup()
temp.mean

# Graph theme
windowsFonts(Helvetica=windowsFont("TT Helvetica"))
theme_new <- function(base_size = 12, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      #axis.title = element_text(size = 14),
      #axis.text = element_text(colour="black", size=8),
      #strip.text = element_text(size=12),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white", colour = "black"),
      strip.background = element_rect(fill = NA)
    )
}

# plot the zeroed temperatures;
# create a facet for each site, and colour-code the src
plot1 <- ggplot(temp.mean, aes(x = date, y = zeroed.temp)) +
  geom_line(aes(colour = src), size = 0.2) +
  scale_y_continuous(breaks = c(0, 10))+
  facet_wrap(~site, ncol = 5) +
  xlab(NULL) + ylab("Temperature (°C)") + ggtitle("Zero centered coastal water temperature") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot1

ggsave(plot1, filename = "plot1.png")

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

# create plots of overlapping time by site

sites8 <- ggplot(final_time,aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_wrap(~site) +
  labs(x = "Year", y = "Temperature (°C)")  +
  theme_new()
sites8

ggsave(sites8, filename = "sites8.png")

# selected 1 year(2005)
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

year_PN <- final_time %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date)) %>%
  filter(site == "Port Nolloth" & year == 2016 & month != "Dec")

year_PN_bonus <- final_time %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date)) %>%
  filter(site == "Port Nolloth" & year == 2015 & month == "Dec")

year_clean <- rbind(year, year_bonus, year_PN, year_PN_bonus)

# Divide this one year into the four seasons
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

all1$season <- factor(all1$season, levels = c("summer", "autumn", "winter", "spring"))


# Graph showwing all the sites and seasons

ggplot(all1, aes(x = as.Date(date), y = temp)) +
  geom_line(aes(colour = src)) +
  facet_grid(site ~ season) +
  labs(x = "Year", y = "Temperature (°C)") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "2 years") +
  theme_new() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting with all sites except Port Nolloth

sites <- c("Ballito", "Knysna", "Sodwana", "Mossel Bay", "Hout Bay")

sites3 <- all1 %>%
  filter(site %in% sites)

one <- ggplot(sites3, aes(x = as.Date(date), y = temp)) + ## all sites except PN
  geom_line(aes(colour = src)) +
  facet_grid(site~season) +
  labs(x = "Year", y = "Temperature (°C)") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "2 months") + ## for consistant x-axis, can be adjusted
  theme_new() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
one

# Plotting only Port Nolloth

sitesPN <- c("Port Nolloth")

sites4 <- all1 %>%
  filter(site %in% sitesPN)

two <- ggplot(sites4, aes(x = as.Date(date), y = temp)) + ## Only PN
  geom_line(aes(colour = src)) +
  facet_grid(site~season) +
  labs(x = "Year", y = "Temperature (°C)") +
  scale_x_date(labels = date_format("%m-%Y"), date_breaks = "2 months") +
  theme_new() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
two

ggarrange(one, two) ## Combining all sites and PN into one image

##### Faceting by season

sites <- c("Ballito", "Knysna","Sodwana", "Mossel Bay", "Hout Bay")

Summer <- summer1 %>%
  filter(site %in% sites)

Su1 <-ggplot(Summer, aes(x = date, y = temp)) +
  geom_line(aes(colour = src)) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme_new()

Spring <- spring1 %>%
  filter(site %in% sites)

Sp1 <- ggplot(Spring, aes(x = date, y = temp)) +
  geom_line(aes(colour = src)) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) +
  theme_new()

Winter <- winter1 %>%
  filter(site %in% sites)

Wi1 <- ggplot(Winter, aes(x = date, y = temp)) +
  geom_line(aes(colour = src)) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) +
  theme_new()

Autumn <- autumn1 %>%
  filter(site %in% sites)

Au1 <- ggplot(Autumn, aes(x = date, y = temp)) +
  geom_line(aes(colour = src)) +
  facet_grid(site ~ season, scales = "free_x") +
  labs(x = "Year", y = NULL) +
  theme_new() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(Su1, Au1, Wi1, Sp1, ncol = 4, nrow = 1, common.legend = TRUE, align = "hv")




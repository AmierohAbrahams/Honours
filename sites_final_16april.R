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

temp.mean$year <- format(temp.mean$date,"%Y")
temp.mean$month <- format(temp.mean$date,"%b")
temp.mean$month <- factor(temp.mean$month, levels = c("Jan","Feb","Mar","Apr",
                                                      "May","Jun","Jul","Aug",
                                                      "Sep","Oct","Nov","Dec"))
site_list <- temp.mean %>%
  group_by(site) %>%
  summarise(count = length(unique(src))) %>%
  filter(count > 1) %>%
  ungroup()


site_dates <- temp.mean %>%
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

year <- min_max_time %>%
  filter(site %in% site_list_final$site)
  

final_graph <- ggplot(year, aes(x = date, y = temp)) +
  geom_line(aes(colour = src), alpha = 0.5) +
  facet_wrap(~site, scales = "free_x", ncol= 2) +
  labs(x = "Year", y = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", size = 0.4))

final_graph
ggsave(final_graph, filename = "final_graph16april.png")
###################################################
 dfnew <- match(subset(df, station=="ALWA")




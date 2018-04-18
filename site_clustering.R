# site_clustering.R
# This script creates a new site list
# by clustering the original site list by statistical properties
# April 18th, 2018


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(zoo)
library(lubridate)

# Load data ---------------------------------------------------------------

load("site_list_v4.2.RData")


# Perform clustering ------------------------------------------------------

# Neat
kmeans(site_list[,c(15, 18, 19)], 3)$cluster


# Visualise different cluster options -------------------------------------

clust_columns <- site_list[,c(15:19)]

clust_i <- function(i) {
  ggplot(data = site_list, 
                  aes(x = lon, y = lat, 
                      colour = as.factor(kmeans(clust_columns, i)$cluster))) +
  borders() +
  geom_point() +
  labs(colour = "cluster") +
  coord_equal(xlim = c(15, 35), ylim = c(-37, -27)) +
  ggtitle(paste0("clust = ", i))
}

clust_6 <- clust_i(6)
clust_5 <- clust_i(5)
clust_4 <- clust_i(4)
clust_3 <- clust_i(3)

ggarrange(clust_6, clust_5, clust_4, clust_3, common.legend = T)


# Cluster sites by k-means results ----------------------------------------

load("SACTN_data/SACTN_daily_v4.2.RData")

# Create cluster index
site_list$cluster <- as.factor(kmeans(site_list[,c(15, 18:19)], 4)$cluster)

SACTN_daily_clusters <- left_join(SACTN_daily_v4.2, site_list[,c(4, 13, 21)]) %>% 
  filter(length >= 3650 * 3) 
# Here adjusted to 30 years, which works with the KZNSB data;
# use a shorter time for sites along other coastlines

SACTN_clust_1 <- SACTN_daily_clusters %>% 
  filter(cluster == 1) %>% 
  select(-length) %>% 
  droplevels()

# How many sites in this cluster?
length(unique(SACTN_clust_1$index)) 
# 42 of them if time series are 10 years or less; 
# or 33 if lengths of 30 years are selected...
# This might have to be adjusted depending on the coastline 
# we are working on

# This code below takes only a few minutes on my computer (AJS)...
# I now also wrapped it in an argument-less function...
match_sites <- function() {
  SACTN_clust_1_match <- data.frame()
  for (i in 1:length(levels(SACTN_clust_1$index))) {
    # for (i in 1:10) {
    SACTN_df_1 <- filter(SACTN_clust_1, index == levels(index)[i])
    for (j in 1:length(levels(SACTN_clust_1$index))) {
      # for(j in 1:10) {
      if (i == j) {
        next
      }
      if (j < i) {
        next
      }
      SACTN_df_2 <- filter(SACTN_clust_1, index == levels(index)[j])
      SACTN_df_3 <-
        left_join(SACTN_df_1, SACTN_df_2, by = "date") %>%
        na.trim() %>%
        select(date,
               index.x,
               temp.x,
               index.y,
               temp.y,
               -cluster.x,
               -cluster.y) %>%
        rename(
          index_1 = index.x,
          temp_1 = temp.x,
          index_2 = index.y,
          temp_2 = temp.y
        ) %>%
        mutate(index_pair = paste0(index_1, " - ", index_2))
      SACTN_clust_1_match <- rbind(SACTN_clust_1_match, SACTN_df_3)
    }
  }
}

# Run the function, and also time it for interest sake...
system.time(match_sites())
# On my computer (AJS)... time in seconds
# > system.time(match_sites())
# user  system elapsed 
# 394.857 145.019 541.344 
# this is about 9 minutes

# Create paired legitimate difference values  -----------------------------

SACTN_clust_1_legit <- SACTN_clust_1_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

# Visualise results -------------------------------------------------------

# Paired values

ggplot(data = SACTN_clust_1_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Legit mean values
# I plot only the sites that are paired with "Glenmore/KZNSB -"
# We need the stringr package for that
# Also, for the KZNSB data we have time series of >30 years, and these are the ones
# plotted here...
library(stringr)
SACTN_clust_1_legit %>% 
  filter(str_detect(index_pair, "Glenmore/KZNSB -")) %>% 
  ggplot(aes(x = year, y = temp_mean_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7, show.legend = F) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair), show.legend = F)  +
  facet_wrap(~month) + 
  theme_pubclean()
# Referring to the plot above: since the KZNSB data series are very long, we can
# have time series of 30 years in length;
# I'd also calculate a column with the distances between site pairs, and then
# colour the lines as a function of the site-site distances in this column

# Legit SD values
ggplot(data = SACTN_clust_1_legit, 
       aes(x = year, y = temp_sd_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair))  +
  facet_wrap(~month)

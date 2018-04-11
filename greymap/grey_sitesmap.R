# Loading the packages ----------------------------------------------------
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(GGally)
library(ggpubr)
library(ggrepel)

# Loading the datasets ----------------------------------------------------
load("~/Honours/R assignment/Honours.P/south_africa_coast.RData")
site_coordinates_new <- read_csv("site_coordinates_new.csv")
load("~/Honours/R assignment/Honours.P/sa_provinces.RData")


ggplot(data = site_coordinates_new, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = site_coordinates_new)  +
  geom_label_repel(data =site_coordinates_new, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())



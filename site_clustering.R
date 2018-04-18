# site_clustering.R
# This script creates a new site list
# by clustering the original site list by statistical properties
# April 18th, 2018


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(FNN)


# Load data ---------------------------------------------------------------

load("site_list_v4.2.RData")


# Perform clustering ------------------------------------------------------

# Neat
kmeans(site_list[,c(15, 18, 19)], 3)$cluster


# Visualise different cluster options -------------------------------------

clust_columns <- site_list[,c(15:19)]

clust_i <- function(i){
  ggplot(data = site_list, 
                  aes(x = lon, y = lat, 
                      colour = as.factor(kmeans(clust_columns, i)$cluster))) +
  borders() +
  geom_point() +
  labs(colour = "cluster") +
  coord_cartesian(xlim = c(15, 35), ylim = c(-37, -27)) +
  ggtitle(paste0("clust = ", i))
}

clust_6 <- clust_i(6)
clust_5 <- clust_i(5)
clust_4 <- clust_i(4)
clust_3 <- clust_i(3)

ggarrange(clust_6, clust_5, clust_4, clust_3, common.legend = T)


# Cluster sites by k-means results ----------------------------------------

load("SACTNdaily_v4.1.Rdata")

# Create cluster index
site_list$cluster <- as.factor(kmeans(site_list[,c(15:19)], 5)$cluster)

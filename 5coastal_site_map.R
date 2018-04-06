#####################################################################
####Mapping the 5sites
####
######################################################################

#Loading the packages
library(tidyverse)
library(ggmap)
library(maps)

#Loading the data
site_location <- read_csv("site_coordinates.csv")

#Getting the map
sa <- get_map(location = 'South Africa', zoom = 5, maptype = "hybrid")
sa

sa1 <- ggmap(sa) +
  geom_point(data = site_location, aes(x = lon , y = lat ), 
             colour = "red", size =  2, alpha=0.5) +
  labs(y = "Latitude(°E)", x = "Longitude(°S)", title = "Location of the sites along the coast of South Africa")
sa1

Mossel_Bay <- data.frame(lat=c(-34.144076), lon = c(22.124972), Site = "Mossel Bay")

sa2 <- sa1 +
  geom_text(data = site_location,
            aes(lon , lat , label = Site), 
            hjust = 0.03, vjust = -1, 
            size = 4, colour = "white") +
  geom_point(data=Mossel_Bay, aes(x=lon, y=lat), color="red", size=2, alpha=0.5) + 
  geom_text(data = Mossel_Bay, aes(x=lon, y=lat, label=Site),
            size = 4, vjust = 1,
            hjust = 1, colour = "white") +
  theme_bw()+
  coord_cartesian()

ggsave(sa2, filename = "5sites_map.png")

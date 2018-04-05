# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggmap)
library(maps)
library(ggplot2)



# Loading the data --------------------------------------------------------
# Final map

site_loc <- read_csv("~/Honours/R assignment/Honours.P/final3_site_coordinates.csv")
sa <- get_map(location = 'South Africa', zoom = 5, maptype = "hybrid")

sa

sa1 <- ggmap(sa) +
  geom_point(data = site_loc, aes(x = lon , y = lat ), 
             colour = "red", size =  2, alpha=0.5) +
  labs(y = "Latitude(°E)", x = "Longitude(°S)", title = "Location of one site along each of the coasts")
sa1

Hout_Bay <- data.frame(lat=c(-34.049890), lon = c( 18.354964), Site = "Hout Bay")



sites3 <- sa1 +
  geom_text(data = site_loc,
            aes(lon , lat , label = Site), 
            hjust = 0.5, vjust = -1, 
            size = 4, colour = "black")+ 
  geom_point(data=Hout_Bay, aes(x=lon, y=lat), color="red", size=2, alpha=0.5) + 
  geom_text(data = Hout_Bay, aes(x=lon, y=lat, label=Site),
            size = 4, vjust = 1.5,
            hjust = 0.5, colour = "black")
sites3
ggsave(sites3, filename = "sites3.png")

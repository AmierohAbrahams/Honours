# Creating map showing the project locations

# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggmap)
library(maps)
library(ggplot2)


# Loading the data --------------------------------------------------------
# Final map

site_location <- read_csv("site_coordinates.csv")

sa <- get_map(location = 'South Africa', zoom = 5, maptype = "hybrid")
sa

sa1 <- ggmap(sa) +
  geom_point(data = site_location, aes(x = lon , y = lat ), 
             colour = "red", size =  2, alpha=0.5) +
  labs(y = "Latitude(°E)", x = "Longitude(°S)", title = "Location of the sites along the South African coast")

Tsitsikamma <- data.frame(lat=c(-34.023150), lon = c(23.857731), Site = "Tsitsikamma")

Aliwal_Shoal <- data.frame(lat=c(-30.209897), lon = c(30.802942), Site = "Aliwal Shoal")

Mossel_Bay <- data.frame(lat=c(-34.144076), lon = c(22.124972), Site = "Mossel Bay")


sa2 <- sa1 +
  geom_text(data = site_location,
            aes(lon , lat , label = Site), 
            hjust = 0.5, vjust = -1, 
            size = 3, colour = "black") + 
  geom_point(data=Tsitsikamma, aes(x=lon, y=lat), color="red", size=2, alpha=0.5) + 
  geom_text(data = Tsitsikamma, aes(x=lon, y=lat, label=Site),
            size = 3, vjust = 1.5,
            hjust = 0.01, colour = "black") +
  geom_point(data=Aliwal_Shoal, aes(x=lon, y=lat), color="red", size=2, alpha=0.5) + 
  geom_text(data = Aliwal_Shoal, aes(x=lon, y=lat, label=Site),
            size = 3, vjust = 1,
            hjust = 0.5, colour = "black") +
  geom_point(data=Mossel_Bay, aes(x=lon, y=lat), color="red", size=2, alpha=0.5) + 
  geom_text(data = Mossel_Bay, aes(x=lon, y=lat, label=Site),
            size = 3, vjust = 1,
            hjust = 1, colour = "black") +
  theme_bw()+
  coord_cartesian()


sa2






# ignore

# --------------------------------------------------------- ---------------

Knysna <- get_map(location = c(lon = 23.061299, lat = -34.085496),
                   zoom = 10, maptype = 'satellite')


ggmap(Knysna)

Knysna1 <- ggmap(Knysna) +
  geom_point(data = site_location, aes(x = lon , y = lat ), 
             colour = "red", size =  2.5) +
  labs(x = "Latitude(°E)", y = "Longitude(°S)", title = "Knysna") 

Knysna2 <- Knysna1 +
  annotate("text", label = "23.061299, -34.085496", 
           x = 23.061299, y = -34.085496, 
           size = 3, colour = "salmon") +
  theme_void()+
  coord_cartesian()

Knysna2


# ------------------------------------------------ ------------------------

final <- sa2 + 
  annotation_custom(grob = ggplotGrob(Knysna2),
                    xmin = 18.5, xmax = 18.7,
                    ymin = -33.82, ymax = -34) 
  

ggsave(final, filename = "final_sitemap.png")



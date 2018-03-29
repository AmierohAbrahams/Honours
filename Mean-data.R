# find the mean of all the temps.
# subtract it from the actual temp
# Plot it on a graph
# find three overlapping sites in the west,east and south coast
# center around zero

library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)

load("~/Honours/R assignment/Honours.P/SACTNdaily_v4.1.Rdata")

temp <- SACTNdaily_v4.1


Meantemp <- temp %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE))
Meantemp


## subtracting averages

Ave <- 18.6 ## taken from Meantemp

SubtractedMean <- temp$temp - Ave
SubtractedMean

## adding to previous dataframe

Newtemp <- cbind(temp, SubtractedMean)
head(Newtemp)

Newtemp

plot1 <- ggplot(temp, aes(x = date, y = temp)) +
  geom_line(col = "grey25", size = 0.2) +
  geom_line(data = Newtemp, aes(x = date, y = SubtractedMean), col = "deepskyblue3",
            alpha = 1, size = 0.5) +
  xlab(NULL) + ylab("Temperature") + ggtitle("Raw data")


# ------------- -----------------------------------------------------------




temp <- as_tibble(SACTNdaily_v4.1) # because I like tibbles

# for each combination of site and src, calculate the mean temperature and subtract it
# from the daily temperatures
temp.mean <- temp %>%
  group_by(site, src) %>%
  mutate(zeroed.temp = temp - mean(temp, na.rm = TRUE)) %>%
  ungroup()
temp.mean

# plot the zeroed temperatures;
# create a facet for each site, and colour-code the src
plot1 <- ggplot(temp.mean, aes(x = date, y = zeroed.temp)) +
  geom_line(aes(colour = src), size = 0.2) +
  scale_y_continuous(breaks = c(0, 10))+
  facet_wrap(~site) +
  xlab(NULL) + ylab("Temperature (°C)") + ggtitle("Zero centered coastal water temperature")
  
plot1


# --------- ---------------------------------------------------------------


trial1 <-temp.mean %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(site, year) %>%
  summarise(average_temp = median(zeroed.temp, na.rm = TRUE))

summary(aov(data = trial1, average_temp ~ site))

final <- ggplot(data = trial1, aes(x = year, y = average_temp)) +
  geom_line(aes(group = site, colour = "red")) +
  facet_wrap(~site, ncol = 5) +
  labs(x = "Year", y = "Temperature(C)")

final


# ------------------- -----------------------------------------------------
temp.mean1 <- temp %>%
  group_by(site, src) %>%
  mutate(year = format(date, "%Y")) %>%
  mutate(zeroed.temp = temp - mean(temp, na.rm = TRUE)) %>%
  ungroup()
temp.mean1


plot1.1 <- ggplot(temp.mean1, aes(x = year, y = zeroed.temp)) +
  geom_line(aes(colour = src), size = 0.2) +
  scale_y_continuous(breaks = c(0, 10))+
  facet_wrap(~site) +
  xlab(NULL) + ylab("Temperature (°C)") + ggtitle("Zero centered coastal water temperature")

plot1.1


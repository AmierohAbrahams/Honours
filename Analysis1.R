library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)
load("~/Honours/R assignment/Honours.P/SACTNdaily_v4.1.Rdata")
temp <- SACTNdaily_v4.1

temp1 <- temp %>%
  filter(src == c("DEA", "SAWS", "UWC"))
colnames(temp1)

summary(aov(data = temp1, temp ~ site))
ggplot(temp1, aes(x = date, y = temp)) +
  geom_line(aes(group = site, colour = "red")) +
  facet_wrap(~site, ncol = 5) +
  labs(x = "Year", y = "Temperature(C)")

trial1 <- temp1 %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(site, year) %>%
  summarise(average_temp = mean(temp, na.rm = TRUE))

summary(aov(data = trial1, average_temp ~ site))

final <- ggplot(data = trial1, aes(x = year, y = average_temp)) +
  geom_line(aes(group = site, colour = "red")) +
  facet_wrap(~site, ncol = 5) +
  labs(x = "Year", y = "Temperature(C)")

final


# -------- ----------------------------------------------------------------

load("~/Honours/R assignment/Honours.P/SACTNdaily_v4.1.Rdata")
temp <- SACTNdaily_v4.1

ggplot(temp, aes(x = date, y = temp)) +
  geom_line(aes(group = site, colour = site))

dat1 <- temp %>% 
  group_by(site) %>% 
  summarise(mn.temp = mean(temp, na.rm = TRUE),
            sd.temp = sd(temp, na.rm = TRUE))

ggplot(dat1, aes(x = site, y = mn.temp)) +
  geom_point() +
  geom_errorbar(aes(ymin = mn.temp - sd.temp,
                    ymax = mn.temp + sd.temp),
                width=.1) +
  theme(axis.text.x = element_text(angle = 90, hjust =0.05))


# ------ ------------------------------------------------------------------



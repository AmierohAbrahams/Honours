# ################################################################ --------
# This script analyses the wind data at the 8sites
# steps on changing the date 
#
# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)


# working with the wind data for the various sites
# Loading in the wind data for the sites
Ballito_2005 <-Ballito_2005 <- read_csv("Ballito_2005.csv") %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd))


Sodwana_2005 <- read_csv("Sodwana_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 

Tsitsikamma_2005 <- read_csv("Tsitsikamma_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 

Hout_Bay_2005 <- read_csv("Hout Bay_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 

Knysna_2005 <- read_csv("Knysna_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 

Mossel_Bay_2005 <- read_csv("Mossel Bay_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd))


Port_Nolloth_2005 <- read_csv("Port Nolloth_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 

Aliwal_Shoal_2005 <- read_csv("Aliwal Shoal_2005.csv")%>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  group_by(date) %>% 
  summarise(mean_speed = mean(wndSpd)) 



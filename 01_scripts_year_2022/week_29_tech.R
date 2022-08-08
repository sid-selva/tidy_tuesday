# library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(lubridate)
library(tidyquant)
library(gtExtras)
library(gt)
library(gtsummary)

#tuesdata <- tidytuesdayR::tt_load('2022-07-19')
# tuesdata <- tidytuesdayR::tt_load(2022, week = 29)

# rm(tuesdata)

# technology <- tuesdata$technology

# saveRDS(technology,file = "00_data/2022/week-29_tech.rds")

technology <- readRDS(file = "00_data/2022/week-29_tech.rds")

technology %>% glimpse()

var_list <- technology %>% 
  distinct(variable)

category_list <- technology %>% 
  distinct(category)

group_list <- technology %>% 
  distinct(group)

country_list <- technology %>% distinct(iso3c) %>% arrange(iso3c)

country <- "CAN"
cat1 <- "Industry"
var1 <- "aluminum"
  
  
technology %>% 
  filter(iso3c == country & variable == var1) %>% 
  ggplot(aes(year,value)) + 
  geom_line() + 
  theme_tq()

technology %>% filter(iso3c == country & category == cat1) %>% distinct(variable)

technology %>% 
  filter(iso3c == country & category == cat1) %>% 
  ggplot(aes(year,value, color = variable)) + 
  geom_line() + 
  facet_wrap(facets = ~ variable,scales = "free_y")
  
  geom_area()  

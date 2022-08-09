# Tidy Tuesday Week 30 2022 ----
# Oregon Frog Data set ----
# Use TidyTuesday package Once


# library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(tidyquant)
library(lubridate)
library(janitor)
library(waffle)

# Load Data ----

tuesdata <- tidytuesdayR::tt_load(2022, week = 31)
frogs <- as_tibble(tuesdata$frogs)

rm(tuesdata)
# saveRDS(object = frogs,file = "00_data/2022/week-31_frogs.rds")

frogs <- read_rds("00_data/2022/week-31_frogs.rds")

frogs %>% glimpse()

frogs %>% view()

frogs %<>% # %<>% is also frogs <- frogs %>% 
  
  # Convert Date Type 
  mutate(SurveyDate = mdy(SurveyDate)) %>% 
  
  # Add Month Column
  mutate(month = month(SurveyDate,label = TRUE,abbr = FALSE)) %>% 
  
  # Rearrange Cols 
  dplyr::relocate(month,.after = SurveyDate) %>% 
  
  # Rename Cols 
  rename(frog_id = Frequency) %>% 
  
  glimpse()

n_frogs <- frogs %>% distinct(frog_id) %>% 
  dplyr::n_distinct()

# How Many times Each frog Was Observed 

frog_observations <- frogs %>% 
  group_by(frog_id,month) %>% 
  summarise(count = n()) %>% 
  # Add Total row at the end
  janitor::adorn_totals() %>% 
  ungroup()

frogs %>% 
  select(month,HabType)


tbl_HabType <- table(frogs$month,
                     frogs$HabType,
                     dnn = c("Month", "HabType")) %>%
  as_tibble() %>%
  subset(n > 0) %>%
  dplyr::arrange(Month, desc(n)) %>%
  split(.[, "Month"], drop = TRUE) %>%
  purrr::map_df(., janitor::adorn_totals)

knitr::kable(tbl_HabType)


tbl_HabType_Str <- table(frogs$HabType,
                     frogs$Structure,
                     dnn = c("HabType", "Structure")) %>%
  as_tibble() %>%
  subset(n > 0) %>%
  dplyr::arrange(HabType, desc(n)) %>%
  split(.[, "HabType"], drop = TRUE) %>%
  purrr::map_df(., janitor::adorn_totals)

bg <- "#2C2818"
 # Plot 
frogs %>% 
  group_by(HabType,month) %>% 
  summarise(count = n(),.groups = "keep") %>% 
  ggplot(aes(fill = HabType,values = count)) + 
  geom_waffle(size = 1,
              flip = TRUE,
              radius = unit(6, "pt")) + 
  facet_wrap(~ month, strip.position = "bottom")+
  
  # Remove X Discrete Values
  scale_x_discrete(expand = c(0,0))+
  coord_equal() +
  theme_tq() + 
  scale_fill_tq() + 
  labs(
    title = "Where to spot Oregon spotted frogs?",
    subtitle = "Using radio-telemetry, the U.S. Geological Survey monitored Oregon spotted frogs",
    caption = "#TidyTuesday | Week 31 | August 2, 2022 \n Data: USGS | doi.org/10.5066/P9DACPCV")

sessionInfo()

# Tidy Tuesday Week 32 2022 ----
# Data set ----
# Use TidyTuesday package Once

# library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(lubridate)
library(tidyquant)
library(ggrepel)

#tuesdata <- tidytuesdayR::tt_load(2022, week = 32)
# wheels <- as_tibble(tuesdata$wheels)

rm(tuesdata)
# saveRDS(object = wheels,file = "00_data/2022/week-32_wheels.rds")

wheels <- read_rds("00_data/2022/week-32_wheels.rds")

wheels %>% glimpse()


# Get % of missing values (NAs)
plot_missing(wheels)

# Get Discrete or continuous Variables 
plot_intro(wheels)

tail(wheels)

wheels %>% select(-c("closed","vip_area","turns","diameter","design_manufacturer")) %>%
  select(name,country, height,opened) %>% 
  mutate(country = as.factor(country)) %>% 
  filter(year(opened)>=2000) %>% 
  ggplot(aes(opened,country)) + 
  geom_point(aes(size = height), color  = "dodgerblue4") + theme_tq() + 
  ggrepel::geom_label_repel(aes(label = name))


g1 <- wheels %>% select(-c("closed","vip_area","turns","diameter","design_manufacturer")) %>%
  select(name,country, height,opened) %>% 
  arrange(desc(height)) %>% mutate(name = as_factor(name) %>% fct_rev()) %>% 
  slice(1:20) %>% 
  mutate(lab_1 = str_glue("{name}, {country},{year(opened)}")) %>% 
  ggplot(aes(name,height)) + 
  geom_col(fill = "#2c3e50") + 
  coord_flip()+ 
  geom_label(mapping = aes(label = lab_1),hjust = 1)  + theme_tq() +
  theme(axis.title.y =element_blank(),
        axis.text.y  =element_blank(),
        axis.ticks.y =element_blank()) + 
  labs(
    title = "Top 20 Ferris Wheels, by Height",
    caption = "Data : Tidy Tuesday \n @Emil_Hvitfeldt"
  )

# Angle will work in geom_text not on geom_label, so used coord flip
# geom_text(mapping = aes(label = lab_1),angle = 90 ,vjust = 1)  + theme_tq()
# Use grid text https://github.com/tidyverse/ggplot2/issues/3373


wheels %>% select(-c("closed","vip_area","turns","diameter","design_manufacturer")) %>% 
  select(name,height, country) %>% 
  group_by(country) %>% 
  summarise(count = n(),
            height  = list(height)) %>% 
  arrange(desc(count)) %>% gt() %>% 
  gtExtras::gt_plt_dist(column = "height",type = "histogram", fill_color = "red") %>% 
  gt_theme_dark() %>%
  tab_header(
    title = "Ferris Wheels Summary, by Country",
    subtitle = "Tidy Tuesday, Week 32"
  )

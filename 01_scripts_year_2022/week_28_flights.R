# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# install.packages("gtExtras")
# install.packages("gtsummary")

# library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(lubridate)
library(tidyquant)
library(gtExtras)
library(gt)
library(gtsummary)

#tuesdata <- tidytuesdayR::tt_load('2022-07-12')
# tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

# flights <- tuesdata$flights 

# flights <- flights %>% as_tibble()

# saveRDS(flights,file = "00_data/2022/week-28_flights.rds")

# rm(tuesdata)

flights <- readRDS("00_data/2022/week-28_flights.rds")

flights %>% DataExplorer::plot_missing()

flights %>% plot_intro()

flights %>% glimpse()

flights %>% summary()

flights_trimmed_tbl <- flights %>% 
  select(-contains("IFR_"),-`Pivot Label`,-MONTH_NUM,-MONTH_MON,-APT_ICAO)

flights_trimmed_tbl <- readRDS("00_data/2022/week-28_flights_trimmed.rds")

# saveRDS(flights_trimmed_tbl,file = "00_data/2022/week-28_flights_trimmed.rds")

ordered_state_traffic_tbl <- flights_trimmed_tbl %>% 
  group_by(STATE_NAME) %>% 
  summarise(flights_total = sum(FLT_TOT_1)) %>% 
  ungroup() %>% 
  arrange(desc(flights_total))%>% 
  mutate(proporion = flights_total/sum(flights_total)) %>% 
  mutate(c_sum = cumsum(proporion))%>% 
  mutate(across(3:4,.fns = ~ round(.,digits = 3)))

ordered_apt_traffic_tbl <- flights_trimmed_tbl %>% 
  group_by(APT_NAME) %>% 
  summarise(flights_total = sum(FLT_TOT_1)) %>% 
  ungroup() %>% 
  arrange(desc(flights_total)) %>% 
  mutate(proporion = flights_total/sum(flights_total)) %>% 
  mutate(c_sum = cumsum(proporion)) %>% 
  mutate(across(3:4,.fns = ~ round(.,digits = 4)))

yearly_flights_tbl <- flights %>% 
  group_by(STATE_NAME,APT_NAME,YEAR) %>% 
  summarise(flights = sum(FLT_TOT_1)) %>% 
  arrange(desc(flights)) %>% 
  ungroup()


state_names <- yearly_flights_tbl %>% distinct(STATE_NAME)

top_30_apt_tbl <- ordered_apt_traffic_tbl %>% slice(1:31) %>% mutate(APT_NAME = as_factor(APT_NAME))
top_20_state_tbl <- ordered_state_traffic_tbl %>% slice(1:20) %>% mutate(STATE_NAME = as_factor(STATE_NAME))

apt_month_tbl <- flights_trimmed_tbl %>% mutate(date_mon = ymd(floor_date(FLT_DATE,unit = "month"))) %>%
  group_by(APT_NAME,date_mon) %>% 
  summarise(flights = sum(FLT_TOT_1)) %>% 
  ungroup()

state_month_tbl <- flights_trimmed_tbl %>% 
  mutate(date_mon = ymd(floor_date(FLT_DATE,unit = "month"))) %>%
  group_by(STATE_NAME,date_mon) %>% 
  summarise(flights = sum(FLT_TOT_1)) %>% 
  ungroup()

top_20_state_monthly_tbl <- top_20_state_tbl %>% select(STATE_NAME) %>% 
  left_join(state_month_tbl,by = "STATE_NAME") %>% 
  mutate(STATE_NAME = as_factor(STATE_NAME))

top_20_state_monthly_tbl %>% 
  mutate(label_num = scales::unit_format(flights,scale = 1e-3))

top_20_state_monthly_tbl %>% 
  mutate(year = year(date_mon),
         month = month(date_mon)) %>% 
  ggplot(mapping = aes(month,y = STATE_NAME)) +  
  geom_tile(aes(fill = flights)) + 
  scale_fill_gradient(low = "white", high = palette_light()[1]) + 
  facet_wrap(facets = ~ year,ncol = 1)



top_20_state_monthly_tbl %>% 
  ggplot(aes(date_mon,flights, color = STATE_NAME)) + 
  geom_line()

top_20_state_monthly_tbl %>% 
  filter(year(date_mon)>= 2019 & date_mon<= ymd("2022-06-01")) %>% 
  group_by(STATE_NAME) %>% 
  mutate(pct = flights/sum(flights)) %>% 
  mutate(flights_k = round((flights/1000),digits = 0)) %>% 
  ungroup() %>%
  mutate(year = year(date_mon),
         month = month(date_mon,label = TRUE)) %>%  
  ggplot(mapping = aes(month,y = STATE_NAME)) +  
  geom_tile(aes(fill = pct)) + 
  facet_wrap(facets = ~ year,nrow = 1) +
  scale_fill_gradient(low = "white", high = palette_light()[1]) + 
  geom_text(aes(label = flights_k),size = 3) + 
  theme_tq() + 
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  ) + 
  labs(
    title = "Monthly Distribution of Flights by Volume between 2019 - May 2022",
    subtitle = "Top 20 Busiest Memeber States in Europe, Values in Thousands",
    x = "Month", y = "State Name",
    caption = "Change in Hue indicates the propotion of flights volume w.r.t Country \n Data: TidyTuesday Week 28, 2022 \n Insights and Analytics"
  )

top_10_apr_monthly_tbl <- top_30_apt_tbl %>% select(APT_NAME) %>% 
  left_join(apt_month_tbl,by = "APT_NAME") %>% 
  mutate(APT_NAME = as_factor(APT_NAME)) %>% 
  filter(as.numeric(APT_NAME)<=10) %>% 
  filter(year(date_mon)>=2019)

top_10_apr_monthly_tbl %>% 
  mutate(date_mon = floor_date(date_mon,unit = "quarter")) %>% 
  group_by(date_mon,APT_NAME) %>% 
  summarise(flights = sum(flights)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "date_mon",values_from = "flights",names_prefix = "Q_") %>% 
  gt()

top_10_apr_monthly_tbl %>% 
  arrange(date_mon) %>% 
  select(-date_mon) %>% 
  group_by(APT_NAME) %>% 
  summarise(flights = list(flights)) %>% 
  gt() %>% 
  gt_plt_sparkline(flights)

top_20_apr_monthly_tbl <- top_30_apt_tbl %>% select(APT_NAME) %>% 
  left_join(apt_month_tbl,by = "APT_NAME") %>% 
  mutate(APT_NAME = as_factor(APT_NAME)) %>% 
  filter(as.numeric(APT_NAME)<=20)

top_20_apr_monthly_tbl %>% 
  arrange(date_mon) %>% 
  select(-date_mon) %>% 
  group_by(APT_NAME) %>% 
  summarise(flights = list(flights)) %>% 
  gt() %>% 
  gt_plt_sparkline(flights) %>% 
  gt_theme_nytimes() %>% 
  tab_header(title = "Monthly Flights in Europe's Busiest 20 Airports")

top_10_apr_monthly_tbl %>% 
  mutate(date_mon = floor_date(date_mon,unit = "quarter")) %>% 
  rename(quarter = date_mon) %>% 
  group_by(quarter,APT_NAME) %>% 
  summarise(flights = sum(flights)) %>% 
  pivot_wider(names_from = "APT_NAME",values_from = "flights") %>% 
  janitor::clean_names() %>% 
  gt() %>% 
  gt_color_rows(2:11) %>% 
  gt_theme_nytimes()

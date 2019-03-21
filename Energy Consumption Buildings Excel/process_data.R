library(tidyverse)
library(lubridate)
library(janitor)


data <- read_csv("Hourly elect use Alumni.csv",
                 col_types = "cd") %>% clean_names()
elec_col = names(data)[2]
data <- data %>% rename(electricity = !!(sym(elec_col))) %>% 
  # Filter out consecutive zeros at the beginning until 
  # we start getting meaningful data
  mutate(started = cumsum(electricity > 0) > 0) %>%
  filter(started) %>% select(-started)
# Weekday: 1 = Sunday, 2 = Monday ...
data <- data %>% mutate(date_time = ymd_hm(date_time), year = year(date_time), 
                        mon = month(date_time),
                        day = day(date_time), hour = hour(date_time), 
                        month = month(date_time, label = TRUE),
                        week_day = wday(date_time, label = TRUE))
summary_data <- data %>% filter(electricity > 0, electricity < 500) %>%
  group_by(month, week_day, hour) %>% 
  
  #make something use mutat on data that creates vairable true or false for if it is a weekend and group by weekend
  #solar potential for each month hour by hour. distribution of sunlight there. 
  summarize(mean_electricity = mean(electricity, na.rm = T), 
            sd_electricity = sd(electricity, na.rm = T)) %>% 
  ungroup()
p <- ggplot(summary_data, aes(x = hour, y = mean_electricity, color = ordered(week_day))) + 
  geom_line() + 
  facet_wrap(~month, scales = "free_y") + 
  scale_color_brewer(palette = "Dark2")

plot(p)
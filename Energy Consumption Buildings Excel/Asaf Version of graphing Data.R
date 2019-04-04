library(tidyverse)
library(lubridate)
library(janitor)
library(here)

data_dir <- here::here("Energy Consumption Buildings Excel")


process_building <- function(fname, bldg_name) {
  data_asaf <- read_csv(file.path(data_dir, fname)) %>%
    clean_names() %>% rename(kwh = 2) %>%
    mutate(building_name = bldg_name)
  #
  # Use mutate to add a colmn with the month and the day of the week and the hour of the day
  # (lubridate has functions to get these thihgs from the date_time column)
  # 
  # Use filter to remove rows with crazy values for kwh
  # 
  # Use group_by and summarize to calculate average values
  # for the hourly electricity use based on day of the week
  # so you will have a data frame with day, month, hour, and 
  # average_kwh for all the entries with that hour, day of week, 
  # and month.
  #
  # So for instance, "February", "Wednesday", "13" will be the
  # average electricity use for 1:00 pm to 1:59 pm for all of the
  # Wednesdays in all of the Februaries of all the years in your
  # data set.
  #
  # df %>% mutate(...) %>% group_by(...) %>% summarize(...) %>% ungroup()
  #
  # Make sure that you don't lose the "building_name" column
  
  

  invisible(data_asaf)
}


# Chapter 12 exercises 

df <- data_frame()

for(f in head(list.files(data_dir, pattern = "\\.csv$"), 3)) {
  # From the stringr package
  # Use a string command str_replace_all or something like that
  # to get the part of the file name before the period.
  # bldg_name <- ...
  bldg_name <- f
  df <- df %>% bind_rows(
    process_building(f, bldg_name)
  )
}

df2 <- df %>% spread(key = building_name, value = kwh)
df3 <- df2 %>% gather(key = building_name, value = kwh, -date_time) #date is minus because we do not want to gather it



print(data_asaf)
      

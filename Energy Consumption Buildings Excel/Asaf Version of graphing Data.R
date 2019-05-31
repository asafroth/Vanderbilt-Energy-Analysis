library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(tidyr)
library(forcats)

data_dir <- here::here("Energy Consumption Buildings Excel")


# what doies kwh = 2 do?
# what does the first mutate do mutate(building_name = bldg_name)



# example of how to use process data
# process_building("buttrick.csv", "buttrick")

process_building <- function(fname, bldg_name) {
  data_asaf <- read_csv(file.path(data_dir, fname)) %>%
    clean_names() %>% rename(kwh = 2) %>%
    transmute(kwh, building_name = bldg_name, date = date(date_time), 
              hour = hour(date_time))
  invisible(data_asaf)
}


  
no_building_name <- function(df){
df<-select(df, kwh, date, hour)
  invisible(df)
}


group_month_and_day <-function(df){
  data_asaf <- df %>% mutate(mon = month(date), dow = wday(date))
  avg_data <- data_asaf %>% group_by(building_name, mon, dow, hour) %>% 
                      summarize(avg_kwh = mean(kwh, na.rm = TRUE )) %>% ungroup()
  invisible(avg_data)
}


#
# Modify this to group weekends, and two groups of weekdays: MWF and TR
# So there will be three groups for dow: SS, MWF, and TR
#
group_month_and_day_2 <-function(df){
  
  data_asaf <- df %>% mutate(mon = month(date) %>% ordered(levels = 1:12, labels = month.abb), 
                             dow = wday(date), 
                             dow2 =  factor(dow) %>% 
                               fct_collapse(SS = c("1","7"),
                                            MWF = c("2","4","6"),
                                            TR = c("3","5")))
  avg_data <- data_asaf %>% group_by(building_name, mon, dow2, hour) %>% 
    summarize(avg_kwh = mean(kwh, na.rm = TRUE )) %>% ungroup()
  invisible(avg_data)
}

gg_data <-function(df){
avg_data_2 <- group_month_and_day_2(df) # calls function group_month_and_day_2 in the function
ggplot(avg_data_2, aes(x = hour, y = avg_kwh, color = dow2)) + geom_line() + facet_wrap(~mon)
  
}

# broke down every step




# how does facet wrap work?
# it must divide the different panels by the months
# why is the tilda in front of day
# factors are subset of the variable. days of the week is a factor in this. since the x is hours and y is kwh
#ex gender or day of the week

# why cant i call facet in the aes


asaf_gg_plot <-function(data){
  ggplot(data, aes(x = hour, y = avg_kwh, color = dow2)) + geom_line() + facet_wrap(~mon) + theme_bw() + 
    labs(title = "ASAF's Plot") +  labs(x = "hour", y = "average KWH")
}





##


## combing multiple different data sets together on to one ggplot using facet wrap
## facet grid, building on the y axis, x is the month  facet_grid(building ~ mon)

#combining data sets
  # for loop on process building and then use bind
buildings <- c("Buttrick Hall" = "buttrick.csv",
                 "Branscomb Market" = "brandscomb_market_place.csv",
               "Bronson Art Studio" = "BronsonArtStudio.csv",
               "Calhoun Hall" = "calhound.csv")





# write a function that groups by month and day of month.
# create new columns, day of the week and month
# use libridate function to get the month
#same way we did the hour
# and day of the week (wday)
# month is (month)



  # Asaf's Questions
  # looking at the date I noticed that the firt column is the date and the time and I want to separate those 
  # I want to separet the first column onto date and time


  #I want to Rename the Column and then separate the column into date and time
  
  
  
  
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
  

# Chapter 12 exercises 

# df1 <- data_frame()
# 
# for(f in head(list.files(data_dir, pattern = "\\.csv$"), 3)) {
#   # From the stringr package
#   # Use a string command str_replace_all or something like that
#   # to get the part of the file name before the period.
#   # bldg_name <- ...
#   bldg_name <- f
#   df1 <- df1 %>% bind_rows(
#     process_building(f, bldg_name)
#   )
# }
# 
# df2 <- df1 %>% spread(key = building_name, value = kwh)
# df3 <- df2 %>% gather(key = building_name, value = kwh, -date) #date is minus because we do not want to gather it
# 
# 
# 
# print(data_asaf)

# for loop on process building and then use bind
buildings <- c("Buttrick Hall" = "buttrick.csv",
               "Branscomb Market" = "brandscomb_market_place.csv",
               "Bronson Art Studio" = "BronsonArtStudio.csv",
               "Calhoun Hall" = "calhoun.csv")

data <- tibble()

for (bldg in names(buildings)) {
  df <- process_building(buildings[bldg], bldg)
  data <- bind_rows(data, df)
}

# This is how you would do this in C++
#
# for(i = 0, i < length(buildings), i++) {
#   const char * bldg = names(buildings)[i];
#   ...
# }

# The R way of writing a C++ loop
#
# for (i in seq(length(buildings))) {
#   bldg <- names(buildings)[i]
#   df <- process_building(buildings[bldg], bldg)
#   data <- bind_rows(data, df)
# }

# Now do it with purrr, INSTEAD of the for loop
#
# For really cool examples and tutorial of this kind of thing,
# Google Professor Jenny Bryan's lectures on "Data Rectangling"
#

#
# map creates a list:
# map2(a, b, ~f(.x, .y)) creates a list of f(a[1], b[1]), f(a[2],b[2]), ...
# 
# If f returns data frames, then the output of map2(a, b, ~f(.x, .y))
# will be a list of data frames. If we want to bind their rows together
# to make one big data frame, we can pipe the output of map2() to bind_rows():
#
# map2(a, b, ~f(.x, .y)) %>% bind_rows() will return one big data frame.
#
# map2_df(a, b, ~f(.x, .y)) tells map that the output of the function f
# will be a data frame, so it should bind all of the data frames together
# so
# map2_df(a, b, ~f(.x, .y))
# is exactly the same as
# map2(a, b, ~f(.x, .y)) %>% bind_rows()
#
# Each line in the stuff below does exactly the same thing.
# They are all different ways to creqte the same data frame "data"
#
data <- map2_df(names(buildings), buildings, ~process_building(.y, .x))
data <- map2_df(buildings, names(buildings), ~process_building(.x, .y))

data <- map2(buildings, names(buildings), ~process_building(.x, .y)) %>%
  bind_rows()


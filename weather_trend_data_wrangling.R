########################################################################
# Amber Simmons
# 2023-03-20
#
# Exploring Weather Trends:
# Columbus
########################################################################

library(tidyr)
library(zoo)
library(dplyr)
library(lubridate)

########################################################################

# Get current working directory, store, and set new directory 
oldwd <- getwd() 
setwd('F:/Professional Work/Udacity/Nanodegree_Data_Analyst/Project1_ColumbusWeatherTrends/')

########################################################################

# Load CSV files

cbus_data <- read.csv('data/Columbus_temps.csv') %>% 
  select(year, avg_temp) %>%
  mutate(year = year(as.Date(as.character(year),'%Y')),
         avg_temp = as.numeric(avg_temp),
         group = 'Columbus') %>%
  arrange(year)

global_temps <- read.csv('data/global_temps.csv')%>%
  mutate(year = year(as.Date(as.character(year),'%Y')),
         avg_temp = as.numeric(avg_temp),
         group = 'Global') %>% 
  arrange(year)


# Create moving average variables and combine data

cbus_data_full <- cbus_data %>%
  mutate(`5-Year MA` = zoo::rollmean(avg_temp, k = 5, fill = NA, align = 'right'),
         `10-Year MA` =  zoo::rollmean(avg_temp, k = 10, fill = NA, align = 'right'),     
         `50-Year MA` = zoo::rollmean(avg_temp, k = 50, fill = NA, align = 'right')
       )

global_data_full <- global_temps %>%
  mutate(`5-Year MA` = zoo::rollmean(avg_temp, k = 5, fill = NA, align = 'right'),
         `10-Year MA` =  zoo::rollmean(avg_temp, k = 10, fill = NA, align = 'right'),     
         `50-Year MA` = zoo::rollmean(avg_temp, k = 50, fill = NA, align = 'right')
  )

combined <- bind_rows(cbus_data_full,global_data_full) %>%
  arrange(year,group)

differences <- left_join(cbus_data_full %>% filter(year >= 1750),
                         global_data_full %>% filter(year <= 2013),
                         by = 'year',
                         suffix = c('_cbus','_global')) %>% 
  arrange(year) %>%
  mutate(MA_temp_diff = `10-Year MA_cbus` - `10-Year MA_global`)

########################################################################

# Reset to old working directory after script runs
setwd(oldwd)

########################################################################
# Moving average POC
# 'Moving Average Example'

# df <- read.csv('data/Moving Average Example - Data.csv')
# df_ra <- df %>%
#   mutate(Sales =  as.numeric(gsub(',','',Sales)),
#          Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   arrange(Date) %>%
#   mutate(`7-Day MA` = zoo::rollmean(as.numeric(gsub(',','',Sales)), k = 7, fill = NA, align = 'right'),
#          `14-Day MA` = zoo::rollmean(as.numeric(gsub(',','',Sales)), k = 14, fill = NA, align = 'right')
#          ) 

# This solution matches the example's solution!



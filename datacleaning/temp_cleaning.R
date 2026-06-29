#Angus Boswell 114266017

#install.packages("purrr")


#rm(list = ls())

# Load required packages for data manipulation (dplyr, ggplot2, etc.)
library(tidyr)
library(dplyr)
library(ggplot2)
#library(jsonlite)
library(readxl)
library(lubridate)
#library(plm)
library(purrr)
#library(zoo)
library(writexl)
library(leaflet)


setwd("C:/Users/aesbo/Documents/Taiwan/4.IMES/2.Semester/4.Big Data for Social Analysis/2.Project")

##############################################################################

df_temp_raw_2022 <- read.csv("avg_temp_2022.csv", check.names = FALSE, strip.white = TRUE) %>%
  select(-last_col())

df_temp_raw_2023 <- read.csv("avg_temp_2023.csv", check.names = FALSE, strip.white = TRUE) %>%
  select(-last_col())

df_temp_raw_2024 <- read.csv("avg_temp_2024.csv", check.names = FALSE, strip.white = TRUE) %>%
  select(-last_col(), -`20240229`)


df_temp_raw <- df_temp_raw_2022 %>%
  left_join(df_temp_raw_2023, by = c("LON", "LAT")) %>%
  left_join(df_temp_raw_2024, by = c("LON", "LAT"))

#df_temp_raw <- bind_rows(
 # read.csv("avg_temp_2022.csv", check.names = FALSE, strip.white = TRUE),
  #read.csv("avg_temp_2023.csv", check.names = FALSE, strip.white = TRUE),
  #read.csv("avg_temp_2024.csv", check.names = FALSE, strip.white = TRUE))


df_temp <- df_temp_raw %>%
  pivot_longer(
    cols = -c(LON, LAT),
    names_to = "date",
    values_to = "avg_temp"
  ) %>%
  mutate(
    date = as.Date(date, format = "%Y%m%d"),
    avg_temp = ifelse(avg_temp < -90, NA, avg_temp)
  ) %>%
  rename(lat_r = LAT, lon_r = LON)

# get only the unique date/coord combos we actually need
lookup <- df_eng %>%
  select(date, lon_r, lat_r) %>%
  distinct()

# filter df_temp to only rows we need
avg_temp_lookup <- df_temp %>%
  inner_join(lookup, by = c("lon_r", "lat_r", "date")) %>%
  select(lon_r, lat_r, date, avg_temp) %>%
  distinct(lon_r, lat_r, date, .keep_all = TRUE)

# join back to df_eng
df_eng <- df_eng %>%
  inner_join(avg_temp_lookup, by = c("lon_r", "lat_r", "date"))


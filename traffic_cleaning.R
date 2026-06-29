#Angus Boswell 114266017

#install.packages("purrr")


rm(list = ls())

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

# Read all CSVs from all folders and merge

files <- list.files(path = c("C:/Users/aesbo/Documents/Taiwan/4.IMES/2.Semester/3.Causal Inference and Data Science/1.Assignments/2.HW/111",
                             "C:/Users/aesbo/Documents/Taiwan/4.IMES/2.Semester/3.Causal Inference and Data Science/1.Assignments/2.HW/112",
                             "C:/Users/aesbo/Documents/Taiwan/4.IMES/2.Semester/3.Causal Inference and Data Science/1.Assignments/2.HW/113"),
                    pattern = "\\.csv$", full.names = TRUE)

common_cols <- Reduce(intersect, lapply(files, function(f) names(read.csv(f, nrow = 1))))
df <- bind_rows(lapply(files, function(f) read.csv(f)[, common_cols]))


##############################################################################

df_eng <- df %>%
  rename(
    Year_of_Occurrence = 發生年度,
    Month_of_Occurrence = 發生月份,
    Date_of_Occurrence = 發生日期,
    Time_of_Occurrence = 發生時間,
    Accident_Category = 事故類別名稱,
    Handling_Police_Department = 處理單位名稱警局層,
    Location_of_Occurrence = 發生地點,
    Weather_Condition = 天候名稱,
    Lighting_Condition = 光線名稱,
    Road_Type_First_Party = `道路類別.第1當事者.名稱`,
    Speed_Limit_First_Party = `速限.第1當事者`,
    Road_Type_Main_Category = 道路型態大類別名稱,
    Road_Type_Subcategory = 道路型態子類別名稱,
    Accident_Location_Main_Category = 事故位置大類別名稱,
    Accident_Location_Subcategory = 事故位置子類別名稱,
    Road_Surface_Pavement_Type = `路面狀況.路面鋪裝名稱`,
    Road_Surface_State = `路面狀況.路面狀態名稱`,
    Road_Surface_Defect = `路面狀況.路面缺陷名稱`,
    Road_Obstacle_Type = `道路障礙.障礙物名稱`,
    Visibility_Quality = `道路障礙.視距品質名稱`,
    Sight_Distance = `道路障礙.視距名稱`,
    Traffic_Signal_Type = `號誌.號誌種類名稱`,
    Traffic_Signal_Status = `號誌.號誌動作名稱`,
    Direction_Separation_Main_Category = `車道劃分設施.分向設施大類別名稱`,
    Direction_Separation_Subcategory = `車道劃分設施.分向設施子類別名稱`,
    Between_Express_and_General_Lanes = `車道劃分設施.分道設施.快車道或一般車道間名稱`,
    Between_Fast_and_Slow_Lanes = `車道劃分設施.分道設施.快慢車道間名稱`,
    Road_Edge_Line = `車道劃分設施.分道設施.路面邊線名稱`,
    Accident_Type_Main_Category = 事故類型及型態大類別名稱,
    Accident_Type_Subcategory = 事故類型及型態子類別名稱,
    Primary_Cause_Main_Category = `肇因研判大類別名稱.主要`,
    Primary_Cause_Subcategory = `肇因研判子類別名稱.主要`,
    Number_of_Deaths_and_Injuries = 死亡受傷人數,
    Party_Sequence_Number = 當事者順位,
    Vehicle_Type_Main_Category = `當事者區分.類別.大類別名稱.車種`,
    Vehicle_Type_Subcategory = `當事者區分.類別.子類別名稱.車種`,
    Party_Gender = `當事者屬.性.別名稱`,
    Party_Age = 當事者事故發生時年齡,
    Protective_Equipment = 保護裝備名稱,
    Device_Usage = 行動電話或電腦或其他相類功能裝置名稱,
    Party_Action_Status_Main_Category = 當事者行動狀態大類別名稱,
    Party_Action_Status_Subcategory = 當事者行動狀態子類別名稱,
    Initial_Impact_Area_Main_Category = `車輛撞擊部位大類別名稱.最初`,
    Initial_Impact_Area_Subcategory = `車輛撞擊部位子類別名稱.最初`,
    Other_Impact_Area_Main_Category = `車輛撞擊部位大類別名稱.其他`,
    Other_Impact_Area_Subcategory = `車輛撞擊部位子類別名稱.其他`,
    Individual_Cause_Main_Category = `肇因研判大類別名稱.個別`,
    Individual_Cause_Subcategory = `肇因研判子類別名稱.個別`,
    Hit_and_Run_Status = `肇事逃逸類別名稱.是否肇逃`,
    Longitude = 經度,
    Latitude = 緯度
  )


##############################################################################

df_eng <- df_eng %>%
  select(
    Date_of_Occurrence,
    Time_of_Occurrence,
    Location_of_Occurrence,
    Weather_Condition,
    Accident_Location_Main_Category,
    Accident_Location_Subcategory,
    Accident_Type_Main_Category,
    Accident_Type_Subcategory,
    Primary_Cause_Main_Category,
    Primary_Cause_Subcategory,
    Number_of_Deaths_and_Injuries,
    Party_Sequence_Number,
    Vehicle_Type_Main_Category,
    Vehicle_Type_Subcategory,
    Party_Gender,
    Party_Age,
    Hit_and_Run_Status,
    Longitude,
    Latitude) %>%
  rename(
    date                    = Date_of_Occurrence,
    time                    = Time_of_Occurrence,
    location                = Location_of_Occurrence,
    weather                 = Weather_Condition,
    accident_location       = Accident_Location_Main_Category,
    accident_location_sub   = Accident_Location_Subcategory,
    accident_type           = Accident_Type_Main_Category,
    accident_type_sub       = Accident_Type_Subcategory,
    cause                   = Primary_Cause_Main_Category,
    cause_sub               = Primary_Cause_Subcategory,
    deaths_injuries         = Number_of_Deaths_and_Injuries,
    party_seq               = Party_Sequence_Number,
    vehicle_type            = Vehicle_Type_Main_Category,
    vehicle_type_sub        = Vehicle_Type_Subcategory,
    gender                  = Party_Gender,
    age                     = Party_Age,
    hit_and_run             = Hit_and_Run_Status,
    longitude               = Longitude,
    latitude                = Latitude)


df_eng <- df_eng %>%
  mutate(
    deaths   = as.integer(substr(deaths_injuries, 3, 3)),
    injuries = as.integer(substr(deaths_injuries, nchar(deaths_injuries), nchar(deaths_injuries))),
    deaths_injuries = NULL)

df_eng <- df_eng %>%
  mutate(
    gender = case_when(
      gender == "男"                ~ "M",
      gender == "女"                ~ "F",
      gender == "無或物(動物、堆置物)" ~ NA_character_,
      TRUE                          ~ NA_character_))

df_eng <- df_eng %>%
  mutate(
    vehicle_type = case_when(
      vehicle_type == "機車"                   ~ "Motorcycle",
      vehicle_type == "小客車"                  ~ "Car",
      vehicle_type == "小貨車(含客、貨兩用)"    ~ "Car",
      vehicle_type == "人"                      ~ "Pedestrian",
      vehicle_type == "小客車(含客、貨兩用)"    ~ "Car",
      vehicle_type == "小貨車"                  ~ "Car",
      TRUE                                      ~ NA_character_))

df_eng <- df_eng %>%
  group_by(date, location, time) %>%
  mutate(accident_id = cur_group_id()) %>%
  ungroup()

df_eng <- df_eng %>%
  filter(!is.na(vehicle_type)) %>%
  group_by(accident_id) %>%
  arrange(party_seq, .by_group = TRUE) %>%
  mutate(party_seq = row_number()) %>%
  ungroup()


valid_ids <- df_eng %>%
  group_by(accident_id) %>%
  filter(n() > 1) %>%
  filter(any(party_seq == 1 & vehicle_type %in% c("Car", "Motorcycle"))) %>%
  filter(any(cause %in% c("駕駛人", "駕駛者"))) %>%
  filter(any(accident_type %in% c("車與車", "人與汽(機)車", "人與車", "平交道事故"))) %>%
  ungroup() %>%
  pull(accident_id) %>%
  unique()

df_eng <- df_eng %>%
  filter(accident_id %in% valid_ids)


df_eng <- df_eng %>%
  mutate(
    weather = case_when(
      weather == "晴"     ~ "Clear",
      weather == "陰"     ~ "Overcast",
      weather == "雨"     ~ "Rain",
      weather == "霧或煙" ~ "Fog or Smoke",
      weather == "風沙"   ~ "Dust Storm",
      weather == "暴雨"   ~ "Heavy Rain",
      weather == "強風"   ~ "Strong Wind",
      weather == "風"     ~ "Wind",
      weather == "雪"     ~ "Snow",
      TRUE                ~ NA_character_
    )
  )

df_eng <- df_eng %>%
  mutate(
    accident_type = case_when(
      accident_type == "汽(機)車本身" ~ "Vehicle-Single",
      accident_type == "車與車"       ~ "Vehicle-Vehicle",
      accident_type == "人與汽(機)車" ~ "Pedestrian-Vehicle",
      accident_type == "平交道事故"   ~ "Level-Crossing",
      accident_type == "車輛本身"     ~ "Vehicle-Single",
      accident_type == "人與車"       ~ "Pedestrian-Vehicle",
      TRUE                            ~ NA_character_
    )
  )


df_eng <- df_eng %>%
  group_by(accident_id) %>%
  filter(all(cause %in% c("駕駛人", "駕駛者"))) %>%
  ungroup()

df_eng <- df_eng %>%
  mutate(
    cause = "Driver Error")

df_eng <- df_eng %>%
  mutate(
    hit_and_run = case_when(
      hit_and_run == "否" ~ "0",
      hit_and_run == "是" ~ "1",
      TRUE                ~ NA_character_))

df_eng <- df_eng %>%
  filter(accident_type %in% c("Pedestrian-Vehicle", "Vehicle-Vehicle"))

df_eng <- df_eng %>%
  mutate(
    date = as.Date(as.character(date), format = "%Y%m%d"),
    time = format(strptime(sprintf("%06d", time), format = "%H%M%S"), "%H:%M:%S")
  )

df_eng <- df_eng %>%
  select(accident_id, everything(), latitude, longitude)

df_eng <- df_eng %>%
  mutate(
    lon_r = round(longitude, 2),
    lat_r = round(latitude, 2)
  )

df_eng_2 <- df_eng %>%
  mutate(rainfall = ifelse(rainfall < -90, NA, rainfall))


df_eng_2 <- df_eng_2 %>%
  group_by(accident_id) %>%
  filter(!any(is.na(rainfall)), !any(is.na(avg_temp))) %>%
  ungroup()

#write.csv(df_eng_2, "df_traffic_temp_rain.csv", row.names = FALSE)

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(purrr)
library(writexl)
library(leaflet)
library(ggrepel)
library(sf)

setwd("C://Users//user//Documents//big data final")

a1 <- read.csv("df_traffic_temp_rain.csv")

df <- a1 %>%
  mutate(
    rain_category = case_when(
      rainfall < 1  ~ "None",
      rainfall < 30 ~ "Light",
      rainfall < 60 ~ "Moderate",
      TRUE          ~ "Heavy"
    ),
    rain_category = factor(rain_category, levels = c("None", "Light", "Moderate", "Heavy"))
  )

accident_counts <- df %>%
  filter(party_seq == 1) %>%
  group_by(date, gender, rain_category) %>%
  summarise(n_accidents = n(), .groups = "drop")

model <- glm(n_accidents ~ rain_category + gender + rain_category:gender,
             data = accident_counts,
             family = poisson)

summary(model)

# Regression Coefficient Plot

model_results <- as.data.frame(summary(model)$coefficients) %>%
  mutate(Variable = rownames(.)) %>%
  rename(Estimate = Estimate, Std_Error = `Std. Error`, P_Value = `Pr(>|z|)`) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Lower_CI = Estimate - 1.96 * Std_Error,
    Upper_CI = Estimate + 1.96 * Std_Error
  )

p_coef <- ggplot(model_results, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "skyblue4", linewidth = 0.8) +
  geom_point(color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Coefficient Plot of Traffic Accident Poisson Regression Model",
    subtitle = "Points represent Estimates; Lines represent 95% Confidence Intervals",
    x = "Variables / Interaction Terms",
    y = "Log-Odds Estimate"
  )

ggsave("coefficient_plot.png", plot = p_coef, width = 8, height = 5, dpi = 300)

# Map 4: Traffic Accident Fatality Rates

df_mortality <- df %>%
  filter(party_seq == 1) %>%
  filter(longitude > 119 & longitude < 123 & latitude > 21 & latitude < 26) %>%
  mutate(
    grid_x = floor(longitude / 0.05) * 0.05,
    grid_y = floor(latitude / 0.05) * 0.05,
    is_fatal = if_else(deaths > 0, 1, 0)
  )

grid_mortality <- df_mortality %>%
  group_by(grid_x, grid_y) %>%
  summarise(
    total_accidents = n(),
    fatal_accidents = sum(is_fatal),
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 10) %>%
  mutate(fatal_rate = fatal_accidents / total_accidents)

p_fatal_rate <- ggplot(data = grid_mortality) +
  geom_tile(aes(x = grid_x + 0.025, y = grid_y + 0.025, fill = fatal_rate)) +
  scale_fill_viridis_c(option = "inferno", labels = scales::percent) +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Grid-Based Traffic Accident Fatality Rates in Taiwan (2022-2024)",
    subtitle = "Grids with at least 10 accidents; Color represents (Fatal Accidents / Total Accidents)",
    x = "Longitude",
    y = "Latitude",
    fill = "Fatality Rate"
  )

ggsave("map_traffic_fatality_rate.png", plot = p_fatal_rate, width = 8, height = 10, dpi = 300)


# Map 5: Male At-Fault Accident Proportions

df_male_rate <- df %>%
  filter(party_seq == 1) %>%
  filter(longitude > 119 & longitude < 123 & latitude > 21 & latitude < 26) %>%
  mutate(
    grid_x = floor(longitude / 0.05) * 0.05,
    grid_y = floor(latitude / 0.05) * 0.05,
    is_male = if_else(gender == "M", 1, 0)
  )

grid_male_rate <- df_male_rate %>%
  group_by(grid_x, grid_y) %>%
  summarise(
    total_accidents = n(),
    male_accidents = sum(is_male),
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 10) %>%
  mutate(male_rate = male_accidents / total_accidents)

p_male_rate <- ggplot(data = grid_male_rate) +
  geom_tile(aes(x = grid_x + 0.025, y = grid_y + 0.025, fill = male_rate)) +
  scale_fill_viridis_c(option = "mako", labels = scales::percent) +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Grid-Based Male At-Fault Accident Proportions in Taiwan (2022-2024)",
    subtitle = "Grids with at least 10 accidents; Color represents (Male At-Fault / Total At-Fault)",
    x = "Longitude",
    y = "Latitude",
    fill = "Male Proportion"
  )

ggsave("map_male_at_fault_rate.png", plot = p_male_rate, width = 8, height = 10, dpi = 300)


###

library(sf)
twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp")
names(twn_sf)



typology <- read.csv("C:/Users/user/Downloads/academia_sinica_typology.csv") %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "都市",
    urban_level <= 5 ~ "半都市",
    TRUE             ~ "鄉村"
  ))

twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp") %>%
  st_transform(crs = 4326)

typology <- read.csv("academia_sinica_typology.csv") %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "都市",
    urban_level <= 5 ~ "半都市",
    TRUE             ~ "鄉村"
  ))

twn_sf <- twn_sf %>%
  left_join(typology, by = "TOWNCODE")

# ── 2. 車禍點對應到鄉鎮 ─────────────────────────────────
df_sf <- df %>%
  filter(party_seq == 1,
         longitude > 119, longitude < 123,
         latitude > 21, latitude < 26) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_urban <- st_join(df_sf, twn_sf[, c("TOWNCODE", "urban_class")],
                    join = st_within) %>%
  st_drop_geometry()

#  ─────────────────────────
df_urban %>%
  filter(!is.na(urban_class)) %>%
  count(urban_class) %>%
  ggplot(aes(x = reorder(urban_class, -n), y = n, fill = urban_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5) +
  scale_fill_manual(values = c("都市" = "#E74C3C", "半都市" = "#F39C12", "鄉村" = "#27AE60")) +
  theme_minimal() +
  labs(title = "各都市化程度車禍件數", x = "都市化程度", y = "車禍件數", fill = "")



# 1. Load shapefile and match coordinate system
twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp") %>% st_transform(crs = 4326)

# 2. Convert traffic data to spatial object and join with map boundaries
df_sf <- df %>%
  filter(party_seq == 1, longitude > 119 & longitude < 123 & latitude > 21 & latitude < 26) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 3. Process urbanization level and plot the gender proportion chart
st_join(df_sf, twn_sf[, "TOWNNAME"], join = st_within) %>%
  st_drop_geometry() %>%
  mutate(
    suffix = substr(TOWNNAME, nchar(TOWNNAME), nchar(TOWNNAME)),
    urbanization = case_when(suffix == "區" ~ "High", suffix %in% c("市", "鎮") ~ "Medium", suffix == "鄉" ~ "Low", TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(urbanization), gender %in% c("M", "F")) %>%
  
  ggplot(aes(x = factor(urbanization, levels = c("High", "Medium", "Low")), fill = gender)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("M" = "#3498DB", "F" = "#E74C3C")) +
  theme_minimal() +
  labs(title = "Accident Gender Proportion by Urbanization", x = "Urbanization Level", y = "Percentage", fill = "Gender")

# 4. Regression model including urbanization
# Model 2: Weather + Gender + Weather × Gender + Urbanization

accident_counts_urban <- df_urban %>%
  filter(
    !is.na(urban_class),
    gender %in% c("M", "F"),
    !is.na(rain_category)
  ) %>%
  mutate(
    gender = factor(gender, levels = c("F", "M")),
    rain_category = factor(rain_category, levels = c("None", "Light", "Moderate", "Heavy")),
    urban_class = factor(urban_class, levels = c("鄉村", "半都市", "都市"))
  ) %>%
  group_by(date, gender, rain_category, urban_class) %>%
  summarise(n_accidents = n(), .groups = "drop")

model_urban <- glm(
  n_accidents ~ rain_category + gender + rain_category:gender + urban_class,
  data = accident_counts_urban,
  family = poisson
)

summary(model_urban)



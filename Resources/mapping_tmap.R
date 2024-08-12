library(here)
library(tidyverse)
library(survey)
library(sf)
library(tigris)
library(viridis)
library(mapsf)
library(grid)
library(tidygeocoder)

# Reading in the 2022 data
data_2022 <- readRDS(here("temp_data", "2022_data.rds"))
str(data_2022)
colnames(data_2022)

# Create a lookup table for state FIPS codes
state_fips <- tibble(
  STATE = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 66, 72, 78),
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Guam", "Puerto Rico", "Virgin Islands")
)

# Removing "-" from all variables - causes problems
data_2022 <- data_2022 %>% 
  rename_with(~str_remove(., "_"), starts_with("_"))

# Join BRFSS data with state FIPS lookup
data_2022 <- data_2022 %>% 
  left_join(state_fips, by = "STATE")

# Levels of state
levels(data_2022$state)

# Recode CNCRDIFF as a factor
data_2022$CNCRDIFF <- factor(data_2022$CNCRDIFF, levels = c(1, 2, 3, 7, 9), 
                             labels = c("One", "Two", "Three or more", "Don't know/Not Sure", "Refused"))

# Set survey options
options(survey.lonely.psu = "adjust")

# Create survey design object
data_2022_survey_adjusted <- svydesign(
  id = ~PSU,
  strata = ~STSTR,
  weights = ~WT2RAKE,
  data = data_2022,
  nest = TRUE
)

# Calculate weighted proportions for each category of the cancer variable by state
cancer_by_state <- svyby(
  ~CNCRDIFF,
  ~STATE,
  data_2022_survey_adjusted,
  svymean,
  na.rm = TRUE
)

# Reshape data for easier merging
cancer_by_state_long <- cancer_by_state %>%
  pivot_longer(cols = starts_with("CNCRDIFF"), names_to = "Cancer_Type", values_to = "Proportion") %>%
  mutate(Type = case_when(
    str_detect(Cancer_Type, "One") ~ "One",
    str_detect(Cancer_Type, "Two") ~ "Two",
    str_detect(Cancer_Type, "Three.or.more") ~ "Three or more",
    str_detect(Cancer_Type, "Don.t.know.Not.Sure") ~ "Don't know/Not Sure",
    str_detect(Cancer_Type, "Refused") ~ "Refused",
    TRUE ~ NA_character_
  )) %>%
  select(-Cancer_Type)

# Geocode the state names to obtain their lat/long
geocoded_states <- state_fips %>%
  geocode(state, method = 'osm', full_results = FALSE)

# Correct the merge code
cancer_by_state_long <- cancer_by_state_long %>%
  left_join(state_fips, by = "STATE") %>%
  left_join(geocoded_states, by = "state")

# Load US states shapefile using tigris
us_states <- states(cb = TRUE, resolution = "20m")

# Simplify state names for merging
us_states$NAME <- tolower(us_states$NAME)
cancer_by_state_long$state <- tolower(cancer_by_state_long$state)

# Merge cancer data with the shapefile
us_states <- us_states %>%
  left_join(cancer_by_state_long, by = c("NAME" = "state"))

# Exclude Alaska and Hawaii for the main map
contiguous_us <- us_states %>%
  filter(!NAME %in% c("alaska", "hawaii", "puerto rico"))

alaska_data <- us_states %>% filter(NAME == "alaska")

hawaii_data <- us_states %>% filter(NAME == "hawaii")

# Set a theme
mf_theme("default", mar = c(0, 0, 0, 0))

# Create a base map for the contiguous US
mf_map(contiguous_us, var = "Proportion", type = "choro", 
       pal = "Dark Mint", leg_title = "Proportion", leg_pos = "bottomright")

# Create Alaska inset map, tripling its size
mf_inset_on(fig = c(0, 0.6, 0, 0.6))

mf_map(x = alaska_data)


# Handle single unique value for Alaska
if (length(unique(alaska_data$Proportion)) > 1) {
  mf_map(alaska_data, var = "Proportion", type = "choro", 
         pal = "Dark Mint", leg_title = NA, leg_pos = "none")
} else {
  alaska_value <- unique(alaska_data$Proportion)
  mf_map(alaska_data, col = alaska_value, border = "black", leg_title = NA, leg_pos = "none")
}

mf_inset_off()

# Create Hawaii inset map, slightly closer to Texas
mf_inset_on(fig = c(0.15, 0.25, 0.05, 0.15))

# Handle single unique value for Hawaii
if (length(unique(hawaii_data$Proportion)) > 1) {
  mf_map(hawaii_data, var = "Proportion", type = "choro", 
         pal = "Dark Mint", leg_title = NA, leg_pos = "none")
} else {
  hawaii_value <- unique(hawaii_data$Proportion)
  mf_map(hawaii_data, col = hawaii_value, border = "black", leg_title = NA, leg_pos = "none")
}

mf_inset_off()
# Load necessary libraries
library(here)
library(tidyverse)
library(survey)

# Reading in the 2022 BRFSS data
data_2022 <- readRDS(here("temp_data", "2022_data.rds"))
str(data_2022)
colnames(data_2022)

# Remove underscores from all variable names as they may cause issues in subsequent processing
data_2022 <- data_2022 %>% 
  rename_with(~str_remove(., "_"), starts_with("_"))

data_2022 <- data_2022 %>%
  select(STATE, STSTR, PSU, WT2RAKE, CNCRDIFF)

print(data_2022)

# Create a lookup table for state FIPS codes based on the 2022 codebook
state_fips <- tibble(
  "STATE" = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 66, 72, 78),
  state_name = as.factor(c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Guam", "Puerto Rico", "Virgin Islands"))
)

# Join BRFSS data with the state FIPS lookup table to add state names
data_2022 <- data_2022 %>% 
  left_join(state_fips, by = "STATE")

# Verify the levels of the state factor to ensure correct matching
levels(data_2022$state_name)

# Create a survey design object accounting for the complex survey design
data_2022_survey <- svydesign(
  id = ~PSU,                  # Primary Sampling Units (PSUs)
  strata = ~STSTR,            # Stratification variable
  weights = ~WT2RAKE,         # Weights variable
  data = data_2022,           # Data set
  nest = TRUE                 # Indicates nesting of the design
) 

# Recode the CNCRDIFF variable as a factor with descriptive labels for each level
data_2022$CNCRDIFF <- factor(data_2022$CNCRDIFF, levels = c(1, 2, 3, 7, 9), 
                             labels = c("One", "Two", "Three or more", "Don't know/Not Sure", "Refused"))

# Set survey options to adjust for lonely PSUs, which may have a single observation
options(survey.lonely.psu = "adjust")

# Recreate the survey design object with adjusted survey options
data_2022_survey_adjusted <- svydesign(
  id = ~PSU,
  strata = ~STSTR,
  weights = ~WT2RAKE,
  data = data_2022,
  nest = TRUE
)

# Calculate weighted COUNTS for the cancer type variable (CNCRDIFF) by state
cancer_counts_by_state <- svyby(
  ~CNCRDIFF,                     # Outcome variable
  ~STATE,                        # Grouping variable (State)
  data_2022_survey_adjusted,     # Survey design object
  svytotal,                      # Function to compute weighted totals
  na.rm = TRUE                   # Remove missing values
)

# Reshape data for easier merging
cancer_counts_long <- cancer_counts_by_state %>%
  pivot_longer(cols = starts_with("CNCRDIFF"), names_to = "Cancer_Type", values_to = "Count") %>%
  mutate(Type = case_when(
    str_detect(Cancer_Type, "One") ~ "One",
    str_detect(Cancer_Type, "Two") ~ "Two",
    str_detect(Cancer_Type, "Three.or.more") ~ "Three or more",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Type)) %>%
  select(-Cancer_Type)

# Join BRFSS data with the state FIPS lookup table to add state names
cancer_counts_long <- cancer_counts_long %>% 
  left_join(state_fips, by = "STATE")

# Calculate the "At least one" count per state by binning counts for "One," "Two," and "Three or more"
at_least_one_counts <- cancer_counts_long %>%
  group_by(STATE) %>%
  summarise(AtLeastOneCount = sum(Count, na.rm = TRUE))

# Calculate the total number of "At least one" cases in the U.S.
total_us_cases <- sum(at_least_one_counts$AtLeastOneCount, na.rm = TRUE)

# Calculate the proportion of "At least one" cases for each state relative to the U.S. total
at_least_one_counts <- at_least_one_counts %>%
  mutate(USPercent = (AtLeastOneCount / total_us_cases) * 100)

sum(at_least_one_counts$USPercent)  # Should be 100

# Merge the "At least one" data with the state FIPS lookup for full state names
at_least_one_counts <- at_least_one_counts %>%
  left_join(state_fips, by = c("STATE" = "STATE")) %>%
  select(1, 4, 2, 3)

print(at_least_one_counts)
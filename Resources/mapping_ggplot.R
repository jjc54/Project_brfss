# Load necessary libraries
library(tidyverse)
library(sf)
library(tigris)
library(PNWColors)

palette <- pnw_palette(name="Shuksan2",n=3,type="continuous")

# Load US states shapefile using tigris
# cb = TRUE uses a generalized (less detailed) version of the shapefile
# resolution = "20m" specifies the resolution for the boundaries
us_states <- states(cb = TRUE, resolution = "20m")

# Filter out non-contiguous states and territories
us_states <- us_states %>% 
  filter(!NAME %in% c("Guam", "Puerto Rico", "Virgin Islands"))

# Function to shift geometries
shift_geometry <- function(geometry, shift) {
  st_geometry(geometry) <- st_geometry(geometry) + shift
  geometry
}

# Apply shifts to Alaska and Hawaii
us_states_shifted <- us_states %>%
  mutate(geometry = case_when(
    NAME == "Alaska" ~ shift_geometry(st_shift_longitude(geometry), c(140, -100)),
    NAME == "Hawaii" ~ shift_geometry(geometry, c(100, -40)),
    TRUE ~ geometry
  ))

# Ensure state names are in a consistent format (lowercase)
us_states_shifted <- us_states_shifted %>%
  mutate(NAME = tolower(NAME))

# Assuming at_least_one_counts contains the "At least one" cancer category data
# Ensure consistency in state names in at_least_one_counts (lowercase)
at_least_one_counts <- at_least_one_counts %>%
  mutate(state = tolower(state))

# Merge the "At least one" data with the spatial data from the shapefile
us_states_shifted <- us_states_shifted %>%
  left_join(at_least_one_counts, by = c("NAME" = "state"))

# Plot the map for the "At least one" cancer category
# Plot the map with state abbreviations and a footnote
us_plot <- plot_usmap(data = us_data, values = "USPercent", regions = "states") +
  scale_fill_gradientn(
    colors = custom_palette,
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),  # Adjust positions of the colors
    na.value = "grey50",
    name = "US Percent"
  ) +
  geom_text(
    data = us_data, 
    aes(x = x, y = y, label = abbr), 
    color = "black", 
    size = 3, 
    fontface = "bold"
  ) +
  theme_void() +
  labs(
    title = "US Percent of At Least One Count by State",
    caption = "Source: CDC BRFSS Data, 2022"
  ) +
  theme(legend.position = "right")

# Display the final map
print(us_plot)

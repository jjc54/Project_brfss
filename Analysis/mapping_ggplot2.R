# Load necessary libraries
library(tidyverse)
library(maps)
library(usmap) 
library(PNWColors)
library(sf)

# Basic bar plot
ggplot(at_least_one_counts, aes(x = state_name, y = USPercent)) +
  geom_bar(stat = "identity", fill = "#9DBF9E") +
  labs(title = "US Percent of At Least One Count by State",
       x = "State",
       y = "US Percent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Issue - hard to read, not very informative


# Enter: Mapping
# Get the map data for the U.S.
states_map <- map_data("state")

# Ensure your state names are lowercase to match with the map data
at_least_one_counts <- at_least_one_counts %>%
  mutate(state_name = tolower(as.character(state_name)))

# Join the map data with your data
map_data <- states_map %>%
  left_join(at_least_one_counts, by = c("region" = "state_name"))

# Custom color palette for the Pacific Northwest
palette <- pnw_palette(name="Shuksan2",n=4,type="continuous")

ggplot(map_data, aes(x = long, y = lat, group = group, fill = USPercent)) +
  geom_polygon(color = "white") +
  scale_fill_gradientn(colors = palette, na.value = "grey50") +
  labs(title = "US Percent of At Least One Count by State",
       fill = "US Percent") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Issue - the "maps" package does not include non-contential U.S. (Hawaii, Alaska)
# Issue - hard to read 0 as blue


# Use `usmap`'s state data to create a map of the U.S. with Hawaii and Alaska
us_data <- us_map()

# Ensure your state names are lowercase to match with the map data
us_data <- us_data %>%
  mutate(full = tolower(as.character(full)))

# Join with your data
us_data <- us_data %>%
  left_join(at_least_one_counts, by = c("full" = "state_name"))

# Define the custom palette with grey for zero
palette_grey <- c("grey50", pnw_palette(name = "Shuksan2", n = 4, type = "continuous"))

# Plot the basic map using ggplot2 and usmap with the custom palette
us_plot <- plot_usmap(data = us_data, values = "USPercent", regions = "states") +
  scale_fill_gradientn(
    colors = custom_palette,
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),  # Adjust positions of the colors
    na.value = "grey50",
    name = "US Percent"
  ) +
  theme_void() +
  labs(title = "US Percent of At Least One Count by State") +
  theme(legend.position = "right")

# Display the final map
print(us_plot)

# Issue - which states are which?
# Issue - axes, data source, customization


# Calculate the centroids of the states
us_data_centroids <- st_centroid(us_data)

# Extract the coordinates from the centroids
us_data_centroids <- cbind(us_data_centroids, st_coordinates(us_data_centroids))

# Plot the map with state abbreviations and a footnote
us_plot <- plot_usmap(data = us_data, values = "USPercent", regions = "states") +
  scale_fill_gradientn(
    colors = custom_palette,
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),  # Adjust positions of the colors
    na.value = "grey50",
    name = "Percent of U.S. Cases (%)"
  ) +
  geom_text(
    data = us_data_centroids, 
    aes(x = X, y = Y, label = abbr), 
    color = "black", 
    size = 3, 
    fontface = "bold"
  ) +
  theme_void() +
  labs(
    title = "Which States Have the Highest Percentage of Cancer Cases?",
    caption = "Source: CDC BRFSS Data, 2022. Data represents the percentage of at least one cancer case by state compared to the entire U.S."
  ) +
  theme(legend.position = "right")

# Display the final map
print(us_plot)

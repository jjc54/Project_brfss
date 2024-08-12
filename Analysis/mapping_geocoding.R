# Load necessary libraries
library(here)
library(tidygeocoder)
library(tidyverse)

# Forward geocode the state names using OpenStreetMap (OSM)
geocoded_states <- state_fips %>%
  geocode(
    address = state_name,     # Column containing the state names
    method = 'osm',      # Geocoding service ('osm' for OpenStreetMap)
    full_results = FALSE # Only return basic results (latitude and longitude)
  )

# Print the geocoded results
print(geocoded_states)


# Example data frame with lat/long coordinates
coords <- data.frame(
  lat = c(34.0522, 40.7128, 37.7749),
  long = c(-118.2437, -74.0060, -122.4194)
)

# Reverse geocode the coordinates to obtain place names
reverse_geocoded <- coords %>%
  reverse_geocode(
    lat = lat,        # Latitude column
    long = long,      # Longitude column
    method = 'osm'    # Reverse geocoding service (OSM)
  )

# Print the reverse geocoded results
print(reverse_geocoded)
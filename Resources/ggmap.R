# Registering a Stadia Maps API Key
register_stadiamaps(key = "2b83d5c8-3bc3-4e6b-87e3-d1937efecec8", write = FALSE)

# Get a basemap using Stadia Maps with a valid maptype
us_basemap <- get_stadiamap(
  bbox = c(left = -125, bottom = 25, right = -66, top = 50),  # U.S. bounding box
  maptype = "stamen_terrain",  # Choose a valid maptype
  zoom = 4
)

# Transform the us_data to match the basemap projection
us_data_transformed <- st_transform(us_data, crs = st_crs(4326))
us_data_centroids_transformed <- st_transform(us_data_centroids, crs = st_crs(4326))

# Plot the map with the basemap, state abbreviations, and a footnote
us_plot <- ggmap(us_basemap) +
  geom_sf(data = us_data_transformed, aes(fill = USPercent), inherit.aes = FALSE, color = "white", lwd = 0.2) +
  scale_fill_gradientn(
    colors = custom_palette,
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),
    na.value = "grey50",
    name = "US Percent"
  ) +
  geom_text(
    data = us_data_centroids_transformed,
    aes(x = X, y = Y, label = abbr),
    color = "black",
    size = 3,
    fontface = "bold"
  ) +
  theme_void() +
  labs(
    title = "US Percent of At Least One Count by State",
    caption = "Source: CDC BRFSS Data, 2024"
  ) +
  theme(legend.position = "right")

# Display the final map
print(us_plot)
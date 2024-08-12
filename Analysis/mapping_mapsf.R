# Load necessary libraries
library(sf)
library(mapsf)
library(PNWColors)

# set a theme
mf_theme("agolalight")

# Start the mapsf plot with base map
mf_map(
  x = us_data, 
  var = "USPercent", 
  type = "choro",  # Choropleth map
  pal = custom_palette,  # Custom palette
  border = "white",  # State borders
  lwd = 0.5,  # Line width for borders
  leg_horiz = TRUE, # Horizontal legend
  leg_title = "Percent of U.S. Cases (%)",  # Legend title
  leg_val_rnd = 2,  # Rounding for legend values
  add = FALSE
)

# Add state abbreviations
mf_label(
  x = us_data_centroids, 
  var = "abbr", 
  col = "black", 
  cex = 0.5, 
  font = 2
)

# Add a title with flair
# Add a title without color adjustment
mf_title(
  txt = "Which States Have the Highest Percentage of Cancer Cases?",
  pos = "left",
  cex = 1.3,
  font = 3,
)

# Add a footnote/caption
mf_credits(
  txt = "Source: CDC BRFSS Data, 2022. Data represents the percentage of at least one cancer case by state compared to the entire U.S.",
  pos = "bottomleft",
  cex = 0.6,
  col = "black"
)
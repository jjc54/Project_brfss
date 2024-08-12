# Load necessary libraries
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(PNWColors)

# Load US states data from rnaturalearth
us_states <- ne_states(country = "united states of america", returnclass = "sf")

# Merge the "At least one" data with the spatial data
us_states <- us_states %>%
  left_join(at_least_one_counts, by = c("name" = "state"))

# Create a hover text column
us_states$hover <- paste0(us_states$name, "\nPercent of U.S.: ", round(us_states$USPercent, 2))

# Define graph properties
graph_properties <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("white"),
  projection = list(type = 'albers usa')
)

# Define font and label styling for hover
font <- list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label <- list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = font
)

# Create the Plotly choropleth map for the "At least one" category proportion
cancer_graph <- plot_geo(us_states, locationmode = "USA-states") %>%
  add_trace(
    locations = ~postal,                  # Use the postal column for state codes
    z = ~USPercent,
    zmin = 0,
    zmax = max(us_states$USPercent, na.rm = TRUE),
    color = ~USPercent,
    colorscale = palette,       # Use custom color scale
    text = ~hover,
    hoverinfo = 'text'
  ) %>%
  layout(
    geo = graph_properties,
    title = list(
      text = "Percent of U.S. At Least One Cancer Case by State",
      font = list(family = "DM Sans", size = 24)  # Increase title font size
    ),
    font = list(family = "DM Sans"),
    annotations = list(
      list(
        x = 0.5,
        y = -0.15, # Adjusted value to bring annotation closer to the plot
        text = "Source: BRFSS 2022. Note: Data represents the proportion of at least one cancer case.",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        font = list(size = 12)
      )
    ),
    margin = list(t = 100, b = 100)  # Increase bottom margin
  ) %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = label) %>%
  colorbar(
    title = "Percent of Cases (%)",          # Rename legend title
    tickprefix = ''
  )

cancer_graph

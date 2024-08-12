library(plotly)
library(sf)
library(tidyverse)

# Convert the geometry to a format compatible with plotly
us_data <- us_data %>%
  mutate(geometry = st_as_text(st_geometry(.))) %>%
  st_drop_geometry()

us_data <- us_data %>%
  mutate(hover = paste0(
    abbr, "<br>",
    "Percent of U.S. Cases: ", round(USPercent, 2), "%"
  ))

graph_properties <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Create the interactive map
interactive_map <- plot_geo(us_data, locationmode = "USA-states") %>%
  add_trace(
    locations = ~abbr,                  # Use the abbr column for state codes
    z = ~USPercent,
    zmin = 0,
    zmax = max(us_data$USPercent, na.rm = TRUE),
    color = ~USPercent,
    colorscale = palette,       # Use custom color scale
    text = ~hover,
    hoverinfo = 'text'
  ) %>%
  layout(
    geo = graph_properties,
    title = list(
      text = "Which States Have the Highest Percentage of Cancer Cases?",
      font = list(family = "DM Sans", size = 24)  # Increase title font size
    ),
    font = list(family = "DM Sans"),
    annotations = list(
      list(
        x = 0.5,
        y = -0.15, # Adjusted value to bring annotation closer to the plot
        text = "Source: CDC BRFSS Data, 2022. Data represents the percentage of at least one cancer case by state compared to the entire U.S.",
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
  colorbar(
    title = "Percent of U.S. Cases (%)",          # Rename legend title
    tickprefix = ''
  )

# Display the interactive map
interactive_map
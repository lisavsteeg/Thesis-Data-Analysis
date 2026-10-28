library(sf)
library(maptiles)
library(terra)
library(ggplot2)
library(ggspatial)
library(ggrepel)

# -----------------------------
# 1. Get basemap
# -----------------------------
locations_basemap <- get_tiles(
  locations_sf,
  provider = "OpenTopoMap",
  zoom = 13
)

# -----------------------------
# 2. Convert sf to coordinates
# -----------------------------
locations_sf_coords <- cbind(
  locations_sf,
  st_coordinates(locations_sf)
)

# -----------------------------
# 3. Plot (FIXED VERSION)
# -----------------------------
ggplot() +
  
  # Use terra raster directly (no as.raster conversion)
  layer_spatial(locations_basemap) +
  
  # Points
  geom_point(
    data = locations_sf_coords,
    aes(x = X, y = Y),
    color = "blue",
    size = 2
  ) +
  
  # Labels
  geom_text_repel(
    data = locations_sf_coords,
    aes(x = X, y = Y, label = locationName),
    size = 3
  ) +
  
  # Scale bar
  annotation_scale(location = "bl") +
  
  # North arrow
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  
  theme_minimal() +
  labs(
    title = "Camera Trap Locations",
    x = "Longitude",
    y = "Latitude"
  )

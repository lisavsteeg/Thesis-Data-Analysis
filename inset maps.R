library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(maptiles)
library(terra)
install.packages("patchwork")
library(patchwork)

# prepare camera points
locations_sf_coords <- cbind(
  locations_sf,
  st_coordinates(locations_sf)
)

# define zoomed study area 
bb <- st_bbox(locations_sf)

# shrink slightly for inset zoom
x_range <- bb$xmax - bb$xmin
y_range <- bb$ymax - bb$ymin

bb_inset <- bb
bb_inset["xmin"] <- bb$xmin + 0.15 * x_range
bb_inset["xmax"] <- bb$xmax - 0.15 * x_range
bb_inset["ymin"] <- bb$ymin + 0.15 * y_range
bb_inset["ymax"] <- bb$ymax - 0.15 * y_range

# overview map 
overview_map <- ggplot() +
  geom_sf(data = locations_sf, color = "red", size = 2) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Study Area Overview",
    x = "Longitude",
    y = "Latitude"
  )

# inset map
# Get tiles (no crop here)
inset_basemap <- get_tiles(
  locations_sf,
  provider = "OpenStreetMap",
  zoom = 13
)

# Crop AFTERWARD
inset_basemap <- crop(inset_basemap, bb_inset)

inset_map <- ggplot() +
  
  layer_spatial(inset_basemap) +
  
  geom_point(
    data = locations_sf_coords,
    aes(x = X, y = Y),
    color = "red",
    size = 2
  ) +
  
  geom_text_repel(
    data = locations_sf_coords,
    aes(x = X, y = Y, label = locationName),
    size = 3
  ) +
  
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr") +
  
  theme_minimal() +
  labs(
    title = "Camera Trap Locations (Zoomed)"
  )

#final map
final_map <- overview_map / inset_map +
  plot_layout(heights = c(1, 2))

final_map

overview_map +
  geom_rect(
    aes(
      xmin = bb_inset["xmin"],
      xmax = bb_inset["xmax"],
      ymin = bb_inset["ymin"],
      ymax = bb_inset["ymax"]
    ),
    fill = NA,
    color = "black",
    linewidth = 1
  )

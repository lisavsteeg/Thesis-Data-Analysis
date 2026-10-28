### Deployments ###

## working directory
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

## library ## 
library(devtools)
library(frictionless)
library(camtraptor)
library(ctdp)
library(sf)
library(maptiles)
library(vegan)
library(terra)
library(ggspatial)
library(ggplot2)
library(ggrepel)
library(pheatmap)

## deployments ## 
deployments <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/deployments.csv", stringsAsFactors=FALSE)
View(deployments)
head(deployments)

# Create a boolean vector, which is TRUE if a value of deployments$deploymentID has already appeared once.
dup_locations_bool <- duplicated(deployments$locationName)
dup_locations_bool 
length(unique(deployments$locationName)) #27 names, which is in accordance with the number of cameras in the field

# Subset the deployment using the dup_locations_bool boolean.

# subset columns by column name,
# keep only the placename, lat and long variables.
locations <- deployments[
  !dup_locations_bool,c("locationName","longitude", "latitude")] # the boolean is a vector of trues and falses, and it only keeps the 'TRUE' rows when you put it into the []
nrow(locations)

#Convert our dataframe to a "simple features" object.
#Specify the object to be converted, the columns and the CRS
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326,
                         remove=FALSE)# keep latitude and longitude columns

plot(locations_sf["locationName"])

# plot camera trap locations including names with ggplot #
locations_sf_coords <- cbind(locations_sf,
                             st_coordinates(locations_sf)
)

ggplot(locations_sf_coords) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = locationName)) +
  theme_minimal()

# plot camera trap locations on basemap # 
# Get basemap
locations_basemap <- get_tiles(locations_sf,
                               provider = "OpenTopoMap",
                               zoom = 13
)

# Convert sf to coordinates
locations_sf_coords <- cbind(locations_sf,
                             st_coordinates(locations_sf)
)

# Create wider longitude extent 
bb <- st_bbox(locations_sf)

x_expand <- 0.03
y_expand <- 0.01

bb_expanded <- bb

bb_expanded["xmin"] <- bb["xmin"] - x_expand
bb_expanded["xmax"] <- bb["xmax"] + x_expand
bb_expanded["ymin"] <- bb["ymin"] - y_expand
bb_expanded["ymax"] <- bb["ymax"] + y_expand

# Convert bbox to sf object
bb_sf <- st_as_sfc(bb_expanded)

# Get larger basemap
locations_basemap <- get_tiles(bb_sf,
                               provider = "OpenTopoMap",
                               zoom = 13
)

# Plot Camera Trap Locations
ggplot() +
  
  # Basemap 
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
  annotation_scale(
    location = "bl", 
    width_hint = 0.25
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Coordinate limits
  coord_sf(
    xlim = c(bb_expanded["xmin"], bb_expanded["xmax"]),
    ylim = c(bb_expanded["ymin"], bb_expanded["ymax"]),
    expand = FALSE
  ) +
  
  # Remove white margins around map
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0,0,0,0)
  ) +
  
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
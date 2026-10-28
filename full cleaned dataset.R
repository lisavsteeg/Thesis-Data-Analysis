## combine phase 2 + phase 3 

library(dplyr)
library(sf)
library(ggplot2)
library(maptiles)
library(ggspatial)
library(ggrepel)
library(lubridate)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)

## -------------------------------------------------------------------------- ##
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

deployments_phase_2 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 2 full dataset/deployments.csv")

deployments_phase_3 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 3 final dataset 2-9-26/deployments.csv")

head(deployments_phase_2)
head(deployments_phase_3)

deployments <- bind_rows(deployments_phase_2, deployments_phase_3)

## -------------------------------------------------------------------------- ##
# Create a boolean vector, which is TRUE if a value of deployments$deploymentID has already appeared once.
dup_locations_bool <- duplicated(deployments$locationName)
dup_locations_bool 
length(unique(deployments$locationName)) #44 names, which is in accordance with the number of cameras in the field

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


# plot camera trap locations including names with ggplot #
locations_sf_coords <- cbind(locations_sf,
                             st_coordinates(locations_sf)
)

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

# Plot Camera Trap Locations to check if all locations are there
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

###--------------------------------------------------------------------------###
## Kenya map
# Get countries
east_africa <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(
    name %in% c(
      "Kenya",
      "Tanzania",
      "Uganda",
      "S. Sudan",
      "Ethiopia",
      "Somalia"
    )
  )

# Separate Kenya
kenya <- east_africa %>%
  filter(name == "Kenya")

# Nairobi
nairobi_point <- st_as_sf(
  data.frame(
    name = "Nairobi",
    longitude = 36.8219,
    latitude = -1.2921
  ),
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Nairobi coordinates for label
nairobi_coords <- st_coordinates(nairobi_point)

# Plot
ggplot() +
  
  # Neighbouring countries
  geom_sf(
    data = east_africa,
    fill = "lightgrey",
    colour = "grey50",
    linewidth = 0.4
  ) +
  
  # Kenya
  geom_sf(
    data = kenya,
    fill = "#D4ECF4",
    colour = "grey30",
    linewidth = 0.6
  ) +
  
  # Camera locations
  geom_sf(
    data = locations_sf,
    colour = "#215FE7",
    size = 2
  ) +
  
  # Nairobi
  geom_sf(
    data = nairobi_point,
    colour = "black",
    size = 2
  ) +
  
  # Nairobi label
  annotate(
    "text",
    x = nairobi_coords[1] + 0.3,
    y = nairobi_coords[2] + 0.3,
    label = "Nairobi",
    size = 3.5
  ) +
  
  # Set map extent
  coord_sf(
    xlim = c(28, 43),
    ylim = c(-12, 8),
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )

### -------------------------------------------------------------------------###
# convert to proper datetime → date
deployments <- deployments %>%
  mutate(
    start_date = as.Date(ymd_hms(deploymentStart)),
    end_date = as.Date(ymd_hms(deploymentEnd))
  )

deployments <- deployments %>%
  mutate(
    trap_days  = as.numeric(end_date - start_date) + 1
  )

### -------------------------------------------------------------------------###
## delete wrong deployments
deployments_clean <- deployments %>%
  filter(!deploymentID %in% c("67725622-4e93-46a4-9e18-b978ebcf89be",
                              "d7116e1b-58d4-45da-98f0-026fe8950203",
                              "d3ea71a7-1409-4b20-8a84-88c779dd6d13",
                              "46957359-8a6a-4729-a161-592263e05c48",
                              "0af812b5-11e2-4e00-a763-93e0a3db4a44",
                              "d535113f-da88-4f03-9ca7-3d106bfa7381",
                              "76a7f4b7-46cc-4970-af96-179ba69e4882",
                              "84940881-7715-4b0b-8567-82595587e407",
                              "961aa375-1f31-4bc5-8ea7-b5e72f51231f",
                              "ad3e2048-c291-49bf-b3bf-7d7cb5a28f41",
                              "7a783829-d5f9-43fd-a304-5d82ac7089ef",
                              "b5df51ef-638c-4d4f-bf8e-d2ff072cde6f",
                              "eed26bb3-1c99-45f0-855c-d46582b367f4",
                              "c74a6ac6-e5c9-4829-90a2-38ae4850c07b",
                              "bfc4a07b-6a99-45e0-a6ac-6f149a3737be",
                              "7be1f497-6c81-4c74-b03a-26d291c7d0ea",
                              "2cbfbcc8-7a8e-4341-9386-609c2169994c",
                              "5853e587-afa2-4ec3-8e30-856b6c7c9c27",
                              "e1439185-2e5b-4fa0-b60f-ea29bdae6274",
                              "a63aa5b4-88d9-46dd-8d7b-2e311f55d805",
                              "6ca0d4b6-54f7-4927-9078-70f1087b422b",
                              "a8829da6-1d7b-478b-937f-618d79245892",
                              "b0dc5b7e-65fe-4e58-a3a8-e734c6938384",
                              "0e5aade3-0c31-42cf-b4ab-8321939a3de1",
                              "4210acfd-a831-4de1-b697-ac69ebbc197c", 
                              "2976da0a-47da-4b03-90e5-7a8e516b710d",
                              "43bbdb9f-f04a-4664-9e4a-64a91ead88f7",
                              "59f48907-50ab-4d2b-a78b-a7b9524b8c44",
                              "5c7afe6f-5917-45fe-aa9c-cda6ba532bd5",
                              "615ce467-3247-4792-a4f3-c54abdcfd01f",
                              "83490ce5-1a4c-41f9-9619-6b89a021dd84",
                              "9a0125d7-ad4b-48b4-8126-702066a58d76",
                              "a6c9f83d-a80e-4ea8-bdc5-553b6829873a",
                              "2f177b9e-b26f-4d25-88aa-b3c769dfbdd3",
                              "50ef1c83-3757-4f22-b066-6fdf869e3f47",
                              "7c8ca6c1-15d3-4503-8d18-48d8aad7198e",
                              "ad8214cf-2603-4d84-b5f8-b7f058c7cd3a",
                              "0d4bf4bc-c966-453a-93b7-8c152b98e599"
                            ))

range(deployments_clean$deploymentStart)
range(deployments_clean$deploymentEnd)

# Define study period
study_start <- as.Date("2023-10-20")
study_end <- max(deployments_clean$end_date, na.rm = TRUE)

# Create sequence of all dates in study period
all_dates <- seq(
  from = study_start,
  to = study_end,
  by = "day"
)

# Create daily camera operation dataset
camera_operation <- expand_grid(
  locationName = unique(deployments_clean$locationName),
  date = all_dates
) %>%
  
  # Match dates to deployment periods
  left_join(
    deployments_clean %>%
      dplyr::select(locationName, start_date, end_date),
    by = "locationName",
    relationship = "many-to-many"
  ) %>%
  
  # Determine whether there was a deployment on each date
  mutate(
    operational = if_else(
      date >= start_date & date <= end_date,
      1L,
      0L
    )
  ) %>%
  
  # Combine multiple deployments at the same camera/date
  group_by(locationName, date) %>%
  summarise(
    operational = if_else(
      any(operational == 1),
      1L,
      0L
    ),
    .groups = "drop"
  ) %>%
  
  # only assign NA during the field-removal period when there was NO deployment recorded
  mutate(
    operational = if_else(
      date >= as.Date("2025-11-25") &
        date <= as.Date("2026-02-10") &
        operational == 0,
      NA_integer_,
      operational
    )
  )

camera_operation_matrix <- camera_operation %>%
  pivot_wider(
    names_from = date,
    values_from = operational
  )

camera_operation_matrix

# get active and inactive days 
station_periods <- deployments_clean %>%
  group_by(locationName) %>%
  summarise(
    first_deployment = min(start_date),
    last_deployment = max(end_date),
    .groups = "drop"
  )

camera_operation <- camera_operation %>%
  left_join(station_periods, by = "locationName") %>%
  mutate(
    operational = if_else(
      date < first_deployment | date > last_deployment,
      NA_integer_,
      operational
    )
  )

camera_operation

n_stations <- n_distinct(camera_operation$locationName)

n_stations

n_days <- n_distinct(camera_operation$date)

n_days

active_trap_days <- sum(
  camera_operation$operational == 1,
  na.rm = TRUE
)

active_trap_days

station_summary <- camera_operation %>%
  group_by(locationName) %>%
  summarise(
    monitored_days = sum(!is.na(operational)),
    active_days = sum(operational == 1, na.rm = TRUE),
    inactive_days = sum(operational == 0, na.rm = TRUE),
    .groups = "drop"
  )

station_summary

# attach active_days to deployments_clean
deployments_clean <- deployments_clean %>%
  left_join(
    station_summary %>% dplyr::select(locationName, active_days),
    by = "locationName"
  )

head(deployments_clean %>% 
       dplyr::select(locationName, deploymentID, active_days))

deployments_clean %>%
  filter(is.na(active_days)) %>%
  distinct(locationName)

# remove locations with <40 active days
deployments_clean <- deployments_clean %>%
  filter(!locationName %in% c("CAR-35",
                              "CAR-36",
                              "CAR-38",
                              "CAR-43"
                              
  ))

range(deployments_clean$start_date, na.rm = TRUE)
range(deployments_clean$end_date, na.rm = TRUE)

## summary statistics
summary(deployments_clean$trap_days)
sum(deployments_clean$trap_days, na.rm = TRUE)
range(deployments_clean$trap_days, na.rm = TRUE)

deployments_clean %>%
  distinct(locationName, active_days) %>%
  summarise(total_active_days = sum(active_days, na.rm = TRUE))

## 141182 trap days in total 

### -------------------------------------------------------------------------###
## cleaning observations data ##
observations_phase_2 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 2 full dataset/observations.csv")

observations_phase_3 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 3 final dataset 2-9-26/observations.csv")

observations <- bind_rows(observations_phase_2, observations_phase_3)
head(observations)

# link the observation to the deployments, via the deploymentID
obs_deploy_index <- match(observations$deploymentID, deployments$deploymentID)

# join location to the observations dataframe
observations <- observations %>%
  left_join(
    deployments %>%
      dplyr::select(deploymentID, locationName),
    by = "deploymentID"
  )

head(observations) #extra column with locationName 

## fix year issues (checked years with Agouti)
year_corrections <- tibble::tribble(
  ~deploymentID, ~start_year, ~end_year,
  "2f3e2471-1963-445c-a3d4-9c9dde8a8e94", 2023, 2023,
  "31fd6467-8a46-47a3-a356-0dcde8c77f76", 2024, 2024,
  "d2df5af9-4cfe-4fcd-b018-000c049cc98e", 2023, 2023,
  "0018b853-4223-4fdb-91dc-f00d40ef44ad", 2026, 2026,
  "26a94005-fc52-4a9c-9772-211dce0dbb38", 2026, 2026,
  "2ac7565f-3b22-4109-934f-19b9c7a9ccd1", 2026, 2026,
  "6956b530-1370-42f2-b503-5eef40f12762", 2026, 2026,
  "818ef385-a2c4-45b4-a6b1-4630a97a9b8f", 2026, 2026,
  "9a5d76a7-a376-4dce-b801-1d2f3ee2bd56", 2026, 2026,
  "b80fbb99-36a1-4879-8788-88ef1d76115d", 2026, 2026,
  "d9937756-debe-459d-ac1f-6c3bca53b829", 2026, 2026,
  "ea859d96-3c7b-4030-8a89-2f7ea1c2483e", 2026, 2026,
  "250bc60d-9aaa-4041-8075-716ff435a6b1", 2026, 2026
)

observations <- observations %>%
  left_join(
    year_corrections,
    by = "deploymentID"
  ) %>%
  mutate(
    eventStart = if_else(
      !is.na(start_year),
      format(
        update(ymd_hms(eventStart), year = start_year),
        "%Y-%m-%dT%H:%M:%S%z"
      ),
      eventStart
    ),
    eventEnd = if_else(
      !is.na(end_year),
      format(
        update(ymd_hms(eventEnd), year = end_year),
        "%Y-%m-%dT%H:%M:%S%z"
      ),
      eventEnd
    )
  ) %>%
  dplyr::select(-start_year, -end_year)

### -------------------------------------------------------------------------###
## remove unnecessary rows 
# remove "unclassified" 
observations <- observations %>%
  filter(
    !observationType %in% c("unclassified"
    )
  )

# remove deployments that won't be used
observations <- observations %>%
  filter(!deploymentID %in% c("67725622-4e93-46a4-9e18-b978ebcf89be",
                              "d7116e1b-58d4-45da-98f0-026fe8950203",
                              "d3ea71a7-1409-4b20-8a84-88c779dd6d13",
                              "46957359-8a6a-4729-a161-592263e05c48",
                              "0af812b5-11e2-4e00-a763-93e0a3db4a44",
                              "d535113f-da88-4f03-9ca7-3d106bfa7381",
                              "76a7f4b7-46cc-4970-af96-179ba69e4882",
                              "84940881-7715-4b0b-8567-82595587e407",
                              "961aa375-1f31-4bc5-8ea7-b5e72f51231f",
                              "ad3e2048-c291-49bf-b3bf-7d7cb5a28f41",
                              "7a783829-d5f9-43fd-a304-5d82ac7089ef",
                              "b5df51ef-638c-4d4f-bf8e-d2ff072cde6f",
                              "eed26bb3-1c99-45f0-855c-d46582b367f4",
                              "c74a6ac6-e5c9-4829-90a2-38ae4850c07b",
                              "bfc4a07b-6a99-45e0-a6ac-6f149a3737be",
                              "7be1f497-6c81-4c74-b03a-26d291c7d0ea",
                              "2cbfbcc8-7a8e-4341-9386-609c2169994c",
                              "5853e587-afa2-4ec3-8e30-856b6c7c9c27",
                              "e1439185-2e5b-4fa0-b60f-ea29bdae6274",
                              "a63aa5b4-88d9-46dd-8d7b-2e311f55d805",
                              "6ca0d4b6-54f7-4927-9078-70f1087b422b",
                              "a8829da6-1d7b-478b-937f-618d79245892",
                              "b0dc5b7e-65fe-4e58-a3a8-e734c6938384",
                              "0e5aade3-0c31-42cf-b4ab-8321939a3de1",
                              "4210acfd-a831-4de1-b697-ac69ebbc197c", 
                              "2976da0a-47da-4b03-90e5-7a8e516b710d",
                              "43bbdb9f-f04a-4664-9e4a-64a91ead88f7",
                              "59f48907-50ab-4d2b-a78b-a7b9524b8c44",
                              "5c7afe6f-5917-45fe-aa9c-cda6ba532bd5",
                              "615ce467-3247-4792-a4f3-c54abdcfd01f",
                              "83490ce5-1a4c-41f9-9619-6b89a021dd84",
                              "9a0125d7-ad4b-48b4-8126-702066a58d76",
                              "a6c9f83d-a80e-4ea8-bdc5-553b6829873a",
                              "2f177b9e-b26f-4d25-88aa-b3c769dfbdd3",
                              "50ef1c83-3757-4f22-b066-6fdf869e3f47",
                              "7c8ca6c1-15d3-4503-8d18-48d8aad7198e",
                              "0d4bf4bc-c966-453a-93b7-8c152b98e599",
                              "ad8214cf-2603-4d84-b5f8-b7f058c7cd3a"))

head(observations)

# remove locations with <40 active days
observations <- observations %>%
  filter(!locationName %in% c("CAR-35",
                              "CAR-36",
                              "CAR-38",
                              "CAR-43"
  ))


# check remaining observations
View(observations)
nrow(observations)
unique(observations$observationType) #"blank", "animal", "human", "unknown", "vehicle"
table(observations$observationType)

# total of 140038 observations, 48004 animal observations (including livestock), 89720 blank images, 797 human images, 131 vehicles and 1386 unidentifiable 

## -------------------------------------------------------------------------- ##
## keep only useful columns and cameras 
observations <- observations[, c("observationID", "deploymentID", "eventID", "locationName", "eventStart", "eventEnd", "observationType", "scientificName", "count", "classificationProbability")]

head(observations)

# get only animal observations 
animal_observations <- observations %>%
  filter(observationType == "animal")

# get only mammal observations with body mass > 500 g (including livestock)
mammal_observations <- animal_observations %>%
  filter(
    !scientificName %in% c("Aves",
                           "Animalia",
                           "Buphagus erythrorhynchus",
                           "Rodentia",
                           "Carnivora",
                           "Columba livia",
                           "Insecta",
                           "Lamprotornis superbus",
                           "Lepidoptera",
                           "Canis lupus familiaris",
                           "Mammalia",
                           "Numida meleagris",
                           "Papilionoidea",
                           "Pternistis leucoscepus",
                           "Ardeotis kori",
                           "Sagittarius serpentarius",
                           "Raphicerus campestris",
                           "Chiroptera",
                           "Squamata",
                           "Tockus leucomelas",
                           "Xerus rutilus", 
                           "Felidae",
                           "Canis familiaris",
                           "Sciuridae",
                           "Varanus",
                           "Struthio camelus", 
                           "Martes melampus",
                           "Helogale parvula",
                           "Hippopotamus amphibius"
    )
  )

sort(unique(mammal_observations$scientificName)) #check all species to see if only mammals are in the data 

# check if counts are missing 
mammal_observations[!is.na(mammal_observations$scientificName) &
               is.na(mammal_observations$count), ]

sum(!is.na(mammal_observations$scientificName) & is.na(mammal_observations$count))

# assign 1 to NA values 
mammal_observations$count[is.na(mammal_observations$count)] <- 1

# remove uncertain observations
unique(mammal_observations$classificationProbability)

mammal_observations <- mammal_observations %>%
  filter(classificationProbability != 0.0)

unique(mammal_observations$classificationProbability)

# get livestock detection 
# Livestock = Bos taurus, Equus asinus, Capra hircus, Ovis aries 
livestock <- mammal_observations[
  mammal_observations$scientificName %in% c("Bos taurus",
                                            "Equus asinus",
                                            "Capra hircus",
                                            "Ovis aries"
  ),
]
head(livestock)
nrow(livestock)

wildlife <- mammal_observations %>%
  filter(
    !scientificName %in% c("Bos taurus",
                           "Equus asinus", 
                           "Capra hircus", 
                           "Ovis aries"
    )
  )

sort(unique(wildlife$scientificName)) #check all species to see if only mammals are in the data 

# change hystrix cristata to hystrix
wildlife <- wildlife %>%
  mutate(scientificName = ifelse(
    scientificName %in% c("Hystrix cristata"),
    "Hystrix",
    scientificName
  ))

sort(unique(wildlife$scientificName))

## -------------------------------------------------------------------------- ##
## add species traits 
diet_lookup <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/diet_lookup.csv", sep=";")

wildlife <- wildlife %>%
  left_join(
    diet_lookup,
    by = "scientificName"
  )

names(wildlife)

# check for species that didn't match
wildlife %>%
  filter(is.na(diet)) %>%
  distinct(scientificName)

## -------------------------------------------------------------------------- ##
# join active days to mammal dataset 
livestock <- livestock %>%
  left_join(
    station_summary %>%
      dplyr::select(locationName, active_days),
    by = "locationName"
  )

wildlife <- wildlife %>%
  left_join(
    station_summary %>%
      dplyr::select(locationName, active_days),
    by = "locationName"
  )

## save clean dataset 
write.csv(deployments_clean, "deployments_clean.csv", row.names = FALSE)

write.csv(livestock, "livestock.csv", row.names = FALSE)

write.csv(wildlife, "wildlife.csv", row.names = FALSE)


## camera trap days + capture rates ## 
# Capture rate = independent detections per unit effort

library(dplyr)
library(ggplot2)
library(sf)
library(ggrepel)
library(ggspatial)
library(lubridate)
library(maptiles)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

getwd()

## -------------------------------------------------------------------------- ##
## get camera trap locations from the deployments ## 
deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean")

livestock <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/livestock.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife.csv")

## -------------------------------------------------------------------------- ##
## livestock observations + capture rates 
length(unique(livestock$locationName)) #37 cameras have recorded livestock
length(unique(wildlife$locationName)) #40, all cameras have wildlife
unique(wildlife$locationName)

# Check if there are any missing values in the count column
sum(is.na(livestock$count))

station_summary <- deployments_clean %>%
  dplyr::select(locationName, active_days) %>%
  distinct()

### -------------------------------------------------------------------------###
## get independent livestock detections 
livestock_independent <- livestock %>%
  mutate(
    datetime = ymd_hms(eventStart)
  ) %>%
  arrange(locationName, datetime) %>%
  group_by(locationName) %>%
  mutate(
    time_diff = as.numeric(
      difftime(datetime, lag(datetime), units = "mins")
    ),
    detection_sequence = cumsum(
      is.na(time_diff) | time_diff >= 30
    )
  ) %>%
  group_by(locationName, detection_sequence) %>%
  summarise(
    max_count = max(count),
    .groups = "drop"
  )

detections_livestock_site <- livestock_independent %>%
  group_by(locationName) %>%
  summarise(
    independent_detections = n(),
    total_individuals = sum(max_count),
    .groups = "drop"
  )

detections_livestock_site %>%
  count(locationName) %>%
  filter(n > 1)

### -------------------------------------------------------------------------###
## Capture rate
# Capture rates were used as an index of relative species occurrence at camera-trap locations. Species with higher capture rates were detected more frequently per trap-night, suggesting greater local activity or abundance, whereas species with lower capture rates were detected less often and may have been less common, less active, or less detectable during the survey period

single_capture_rate <- detections_livestock_site %>%
  left_join(
    station_summary %>%
      dplyr::select(locationName, active_days),
    by = "locationName"
  ) %>%
  mutate(
    capture_rate_100 =
      (independent_detections / active_days) * 100
  )

single_capture_rate %>%
  count(locationName) %>%
  filter(n > 1)

nrow(single_capture_rate)
n_distinct(single_capture_rate$locationName)

### -------------------------------------------------------------------------###
## calculate occupancy time 
# get deployment duration in minutes
trap_effort <- deployments_clean %>%
  mutate(
    deployment_start = ymd_hms(deploymentStart),
    deployment_end = ymd_hms(deploymentEnd),
    deployment_minutes = as.numeric(
      difftime(
        deployment_end,
        deployment_start,
        units = "mins"
      )
    )
  ) %>%
  group_by(locationName) %>%
  summarise(
    trap_minutes = sum(deployment_minutes, na.rm = TRUE),
    .groups = "drop"
  )

# Convert observation timestamps
livestock <- livestock %>%
  mutate(
    datetime_start = ymd_hms(eventStart),
    datetime_end = ymd_hms(eventEnd)
  )

# Function to merge overlapping livestock observation intervals
merge_intervals <- function(df) {
  
  df <- df %>%
    filter(
      !is.na(datetime_start),
      !is.na(datetime_end),
      datetime_end >= datetime_start
    ) %>%
    arrange(datetime_start, datetime_end)
  
  if (nrow(df) == 0) {
    return(tibble(
      start = as.POSIXct(character()),
      end = as.POSIXct(character())
    ))
  }
  
  start_num <- as.numeric(df$datetime_start)
  end_num <- as.numeric(df$datetime_end)
  
  previous_max_end <- lag(cummax(end_num))
  
  interval_group <- cumsum(
    is.na(previous_max_end) |
      start_num > previous_max_end
  )
  
  df %>%
    mutate(
      interval_group = interval_group
    ) %>%
    group_by(interval_group) %>%
    summarise(
      start = min(datetime_start),
      end = max(datetime_end),
      .groups = "drop"
    )
}

# Merge overlapping livestock observations within each camera
livestock_intervals <- livestock %>%
  group_by(locationName) %>%
  group_modify(~ merge_intervals(.x)) %>%
  ungroup()

# Calculate total time livestock were present
livestock_time <- livestock_intervals %>%
  group_by(locationName) %>%
  summarise(
    livestock_minutes = sum(
      as.numeric(
        difftime(end, start, units = "mins")
      )
    ),
    .groups = "drop"
  )

# Calculate occupancy index
livestock_occupancy <- trap_effort %>%
  left_join(
    livestock_time,
    by = "locationName"
  ) %>%
  mutate(
    livestock_minutes = replace_na(
      livestock_minutes,
      0
    ),
    occupancy_index =
      livestock_minutes / trap_minutes,
    occupancy_percent =
      occupancy_index * 100
  )

## -------------------------------------------------------------------------- ##
## Livestock intensity index 
# shows how long livestock is present, how often they appear, how many animals were present
# (total livestock minutes*mean group size)/trap effort 
calculate_livestock_intensity <- function(df) {
  
  # Get all unique start and end times
  time_points <- sort(unique(c(
    df$datetime_start,
    df$datetime_end
  )))
  
  # If there are not enough time points to make an interval
  if (length(time_points) < 2) {
    return(tibble(
      livestock_individual_minutes = 0
    ))
  }
  
  # Create consecutive time intervals
  intervals <- tibble(
    interval_start = head(time_points, -1),
    interval_end   = tail(time_points, -1)
  ) %>%
    mutate(
      duration_minutes = as.numeric(
        difftime(
          interval_end,
          interval_start,
          units = "mins"
        )
      )
    ) %>%
    filter(duration_minutes > 0)
  
  # Calculate herd size for each interval
  interval_values <- bind_rows(
    lapply(seq_len(nrow(intervals)), function(i) {
      
      # Find livestock observations active during this interval
      active <- df %>%
        filter(
          datetime_start <= intervals$interval_start[i],
          datetime_end > intervals$interval_start[i]
        )
      
      # If no livestock is active, ignore this interval
      if (nrow(active) == 0) {
        return(NULL)
      }
      
      # For each species, use the maximum number observed
      # Then add the species together
      herd_size <- active %>%
        group_by(scientificName) %>%
        summarise(
          max_count = max(count),
          .groups = "drop"
        ) %>%
        summarise(
          herd_size = sum(max_count),
          .groups = "drop"
        ) %>%
        pull(herd_size)
      
      tibble(
        duration_minutes = intervals$duration_minutes[i],
        herd_size = herd_size
      )
    })
  )
  
  # If there were no active intervals
  if (nrow(interval_values) == 0) {
    return(tibble(
      livestock_individual_minutes = 0
    ))
  }
  
  # Calculate individual-minutes
  tibble(
    livestock_individual_minutes = sum(
      interval_values$herd_size *
        interval_values$duration_minutes,
      na.rm = TRUE
    )
  )
}

livestock_intensity <- livestock %>%
  group_by(locationName) %>%
  group_modify(~ calculate_livestock_intensity(.x)) %>%
  ungroup()

# compute LII
livestock_intensity <- trap_effort %>%
  left_join(
    livestock_intensity,
    by = "locationName"
  ) %>%
  mutate(
    livestock_individual_minutes = replace_na(
      livestock_individual_minutes, 0
    ),
    LII = livestock_individual_minutes / trap_minutes
  )

# this assumes big and small herds to be averaged together, timing and group size are decoupled, repeated detections are already compressed (coarse summary model) -> average herd size represents all time 

# preserves within site information, accounts for large and small herd sizes, avoids averaging bias, reflects true intensity of use

### -------------------------------------------------------------------------###
# create dataset with three metrics 
livestock_metrics <- deployments_clean %>%
  dplyr::select(locationName) %>%
  distinct() %>%
  left_join(
    single_capture_rate %>%
      dplyr::select(locationName, capture_rate_100),
    by = "locationName"
  ) %>%
  left_join(
    livestock_occupancy %>%
      dplyr::select(locationName, occupancy_index, occupancy_percent),
    by = "locationName"
  ) %>%
  left_join(
    livestock_intensity %>%
      dplyr::select(locationName, LII),
    by = "locationName"
  ) %>%
  mutate(
    capture_rate_100 = replace_na(capture_rate_100, 0),
    occupancy_index = replace_na(occupancy_index, 0),
    occupancy_percent = replace_na(occupancy_percent, 0),
    LII = replace_na(LII, 0)
  ) %>%
  arrange(as.numeric(gsub("CAR-", "", locationName)))

length(unique(livestock_metrics$locationName))

# test correlation
cor(
  livestock_metrics[, c(
    "capture_rate_100", 
    "occupancy_index", 
    "LII"
  )],
  use = "complete.obs",
  method = "spearman"
)

cor.test(
  livestock_metrics$capture_rate_100,
  livestock_metrics$occupancy_index,
  method = "spearman"
)

cor.test(
  livestock_metrics$capture_rate_100,
  livestock_metrics$LII,
  method = "spearman"
)

cor.test(
  livestock_metrics$occupancy_index,
  livestock_metrics$LII,
  method = "spearman"
)

## use only capture rate for classification 
# classify groups based on median
livestock_metrics <- livestock_metrics %>%
  mutate(
    livestock_group = ifelse(
      capture_rate_100 >= median(
        capture_rate_100,
        na.rm = TRUE
      ),
      "High livestock activity",
      "Low livestock activity"
    )
  )

table(livestock_metrics$livestock_group)

setdiff(
  unique(livestock_metrics$locationName),
  unique(livestock$locationName)
)

# Check capture-rate values within each group
livestock_metrics %>%
  group_by(livestock_group) %>%
  summarise(
    n_cameras = n(),
    min_capture_rate = min(capture_rate_100, na.rm = TRUE),
    median_capture_rate = median(capture_rate_100, na.rm = TRUE),
    max_capture_rate = max(capture_rate_100, na.rm = TRUE),
    .groups = "drop"
  )

camera_effort <- livestock_metrics %>%
  dplyr::select(
    locationName,
    capture_rate_100,
    livestock_group
  ) %>%
  left_join(
    station_summary %>%
      dplyr::select(
        locationName,
        active_days
      ),
    by = "locationName"
  )

# Check for duplicate camera locations
camera_effort %>%
  count(locationName) %>%
  filter(n > 1)

# Summarise survey effort
effort_summary <- camera_effort %>%
  group_by(livestock_group) %>%
  summarise(
    n_cameras = n(),
    total_active_days = sum(active_days, na.rm = TRUE),
    mean_active_days = mean(active_days, na.rm = TRUE),
    median_active_days = median(active_days, na.rm = TRUE),
    min_active_days = min(active_days, na.rm = TRUE),
    max_active_days = max(active_days, na.rm = TRUE),
    .groups = "drop"
  )

effort_summary

# 5745 active days for high livestock activity with 20 cameras, 8437 active days for cameras with low livestock activity.

# Test for differences in survey effort between groups
wilcox.test(
  active_days ~ livestock_group,
  data = camera_effort,
  exact = FALSE
)

# p-value = 0.0195

# Plot survey effort
ggplot(
  camera_effort,
  aes(
    x = livestock_group,
    y = active_days,
    fill = livestock_group
  )
) +
  geom_boxplot() +
  labs(
    x = "Livestock activity group",
    y = "Active camera days"
  ) +
  theme_minimal()

# add activity to wildlife data 
wildlife <- wildlife %>%
  left_join(
    livestock_metrics %>%
      dplyr::select(
        locationName,
        capture_rate_100,
        livestock_group
      ),
    by = "locationName"
  )

### -------------------------------------------------------------------------###
# visualize cameras on map 
locations_sf_map <- locations_sf_coords %>%
  left_join(
    livestock_metrics %>%
      dplyr::select(
        locationName,
        livestock_group,
        capture_rate_100
      ),
    by = "locationName"
  ) %>%
  filter(!is.na(livestock_group))

# Check classification
table(locations_sf_map$livestock_group)
table(locations_sf_map$livestock_group, useNA = "always")

ggplot() +
  
  layer_spatial(locations_basemap) +
  
  geom_point(
    data = locations_sf_map,
    aes(
      x = X,
      y = Y,
      color = livestock_group,
      size = capture_rate_100
    )
  ) +
  
  geom_text_repel(
    data = locations_sf_map,
    aes(
      x = X,
      y = Y,
      label = locationName
    ),
    size = 3
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  
  coord_sf(
    xlim = c(bb_expanded["xmin"], bb_expanded["xmax"]),
    ylim = c(bb_expanded["ymin"], bb_expanded["ymax"]),
    expand = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "High livestock activity" = "#003D29",
      "Low livestock activity" = "#9BD4B9"
    ),
    name = "Livestock activity"
  ) +
  
  scale_size_continuous(
    name = "Livestock capture rate",
    trans = "sqrt",
    range = c(2, 8)
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

summary(livestock_metrics$capture_rate_100)

### -------------------------------------------------------------------------###
## sensitivity analysis
# Standardise the three livestock activity metrics
livestock_metrics_sensitivity <- livestock_metrics %>%
  mutate(
    capture_z = as.numeric(scale(capture_rate_100)),
    occupancy_z = as.numeric(scale(occupancy_index)),
    LII_z = as.numeric(scale(LII)),
    
    livestock_activity_score = rowMeans(
      cbind(
        capture_z,
        occupancy_z,
        LII_z
      ),
      na.rm = FALSE
    ),
    
    composite_group = ifelse(
      livestock_activity_score >= median(
        livestock_activity_score,
        na.rm = TRUE
      ),
      "High livestock activity",
      "Low livestock activity"
    )
  )

# Compare composite and capture-rate classifications
classification_comparison <- livestock_metrics_sensitivity %>%
  dplyr::select(
    locationName,
    livestock_group,
    composite_group
  )

table(
  classification_comparison$livestock_group,
  classification_comparison$composite_group
)

# Percentage agreement
mean(
  classification_comparison$livestock_group ==
    classification_comparison$composite_group
)

# Cohen's kappa
irr::kappa2(
  classification_comparison %>%
    dplyr::select(
      livestock_group,
      composite_group
    )
)

###------------------------------------------------------------------------- ###
write.csv(deployments_clean, "deployments_complete.csv", row.names = FALSE)

write.csv(wildlife, "wildlife_complete.csv", row.names = FALSE)

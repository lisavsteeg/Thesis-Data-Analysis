### Camera trap operations matrix ###

library(camtrapR)
library(dplyr)
library(lubridate)
library(tidyr)
library(writexl)
library(ggplot2)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

deployments_phase_2 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 2 full dataset/deployments.csv")

deployments_phase_3 <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/phase 3 final dataset 2-9-26/deployments.csv")

head(deployments_phase_2)
head(deployments_phase_3)

deployments <- bind_rows(deployments_phase_2, deployments_phase_3)
unique(deployments$locationName)

# create columns start_date and end_date 
deployments <- deployments %>%
  mutate(
    start_date = as.Date(ymd_hms(deploymentStart)),
    end_date = as.Date(ymd_hms(deploymentEnd))
  )

class(deployments$start_date)
class(deployments$end_date)

camera_deployments <- deployments %>%
  dplyr::select(
    locationName,
    deploymentID,
    start_date,
    end_date
  )

### -------------------------------------------------------------------------###
# get deployments from the wrong years 
observations_2019_2020 <- camera_deployments %>%
  filter(
    year(start_date) %in% c(2019, 2020) |
      year(end_date) %in% c(2019, 2020)
  )

# deploymentID "0af812b5-11e2-4e00-a763-93e0a3db4a44", "46957359-8a6a-4729-a161-592263e05c48", "67725622-4e93-46a4-9e18-b978ebcf89be", "76a7f4b7-46cc-4970-af96-179ba69e4882", "84940881-7715-4b0b-8567-82595587e407", "961aa375-1f31-4bc5-8ea7-b5e72f51231f", "d3ea71a7-1409-4b20-8a84-88c779dd6d13", "d535113f-da88-4f03-9ca7-3d106bfa7381", "d7116e1b-58d4-45da-98f0-026fe8950203"
# remove wrong years 

# camera_deployments <- camera_deployments %>%
# filter(
#     !year(start_date) %in% c(2019, 2020),
#     !year(end_date) %in% c(2019, 2020)
#   )

range(camera_deployments$start_date, na.rm = TRUE)
range(camera_deployments$end_date, na.rm = TRUE)

### -------------------------------------------------------------------------###
# overlapping dates 
overlapping_deployments <- camera_deployments %>%
  dplyr::select(locationName, deploymentID, start_date, end_date) %>%
  inner_join(
    deployments %>%
      dplyr::select(locationName, deploymentID, start_date, end_date),
    by = "locationName",
    suffix = c("_1", "_2"),
    relationship = "many-to-many"
  ) %>%
  filter(
    deploymentID_1 != deploymentID_2,
    start_date_1 < end_date_2,
    end_date_1 > start_date_2
  ) %>%
  filter(deploymentID_1 < deploymentID_2) %>%
  arrange(locationName, start_date_1)

write_xlsx(
  overlapping_deployments,
  "overlapping_deployments.xlsx"
)

### -------------------------------------------------------------------------###
## deployment duration/ trap days (start and end date also included)
camera_deployments <- camera_deployments %>%
  mutate(
    trap_days = as.numeric(end_date - start_date) + 1
  )

# get long deployments 
deployments_60_days <- camera_deployments %>%
  filter(trap_days >= 60) %>%
  dplyr::select(locationName, deploymentID, start_date, end_date, trap_days) %>%
  arrange(
    as.numeric(sub("CAR-", "", locationName)),
    start_date
  )

write_xlsx(
  deployments_60_days,
  "deployment_60_days.xlsx"
)

### -------------------------------------------------------------------------###
## negative camera trap days
negative_deployments <- camera_deployments %>%
  filter(trap_days < 0) %>%
  dplyr::select(deploymentID, locationName, start_date, end_date, trap_days)

### -------------------------------------------------------------------------###
## camera trap operation matrix 
# remove the deployments from 2020-2026
camera_deployments <- camera_deployments %>%
  filter(!deploymentID %in% c("d535113f-da88-4f03-9ca7-3d106bfa7381", "76a7f4b7-46cc-4970-af96-179ba69e4882"))

# Define study period
study_start <- as.Date("2023-10-20")
study_end <- max(camera_deployments$end_date, na.rm = TRUE)

# Create sequence of all dates in study period
all_dates <- seq(
  from = study_start,
  to = study_end,
  by = "day"
)

# Create daily camera operation dataset
camera_operation <- expand_grid(
  locationName = unique(camera_deployments$locationName),
  date = all_dates
) %>%
  
  # Match dates to deployment periods
  left_join(
    camera_deployments %>%
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

# visualize active camera days 
head(camera_operation)
table(camera_operation$operational, useNA = "always")
range(camera_operation$date)
  
# "#005B44"
# "#B39CD0"

# Define camera order
camera_levels <- sort(unique(camera_operation$locationName))

# Prepare active camera data with numeric y positions
active <- camera_operation %>%
  filter(operational == 1) %>%
  mutate(
    camera_y = match(locationName, camera_levels)
  )

# Data for the single purple block
no_camera_period <- data.frame(
  start = as.Date("2025-11-25"),
  end = as.Date("2026-02-10"),
  ymin = 0.5,
  ymax = length(camera_levels) + 0.5,
  status = "Cameras not deployed"
)

# Create plot
ggplot() +
  
  # Purple block in the background
  geom_rect(
    data = no_camera_period,
    aes(
      xmin = start,
      xmax = end,
      ymin = ymin,
      ymax = ymax,
      fill = status
    )
  ) +
  
  # Green active camera days on top
  geom_tile(
    data = active,
    aes(
      x = date,
      y = camera_y,
      fill = "Active camera day"
    ),
    height = 0.7
  ) +
  
  # Colours and legend
  scale_fill_manual(
    values = c(
      "Active camera day" = "#005B44",
      "Cameras not deployed" = "#B39CD0"
    ),
    name = NULL
  ) +
  
  # Camera names
  scale_y_reverse(
    breaks = seq_along(camera_levels),
    labels = camera_levels
  ) +
  
  # Timeline
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Date",
    y = "Camera location"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

# get active and inactive days 
station_periods <- camera_deployments %>%
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

write_xlsx(
  station_summary,
  "station_summary.xlsx"
)

### -------------------------------------------------------------------------###
# check different cameras 
camera_deployments %>%
  filter(locationName == "CAR-15") %>%
  arrange(start_date) %>%
  dplyr::select(locationName, deploymentID, start_date, end_date)

# eed26bb3-1c99-45f0-855c-d46582b367f4 is wrong, actual deployment dates are 2025-09-11 --- 2025-09-18 
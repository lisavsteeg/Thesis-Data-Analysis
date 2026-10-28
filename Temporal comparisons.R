### ------------------------------------------------------------------------ ### 
## Temporal comparisons 
### -------------------------------------------------------------------------### 

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(patchwork)

### -------------------------------------------------------------------------### 
wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife_complete.csv")

livestock <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/livestock.csv")

deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean")

### -------------------------------------------------------------------------### 
## livestock presence/absence
ggplot(
  livestock %>%
    mutate(date = as.Date(datetime_start)) %>%
    filter(
      date >= as.Date("2023-10-20"),
      date <= as.Date("2026-07-01")
    ) %>%
    count(date),
  aes(x = date, y = n)
) +
  geom_line() +
  geom_point() +
  labs(
    x = "Date",
    y = "Number of livestock detections",
    title = "Livestock detections over time"
  ) +
  theme_classic()

### -------------------------------------------------------------------------### 
## independent livestock events 
livestock_independent <- livestock %>%
  mutate(
    datetime = ymd_hms(eventStart)
  ) %>%
  arrange(
    locationName,
    datetime
  ) %>%
  group_by(
    locationName
  ) %>%
  mutate(
    time_diff = as.numeric(
      difftime(
        datetime,
        lag(datetime),
        units = "mins"
      )
    ),
    detection_sequence = cumsum(
      is.na(time_diff) |
        time_diff >= 30
    )
  ) %>%
  group_by(
    locationName,
    detection_sequence
  ) %>%
  summarise(
    datetime_start = min(datetime),
    max_count = max(
      count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    date = as.Date(datetime_start)
  )

## daily livestock activity
livestock_daily <- livestock_independent %>%
  count(
    date,
    name = "livestock_detections"
  ) %>%
  complete(
    date = seq(
      min(date),
      max(date),
      by = "day"
    ),
    fill = list(
      livestock_detections = 0
    )
  )

# daily camera effort 
camera_operation <- camera_operation %>%
  semi_join(
    camera_info %>%
      dplyr::select(locationName),
    by = "locationName"
  )

# Calculate active camera-days per date and livestock activity group
camera_effort_daily <- camera_operation %>%
  group_by(
    locationName,
    date
  ) %>%
  summarise(
    active = any(
      camera_operation == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    camera_info %>%
      dplyr::select(
        locationName,
        livestock_group
      ),
    by = "locationName"
  ) %>%
  filter(
    !is.na(livestock_group)
  ) %>%
  group_by(
    date,
    livestock_group
  ) %>%
  summarise(
    active_camera_days = sum(active),
    .groups = "drop"
  )

# define 7-day periods 
study_start <- min(
  livestock_daily$date,
  na.rm = TRUE
)

# 7-day livestock activity
livestock_7day <- livestock_daily %>%
  mutate(
    period = floor(
      as.numeric(date - study_start) / 7
    ),
    week_start = study_start + period * 7,
    week_end = study_start + period * 7 + 6
  ) %>%
  group_by(
    period,
    week_start,
    week_end
  ) %>%
  summarise(
    livestock_detections = sum(
      livestock_detections,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# 7-day camera effort 
camera_effort_7day_group <- camera_effort_daily %>%
  mutate(
    period = floor(
      as.numeric(date - study_start) / 7
    )
  ) %>%
  group_by(
    period,
    livestock_group
  ) %>%
  summarise(
    active_camera_days = sum(
      active_camera_days,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# total camera effort 
camera_effort_7day_total <- camera_effort_7day_group %>%
  group_by(
    period
  ) %>%
  summarise(
    active_camera_days = sum(
      active_camera_days,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# livestock RAI
livestock_7day <- livestock_7day %>%
  left_join(
    camera_effort_7day_total,
    by = "period"
  ) %>%
  mutate(
    livestock_RAI =
      livestock_detections /
      active_camera_days * 100
  )

# species RAI
species_7day <- wildlife_independent %>%
  mutate(
    date = as.Date(datetime),
    period = floor(
      as.numeric(date - study_start) / 7
    )
  ) %>%
  group_by(
    period,
    livestock_group,
    scientificName,
    diet,
    feeding_guild
  ) %>%
  summarise(
    species_detections = n(),
    .groups = "drop"
  ) %>%
  right_join(
    camera_effort_7day_group,
    by = c(
      "period",
      "livestock_group"
    )
  ) %>%
  mutate(
    species_detections = replace_na(
      species_detections,
      0
    ),
    species_RAI =
      species_detections /
      active_camera_days * 100
  ) %>%
  left_join(
    livestock_7day %>%
      dplyr::select(
        period,
        week_start,
        week_end,
        livestock_RAI
      ),
    by = "period"
  )

# trophic groups
guild_7day <- wildlife_independent %>%
  mutate(
    date = as.Date(datetime),
    period = floor(
      as.numeric(date - study_start) / 7
    )
  ) %>%
  group_by(
    period,
    livestock_group,
    diet
  ) %>%
  summarise(
    guild_detections = n(),
    .groups = "drop"
  ) %>%
  right_join(
    camera_effort_7day_group,
    by = c(
      "period",
      "livestock_group"
    )
  ) %>%
  mutate(
    guild_detections = replace_na(
      guild_detections,
      0
    ),
    guild_RAI =
      guild_detections /
      active_camera_days * 100
  ) %>%
  left_join(
    livestock_7day %>%
      dplyr::select(
        period,
        week_start,
        week_end,
        livestock_RAI
      ),
    by = "period"
  )

# plot guilds 
plot_data_guild <- guild_7day %>%
  filter(
    !is.na(livestock_group),
    !is.na(diet)
  ) %>%
  dplyr::select(
    period,
    week_start,
    livestock_group,
    diet,
    guild_RAI,
    livestock_RAI
  )

### -------------------------------------------------------------------------### 
## carnivores
plot_carnivore <- guild_7day %>%
  filter(
    diet == "carnivore ",
    !is.na(livestock_group)
  ) %>%
  dplyr::select(
    week_start,
    livestock_group,
    guild_RAI,
    livestock_RAI
  )

ggplot(
  plot_omnivore,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = guild_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Carnivore RAI (High livestock activity)",
      "Low livestock activity" = "Carnivore RAI (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

### -------------------------------------------------------------------------### 
## omnivores 
plot_omnivore <- guild_7day %>%
  filter(
    diet == "omnivore ",
    !is.na(livestock_group)
  ) %>%
  dplyr::select(
    week_start,
    livestock_group,
    guild_RAI,
    livestock_RAI
  )

ggplot(
  plot_omnivore,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = guild_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Omnivore RAI (High livestock activity)",
      "Low livestock activity" = "Omnivore RAI (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

### -------------------------------------------------------------------------### 
## herbivores
 herbivore_guild_7day <- wildlife_independent %>%
   filter(
     diet == "herbivore"
   ) %>%
   mutate(
    date = as.Date(datetime),
    period = floor(
      as.numeric(date - study_start) / 7
    )
  ) %>%
  group_by(
    period,
    livestock_group,
    feeding_guild
  ) %>%
  summarise(
    guild_detections = n(),
    .groups = "drop"
  ) %>%
  right_join(
    camera_effort_7day_group,
    by = c(
      "period",
      "livestock_group"
    )
  ) %>%
  mutate(
    guild_detections = replace_na(
      guild_detections,
      0
    ),
    guild_RAI =
      guild_detections /
      active_camera_days * 100
  ) %>%
  left_join(
    livestock_7day %>%
      dplyr::select(
        period,
        week_start,
        week_end,
        livestock_RAI
      ),
    by = "period"
  )

# grazer 
plot_grazer <- herbivore_guild_7day %>%
  filter(
    feeding_guild == "grazer",
    !is.na(livestock_group)
  ) %>%
  dplyr::select(
    week_start,
    livestock_group,
    guild_RAI,
    livestock_RAI
  )

ggplot(
  plot_grazer,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = guild_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Grazer RAI (High livestock activity)",
      "Low livestock activity" = "Grazer RAI (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

# browser
plot_browser <- herbivore_guild_7day %>%
  filter(
    feeding_guild == "browser",
    !is.na(livestock_group)
  ) %>%
  dplyr::select(
    week_start,
    livestock_group,
    guild_RAI,
    livestock_RAI
  )

ggplot(
  plot_browser,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = guild_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Browser RAI (High livestock activity)",
      "Low livestock activity" = "Browser RAI (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

# mixed-feeder
plot_mixed_feeder <- herbivore_guild_7day %>%
  filter(
    feeding_guild == "mixed-feeder",
    !is.na(livestock_group)
  ) %>%
  dplyr::select(
    week_start,
    livestock_group,
    guild_RAI,
    livestock_RAI
  )

ggplot(
  plot_mixed_feeder,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = guild_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Mixed-feeder RAI (High livestock activity)",
      "Low livestock activity" = "Mixed-feeder RAI (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

### -------------------------------------------------------------------------### 
## body size 
body_size_7day <- wildlife_independent %>%
  filter(
    !is.na(body_size)
  ) %>%
  mutate(
    date = as.Date(datetime),
    period = floor(
      as.numeric(date - study_start) / 7
    )
  ) %>%
  group_by(
    period,
    livestock_group,
    body_size
  ) %>%
  summarise(
    body_size_detections = n(),
    .groups = "drop"
  ) %>%
  right_join(
    camera_effort_7day_group,
    by = c(
      "period",
      "livestock_group"
    )
  ) %>%
  mutate(
    body_size_detections = replace_na(
      body_size_detections,
      0
    ),
    body_size_RAI =
      body_size_detections /
      active_camera_days * 100
  ) %>%
  left_join(
    livestock_7day %>%
      dplyr::select(
        period,
        week_start,
        week_end,
        livestock_RAI
      ),
    by = "period"
  )

plot_data_body_size <- body_size_7day %>%
  filter(
    !is.na(livestock_group),
    !is.na(body_size)
  ) %>%
  dplyr::select(
    period,
    week_start,
    livestock_group,
    body_size,
    body_size_RAI,
    livestock_RAI
  )

plot_small <- plot_data_body_size %>%
  filter(
    body_size == "small "
  )

ggplot(
  plot_small,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = body_size_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Small mammals (High)",
      "Low livestock activity" = "Small mammals (Low)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

## medium
plot_medium <- plot_data_body_size %>%
  filter(
    body_size == "medium"
  )

ggplot(
  plot_medium,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = body_size_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Medium-sized mammals (High livestock activity)",
      "Low livestock activity" = "Medium-sized mammals (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()

## large
plot_large <- plot_data_body_size %>%
  filter(
    body_size == "large"
  )

ggplot(
  plot_large,
  aes(x = week_start)
) +
  geom_smooth(
    aes(
      y = livestock_RAI,
      colour = "Livestock"
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  geom_smooth(
    aes(
      y = body_size_RAI,
      colour = livestock_group,
      group = livestock_group
    ),
    method = "loess",
    se = FALSE,
    span = 0.5,
    linewidth = 1
  ) +
  scale_colour_manual(
    breaks = c(
      "Livestock",
      "High livestock activity",
      "Low livestock activity"
    ),
    values = c(
      "Livestock" = "#039590",
      "High livestock activity" = "#2F4858",
      "Low livestock activity" = "#F4A6B8"
    ),
    labels = c(
      "Livestock" = "Livestock RAI",
      "High livestock activity" = "Large-sized mammals (High livestock activity)",
      "Low livestock activity" = "Large-sized mammals (Low livestock activity)"
    )
  ) +
  labs(
    x = "Date",
    y = "RAI (independent detections per 100 active camera-days)",
    colour = NULL
  ) +
  theme_classic()








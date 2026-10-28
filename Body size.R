### ------------------------------------------------------------------------ ### 
### Body size 
### -------------------------------------------------------------------------### 

library(ggplot2)
library(dplyr)
library(ggdist)
library(car)
library(DHARMa)
library(emmeans)
library(ggsignif)
library(forcats)

### -------------------------------------------------------------------------### 
wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife_complete.csv")

### -------------------------------------------------------------------------### 
wildlife_independent <- wildlife %>%
  mutate(
    datetime = ymd_hms(eventStart)
  ) %>%
  arrange(locationName, scientificName, datetime) %>%
  group_by(locationName, scientificName) %>%
  mutate(
    time_diff = as.numeric(
      difftime(datetime, lag(datetime), units = "mins")
    ),
    detection_sequence = cumsum(
      is.na(time_diff) | time_diff >= 30
    )
  ) %>%
  group_by(locationName, scientificName, detection_sequence) %>%
  mutate(
    max_count = max(count, na.rm = TRUE),
    detection_start = min(datetime),
    detection_end = max(datetime)
  ) %>%
  slice_max(
    order_by = count,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    duration_minutes = as.numeric(
      difftime(detection_end, detection_start, units = "mins")
    )
  )

names(wildlife_independent)
unique(wildlife_independent$body_size)
# size_levels <- c("medium", "large", "small ")

### -------------------------------------------------------------------------### 
camera_info <- wildlife_independent %>% 
  dplyr::select( 
    locationName, 
    livestock_group, 
    active_days 
  ) %>% 
  distinct()

# Create all camera × diet combinations 
size_RAI <- camera_info %>% 
  tidyr::expand_grid(
    body_size = unique(wildlife_independent$body_size)
  ) %>% 
  left_join(
    wildlife_independent %>% 
      group_by(locationName, body_size) %>% 
      summarise(
        independent_detections = n(),
        .groups = "drop"
      ),
    by = c("locationName", "body_size")
  ) %>% 
  mutate(
    independent_detections = tidyr::replace_na(
      independent_detections, 0
    ),
    RAI = (independent_detections / active_days) * 100
  )

# check dataset 

# Number of observations per body-size class
size_RAI %>%
  count(body_size)

# Number of observations per camera
size_RAI %>%
  count(locationName)

# Number of cameras per livestock group
size_RAI %>%
  distinct(locationName, livestock_group) %>%
  count(livestock_group)

# Check body-size × livestock-group combinations
size_RAI %>%
  count(body_size, livestock_group)

## RAI summary
size_RAI %>% 
  group_by(livestock_group, body_size) %>% 
  summarise( 
    n = n(), 
    mean_RAI = mean(RAI), 
    median_RAI = median(RAI), 
    sd_RAI = sd(RAI), 
    .groups = "drop" 
  )

## model 
size_RAI_model <- glmmTMB(
  independent_detections ~
    livestock_group * body_size +
    offset(log(active_days)) +
    (1 | locationName),
  family = nbinom2,
  data = size_RAI
)

summary(size_RAI_model)

car::Anova(
  size_RAI_model,
  type = "II"
)

emm_size_RAI <- emmeans( 
  size_RAI_model, 
  ~ livestock_group * body_size, 
  type = "response" 
)

emm_size_RAI 

# High vs Low comparison within each size group 
pairs(
  emm_size_RAI,
  by = "body_size",
  type = "response",
  adjust = "holm"
)

## diagnostics
res_size <- DHARMa::simulateResiduals(
  size_RAI_model
)

plot(res_size)
DHARMa::testDispersion(res_size)
DHARMa::testZeroInflation(res_size)
DHARMa::testUniformity(res_size)
DHARMa::plotResiduals(
  res_size,
  size_RAI$active_days
)

### -------------------------------------------------------------------------###
# plot by body-size 
ggplot(
  size_RAI,
  aes(
    x = body_size,
    y = RAI,
    fill = livestock_group
  )
) +
  
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    aes(
      group = livestock_group
    ),
    shape = 21,
    colour = "black",
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    ),
    alpha = 0.5,
    size = 1.8
  ) +
  
  # Significant difference for small mammals
  geom_signif(
    annotations = "***",
    xmin = 2.8,
    xmax = 3.2,
    y_position = max(
      size_RAI$RAI[
        size_RAI$body_size == "small "
      ],
      na.rm = TRUE
    ) * 1.15,
    tip_length = 0.02,
    textsize = 4,
    vjust = 0.2
  ) +
  
  scale_fill_manual(
    values = c(
      "High livestock activity" = "#003D29",
      "Low livestock activity" = "#9BD4B9"
    )
  ) +
  
  labs(
    x = "Body-size class",
    y = "RAI (independent detections per 100 active days)",
    fill = "Livestock activity"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  theme_classic()

### -------------------------------------------------------------------------###
## species responsible for difference 
species_RAI <- camera_info %>%
  tidyr::expand_grid(
    scientificName = unique(wildlife_independent$scientificName)
  ) %>%
  left_join(
    wildlife_independent %>%
      group_by(locationName, scientificName) %>%
      summarise(
        independent_detections = n(),
        .groups = "drop"
      ),
    by = c("locationName", "scientificName")
  ) %>%
  mutate(
    independent_detections = tidyr::replace_na(
      independent_detections, 0
    ),
    RAI = independent_detections / active_days * 100
  ) %>%
  left_join(
    wildlife_independent %>%
      distinct(
        scientificName,
        feeding_group,
        body_size
      ),
    by = "scientificName"
  )

species_difference <- species_RAI %>%
  group_by(
    scientificName,
    feeding_group,
    body_size,
    livestock_group
  ) %>%
  summarise(
    mean_RAI = mean(RAI),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = livestock_group,
    values_from = mean_RAI
  ) %>%
  mutate(
    difference = `High livestock activity` -
      `Low livestock activity`
  ) %>%
  arrange(desc(abs(difference)))

species_difference %>%
  filter(body_size == "small ") %>%
  arrange(desc(abs(difference)))

species_difference <- species_difference %>%
  mutate(
    percent_change = (
      (`High livestock activity` - `Low livestock activity`) /
        `Low livestock activity`
    ) * 100
  )

species_difference %>%
  arrange(percent_change)

# plot species
species_plot <- species_difference %>%
  mutate(
    percent_change = (
      (`High livestock activity` - `Low livestock activity`) /
        `Low livestock activity`
    ) * 100,
    
    species_label = fct_reorder(
      scientificName,
      percent_change
    )
  )

ggplot(
  species_plot,
  aes(
    x = percent_change,
    y = species_label,
    colour = body_size
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "grey50"
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = percent_change,
      y = species_label,
      yend = species_label
    ),
    linewidth = 0.7,
    colour = "grey75"
  ) +
  geom_point(
    size = 3
  ) +
  facet_wrap(
    ~ feeding_group,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  scale_colour_manual(
    values = c(
      "small " = "#9BD4B9",
      "medium" = "#4F8A70",
      "large" = "#003D29"
    )
  ) +
  labs(
    x = "Change in RAI from low- to high-livestock activity sites",
    y = NULL,
    colour = "Body size"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom"
  )

ggplot(
  species_difference %>%
    mutate(
      species_label = forcats::fct_reorder(
        scientificName,
        difference
      )
    ),
  aes(
    x = difference,
    y = species_label,
    colour = body_size
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "grey50"
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = difference,
      y = species_label,
      yend = species_label
    ),
    colour = "grey80",
    linewidth = 0.7
  ) +
  geom_point(size = 3) +
  facet_grid(
    feeding_group ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_colour_manual(
    values = c(
      "small " = "#1C879E",
      "medium" = "#D4ECF4",
      "large" = "#332288"
    )
  ) +
  labs(
    x = "Difference in RAI (High − Low livestock activity)",
    y = NULL,
    colour = "Body size"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(face = "italic"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

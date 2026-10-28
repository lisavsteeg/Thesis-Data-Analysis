### ------------------------------------------------------------------------ ### 
### Trophic level  
### -------------------------------------------------------------------------### 

library(ggplot2)
library(dplyr)
library(ggdist)
library(car)
library(DHARMa)
library(emmeans)
library(glmmTMB)

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
unique(wildlife_independent$diet)
# diet_levels <- c("carnivore ", "herbivore", "omnivore ")

## creating feeding guild groups
wildlife %>%
  filter(diet == "herbivore") %>%
  distinct(scientificName)

wildlife %>%
  filter(diet == "carnivore ") %>%
  distinct(scientificName)

wildlife %>%
  filter(diet == "omnivore ") %>%
  distinct(scientificName)

wildlife %>%
  filter(body_size == "small ") %>%
  distinct(scientificName)

wildlife %>%
  filter(body_size == "medium") %>%
  distinct(scientificName)

wildlife %>%
  filter(body_size == "large") %>%
  distinct(scientificName)

# 17 herbivores, 14 carnivores, 11 omnivores, 20 small-sized, 12 medium-sized, 10 large-sized

wildlife_independent <- wildlife_independent %>%
  mutate(
    feeding_guild = case_when(
      scientificName %in% c(
        "Kobus ellipsiprymnus",
        "Equus quagga",
        "Connochaetes taurinus",
        "Phacochoerus africanus",
        "Syncerus caffer",
        "Redunca redunca", 
        "Oryx beisa", 
        "Lepus"
      ) ~ "grazer",
      
      scientificName %in% c(
        "Madoqua kirkii",
        "Giraffa tippelskirchi",
        "Tragelaphus imberbis",
        "Tragelaphus scriptus"
      ) ~ "browser",
      
      scientificName %in% c(
        "Aepyceros melampus",
        "Taurotragus oryx",
        "Loxodonta africana",
        "Nanger granti", 
        "Hystrix"
      ) ~ "mixed feeder",
      
      TRUE ~ NA_character_
    )
  )

wildlife_independent %>%
  distinct(scientificName, feeding_guild) %>%
  arrange(feeding_guild, scientificName)

wildlife_independent %>%
  distinct(scientificName, feeding_guild) %>%
  count(feeding_guild)

# 4 browser, 8 grazer, 5 mixed feeder

names(wildlife_independent)
head(wildlife_independent)

### -------------------------------------------------------------------------### 
# camera info
camera_info <- wildlife_independent %>% 
  dplyr::select( 
    locationName, 
    livestock_group, 
    active_days 
    ) %>% 
  distinct()

# create feeding groups
wildlife_independent <- wildlife_independent %>%
  mutate(
    feeding_group = case_when(
      feeding_guild == "grazer" ~ "Grazer",
      feeding_guild == "browser" ~ "Browser",
      feeding_guild == "mixed feeder" ~ "Mixed feeder",
      diet == "carnivore " ~ "Carnivore",
      diet == "omnivore " ~ "Omnivore",
      TRUE ~ NA_character_
    )
  )

# Check how many detections belong to each feeding group
wildlife_independent %>%
  count(feeding_group)

wildlife_independent %>%
  distinct(
    scientificName,
    diet,
    feeding_guild,
    feeding_group
  ) %>%
  arrange(feeding_group, scientificName)

# Check whether any species/detections remain unclassified
wildlife_independent %>%
  filter(is.na(feeding_group)) %>%
  dplyr::select(
    scientificName,
    diet,
    feeding_guild,
    feeding_group
  ) %>%
  distinct()

# Create camera × feeding-group dataset
guild_RAI <- camera_info %>%
  
  # Create one row for every camera × feeding-group combination
  tidyr::expand_grid(
    feeding_group = c(
      "Grazer",
      "Browser",
      "Mixed feeder",
      "Carnivore",
      "Omnivore"
    )
  ) %>%
  
  # Add the number of independent detections
  left_join(
    wildlife_independent %>%
      filter(!is.na(feeding_group)) %>%
      group_by(locationName, feeding_group) %>%
      summarise(
        independent_detections = n(),
        .groups = "drop"
      ),
    by = c("locationName", "feeding_group")
  ) %>%
  
  # Cameras with no detections for a feeding group get zero
  mutate(
    independent_detections = tidyr::replace_na(
      independent_detections,
      0
    ),
    
    # Detection rate per 100 active camera days
    RAI = (independent_detections / active_days) * 100
  )

guild_order <- c(
  "Grazer",
  "Browser",
  "Mixed feeder",
  "Carnivore",
  "Omnivore"
)

guild_RAI <- guild_RAI %>%
  mutate(
    feeding_group = factor(
      feeding_group,
      levels = guild_order
    )
  )

# Check sample sizes
guild_RAI %>%
  count(
    feeding_group,
    livestock_group
  )

guild_RAI %>%
  group_by(
    feeding_group,
    livestock_group
  ) %>%
  summarise(
    n_cameras = n(),
    mean_RAI = mean(RAI),
    median_RAI = median(RAI),
    .groups = "drop"
  )

# Plot RAI by feeding group and livestock activity
ggplot(
  guild_RAI,
  aes(
    x = feeding_group,
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
    aes(group = livestock_group),
    shape = 21,
    colour = "black",
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    ),
    alpha = 0.5,
    size = 1.8
  ) +
  
  # Browser: **
  annotate("segment",
           x = 1.8, xend = 2.2,
           y = 360, yend = 360) +
  annotate("segment",
           x = 1.8, xend = 1.8,
           y = 350, yend = 360) +
  annotate("segment",
           x = 2.2, xend = 2.2,
           y = 350, yend = 360) +
  annotate("text",
           x = 2,
           y = 375,
           label = "**",
           size = 5) +
  
  # Omnivore: *
  annotate("segment",
           x = 4.8, xend = 5.2,
           y = 205, yend = 205) +
  annotate("segment",
           x = 4.8, xend = 4.8,
           y = 195, yend = 205) +
  annotate("segment",
           x = 5.2, xend = 5.2,
           y = 195, yend = 205) +
  annotate("text",
           x = 5,
           y = 220,
           label = "*",
           size = 5) +
  
  scale_fill_manual(
    values = c(
      "High livestock activity" = "#003D29",
      "Low livestock activity" = "#9BD4B9"
    )
  ) +
  
  labs(
    x = "Diet group",
    y = "RAI (independent detections per 100 active days)",
    fill = "Livestock activity"
  ) +
  
  theme_classic()

###--------------------------------------------------------------------------###
# statistics
guild_RAI_model <- glmmTMB(
  independent_detections ~
    livestock_group * feeding_group +
    offset(log(active_days)) +
    (1 | locationName),
  family = nbinom2,
  data = guild_RAI
)

car::Anova(
  guild_RAI_model,
  type = "II"
)

emm_guild_RAI <- emmeans(
  guild_RAI_model,
  ~ livestock_group * feeding_group,
  type = "response"
)

emm_guild_RAI

# Compare high vs low livestock activity within each feeding group
pairs(
  emm_guild_RAI,
  by = "feeding_group",
  type = "response",
  adjust = "holm"
)

# Model diagnostics
res_guild <- DHARMa::simulateResiduals(
  guild_RAI_model
)

plot(res_guild)
DHARMa::testDispersion(res_guild)
DHARMa::testZeroInflation(res_guild)
DHARMa::testUniformity(res_guild)
DHARMa::plotResiduals(
  res_guild,
  guild_RAI$active_days
)

### -------------------------------------------------------------------------###

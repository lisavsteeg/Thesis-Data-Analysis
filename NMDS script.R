### ------------------------------------------------------------------------ ### 
### NMDS script 
### -------------------------------------------------------------------------### 

## Non-Metric Multidimensional Scaling (NMDS)
# NMDS takes complex, multivariate data and represents the relationships in a way that is easier for interpretation

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(vegan)      # for NMDS functions
library(tidyverse)  # for data wrangling and ggplot2
library(goeveg)     # for NMDS scree plot
library(ggrepel)
library(glmmTMB)
packageVersion("emmeans")
library(emmeans)
library(DHARMa)

deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife_complete.csv")

### -------------------------------------------------------------------------### 
# get independent detections for wildlife 
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

head(wildlife_independent)

### -------------------------------------------------------------------------### 
## visualizing community data
wildlife_plot <- wildlife_independent %>%
  group_by(livestock_group, scientificName) %>%
  summarise(
    total_individuals = sum(max_count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  wildlife_plot,
  aes(
    x = livestock_group,
    y = total_individuals,
    fill = scientificName
  )
) +
  geom_bar(
    position = "stack",
    stat = "identity"
  ) +
  labs(
    x = "Livestock activity group",
    y = "Total number of individuals",
    fill = "Species"
  ) +
  theme_classic()

### -------------------------------------------------------------------------### 
## community matrix and distance index
community_table <- wildlife_independent %>%
  group_by(locationName, scientificName) %>%
  summarise(
    independent_detections = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scientificName,
    values_from = independent_detections,
    values_fill = 0
  )

## Add active camera days
community_table <- community_table %>%
  left_join(
    wildlife_independent %>%
      distinct(locationName, active_days),
    by = "locationName"
  )

## Convert independent detections to detections per 100 active camera days
community_table <- community_table %>%
  mutate(
    across(
      -c(locationName, active_days),
      ~ .x / active_days * 100
    )
  )

## Create community matrix for NMDS
com_matrix <- community_table %>%
  column_to_rownames("locationName") %>%
  dplyr::select(-active_days)

## Check data
nrow(com_matrix)
n_distinct(wildlife_independent$locationName)

### -------------------------------------------------------------------------### 
## NMDS
nmds <- metaMDS(
  com_matrix,
  distance = "bray",
  k = 2
)

nmds_stress <- nmds$stress

print(nmds)
# stress value below 0.2, which is acceptable but not the best 

stressplot(nmds)

dimcheck_out <- 
  dimcheckMDS(com_matrix,
              distance = "bray",
              k = 6)

## create ordination
plot(nmds)

# shows locations and species (not sure which is which)

# get livestock activity 
livestock_activity <- wildlife_independent %>% 
  distinct(locationName, livestock_group) 

# Extract NMDS scores for sites 
nmds_SiteScores <- as.data.frame(
  scores(nmds, display = "sites")
) %>%
  rownames_to_column(var = "locationName") %>%
  left_join(
    livestock_activity,
    by = "locationName"
  )

# check if every camera got a group
nmds_SiteScores %>%
  filter(is.na(livestock_group))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores <- as.data.frame(scores(nmds, display = "species"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

### -------------------------------------------------------------------------### 
## visualize NMDS
ggplot() + 
  
  geom_point(
    data = nmds_SiteScores, 
    aes(
      x = NMDS1, 
      y = NMDS2, 
      colour = livestock_group
    ), 
    size = 2
  ) + 
  
  geom_text(
    data = nmds_SpeciesScores, 
    aes(
      x = NMDS1, 
      y = NMDS2, 
      label = species
    )
  ) +
  
  theme_classic()

# get centroid 
Habitat_Centroid <- 
  nmds_SiteScores %>% 
  group_by(livestock_group) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
habitat.hull <- 
  nmds_SiteScores %>% 
  group_by(livestock_group) %>%
  slice(chull(NMDS1, NMDS2))

nmds_stress <- nmds$stress

# use ggplot to plot 
ggplot() + 
  
  # Site scores
  geom_point(
    data = nmds_SiteScores, 
    aes(
      x = NMDS1, 
      y = NMDS2, 
      colour = livestock_group
    ), 
    size = 2
  ) + 
  
  # Species scores
  geom_text(
    data = nmds_SpeciesScores, 
    aes(
      x = NMDS1, 
      y = NMDS2, 
      label = species
    )
  ) +
  
  # Centroids
  geom_point(
    data = Habitat_Centroid, 
    aes(
      x = axis1, 
      y = axis2, 
      colour = livestock_group
    ), 
    size = 5, 
    shape = 17
  ) +
  
  # Convex hulls
  geom_polygon(
    data = habitat.hull, 
    aes(
      x = NMDS1, 
      y = NMDS2, 
      fill = livestock_group, 
      group = livestock_group
    ), 
    alpha = 0.30
  ) +
  
  # Stress value
  annotate(
    "text", 
    x = 0.75, 
    y = 0.65, 
    label = paste(
      "2d stress =", 
      round(nmds_stress, 3)
    )
  ) +
  
  labs(
    x = "NMDS1", 
    y = "NMDS2"
  ) + 
  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 0.5
    ),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    legend.key.size = unit(0.25, "cm")
  )

# better plot
ggplot() +
  
  # 95% confidence ellipses
  stat_ellipse(
    data = nmds_SiteScores,
    aes(
      x = NMDS1,
      y = NMDS2,
      colour = livestock_group
    ),
    type = "t",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  
  # Site scores
  geom_point(
    data = nmds_SiteScores,
    aes(
      x = NMDS1,
      y = NMDS2,
      colour = livestock_group
    ),
    size = 3
  ) +
  
  # Species labels
  geom_text_repel(
    data = nmds_SpeciesScores,
    aes(x = NMDS1, y = NMDS2, label = species),
    size = 3,
    segment.color = NA,
    box.padding = 0.4,
    point.padding = 0.2
  ) +
  
  # Manual colours
  scale_colour_manual(
    values = c(
      "Low livestock activity" = "#9BD4B9",   
      "High livestock activity" = "#003D29"   
    )
  ) +
  
  # Stress value
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste("Stress =", round(nmds_stress, 3)),
    hjust = 1.1,
    vjust = 1.2,
    size = 4
  ) +
  
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "Livestock activity"
  ) +
  
  coord_equal() +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank()
  )

### -------------------------------------------------------------------------### 
## Statistics 
livestock_activity <- wildlife_independent %>% 
  distinct(locationName, livestock_group) %>%
  filter(locationName %in% rownames(com_matrix)) %>%
  slice(match(rownames(com_matrix), locationName))

all(livestock_activity$locationName == rownames(com_matrix))

## PERMANOVA
adonis2(
  com_matrix ~ livestock_group,
  data = livestock_activity,
  method = "bray"
)

## Check homogeneity of multivariate dispersion
bray_dist <- vegdist(
  com_matrix,
  method = "bray"
)

dispersion <- betadisper(
  bray_dist,
  livestock_activity$livestock_group
)

## Visualise distances to centroid
boxplot(
  dispersion,
  xlab = "Livestock activity",
  ylab = "Distance to centroid"
)

## Test for differences in dispersion
permutest(dispersion)

## Ordination of dispersion
plot(dispersion)

### -------------------------------------------------------------------------### 
## SIMPER analysis 
simper_result <- simper(
  com_matrix,
  livestock_activity$livestock_group,
  permutations = 999
)

# Inspect results
summary(simper_result)
names(simper_result)

# Extract the High vs Low comparison
simper_df <- as.data.frame(simper_result[[1]])

# Add species names
simper_df$species <- rownames(simper_df)

# Calculate contribution (%) and direction
simper_df <- simper_df %>%
  dplyr::mutate(
    contribution_percent = average / overall * 100,
    direction = dplyr::case_when(
      avb > ava ~ "Higher in High livestock activity",
      ava > avb ~ "Higher in Low livestock activity",
      TRUE ~ "Similar"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(average))

# Inspect the top 20 contributors
head(simper_df, 20)

# Select the 10 species contributing most
simper_top10 <- simper_df %>%
  dplyr::slice_max(
    order_by = average,
    n = 10
  )

# Plot top 10 contributors
ggplot(
  simper_top10,
  aes(
    x = contribution_percent,
    y = reorder(species, contribution_percent),
    fill = direction
  )
) +
  geom_col() +
  
  scale_fill_manual(
    values = c(
      "Higher in High livestock activity" = "#003D29",
      "Higher in Low livestock activity" = "#9BD4B9"
    )
  ) +
  
  labs(
    x = "Contribution to Bray–Curtis dissimilarity (%)",
    y = NULL,
    fill = "Higher mean relative abundance"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right"
  )

### -------------------------------------------------------------------------###
## testing mean relative abundance 
# Create one row for every camera location
camera_info <- wildlife_independent %>%
  dplyr::select(
    locationName,
    livestock_group,
    active_days
  ) %>%
  distinct()

wildlife_RAI <- wildlife_independent %>%
  group_by(locationName, livestock_group) %>%
  summarise(
    independent_detections = n(),
    active_days = first(active_days),
    .groups = "drop"
  ) %>%
  mutate(
    RAI = independent_detections / active_days * 100
  )

ggplot(wildlife_RAI,
       aes(x = livestock_group, y = RAI, fill = livestock_group)) +
  geom_boxplot(color = "black", alpha = 1.0, outlier.shape = NA) +
  geom_jitter(aes(color = livestock_group),
              width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c(
    "High livestock activity" = "#003D29",
    "Low livestock activity" = "#9BD4B9"
  )) +
  scale_color_manual(values = c(
    "High livestock activity" = "#003D29",
    "Low livestock activity" = "#9BD4B9"
  )) +
  labs(
    x = "Livestock activity",
    y = "Wild mammal RAI"
  ) +
  theme_classic() +
  theme(legend.position = "none")

wildlife_RAI %>%
  group_by(livestock_group) %>%
  summarise(
    n = n(),
    mean_RAI = mean(RAI),
    median_RAI = median(RAI),
    sd_RAI = sd(RAI),
    min_RAI = min(RAI),
    max_RAI = max(RAI)
  )

sum(wildlife_RAI$RAI == 0)
summary(wildlife_RAI$RAI)

rai_model <- glmmTMB(
  independent_detections ~ livestock_group +
    offset(log(active_days)),
  family = nbinom2,
  data = wildlife_RAI
)

summary(rai_model)

emm_RAI <- emmeans(
  rai_model,
  ~ livestock_group,
  type = "response",
  offset = log(100)
)

emm_RAI

emm_RAI

rai_sim <- simulateResiduals(rai_model)

plot(rai_sim)

testDispersion(rai_sim)
testUniformity(rai_sim)
testZeroInflation(rai_sim)

### -------------------------------------------------------------------------###

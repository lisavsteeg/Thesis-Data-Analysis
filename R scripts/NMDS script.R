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

deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife.csv")

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
head(wildlife)

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
## Community matrix based on independent detection sequences
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
  select(-active_days)

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
      "Low livestock activity" = "#F4B6C2",   
      "High livestock activity" = "#2F4858"   
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
write.csv(wildlife, "wildlife.csv", row.names = FALSE)

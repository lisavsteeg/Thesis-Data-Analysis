### ------------------------------------------------------------------------ ### 
### using iNEXT to correct for sampling effort 
library(dplyr)
library(tidyr)
library(iNEXT)
library(ggplot2)
library(patchwork)


library(dplyr)
library(tidyr)
library(tibble)
packageVersion("iNEXT")
packageDescription(
  "emmeans",
  fields = c("Version", "Date", "Date/Publication", "Packaged", "Built")
)
library(iNEXT)
library(ggplot2)
library(patchwork)
library(lme4)
library(MASS)
library(DHARMa)
library(glmmTMB)
library(emmeans)


# estimate how species richness accumulates with increasing sampling effort, and standardize comparisons so that differences aren't simply due to one camera/group having more detections than another

### -------------------------------------------------------------------------###
# get data 
deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife_complete.csv")

### -------------------------------------------------------------------------###
# rarefaction is a technique used to estimate and compare species diversity across samples of varying sizes
# Extrapolation is the process of using known data, facts, or trends to make an educated guess or prediction about an unknown situation that lies outside of your current observations

# sample coverage = proportion of the community that has effectively been sampled
# Livestock presence increased species richness (q = 0), but differences were weaker for Shannon (q = 1) and Simpson diversity (q = 2), suggesting that the additional species were primarily rare species
# Higher diversity in livestock areas across q = 0, 1, and 2, indicating differences in both richness and community structure

### -------------------------------------------------------------------------###
head(wildlife)

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

livestock_colors <- c(
  "High livestock activity" = "#003D29",
  "Low livestock activity" = "#9BD4B9"
)

camera_species_incidence <- wildlife_independent %>%
  group_by(
    locationName,
    livestock_group,
    scientificName
  ) %>%
  summarise(
    detected = 1,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scientificName,
    values_from = detected,
    values_fill = 0
  )

###--------------------------------------------------------------------------###
high <- camera_species_incidence %>%
  filter(
    livestock_group == "High livestock activity"
  )

low <- camera_species_incidence %>%
  filter(
    livestock_group == "Low livestock activity"
  )

nrow(high)
nrow(low)

# incidence matrices 
high_mat <- high %>%
  dplyr::select(
    -locationName,
    -livestock_group
  ) %>%
  as.matrix() %>%
  t()

low_mat <- low %>%
  dplyr::select(
    -locationName,
    -livestock_group
  ) %>%
  as.matrix() %>%
  t()

incidence_list <- list(
  "High livestock activity" = high_mat,
  "Low livestock activity" = low_mat
)

out0 <- iNEXT(
  incidence_list,
  datatype = "incidence_raw",
  q = 0,
  nboot = 1000
)

out1 <- iNEXT(
  incidence_list,
  datatype = "incidence_raw",
  q = 1,
  nboot = 1000
)

out2 <- iNEXT(
  incidence_list,
  datatype = "incidence_raw",
  q = 2,
  nboot = 1000
)

p0 <- ggiNEXT(out0, type = 1) +
  labs(
    title = "Species richness (q = 0)",
    x = "Number of camera locations",
    y = "Species richness"
  ) +
  scale_color_manual(values = livestock_colors) +
  scale_fill_manual(values = livestock_colors) +
  theme_classic()

p1 <- ggiNEXT(out1, type = 1) +
  labs(
    title = "Shannon diversity (q = 1)",
    x = "Number of camera locations",
    y = "Shannon diversity"
  ) +
  scale_color_manual(values = livestock_colors) +
  scale_fill_manual(values = livestock_colors) +
  theme_classic()

p2 <- ggiNEXT(out2, type = 1) +
  labs(
    title = "Simpson diversity (q = 2)",
    x = "Number of camera locations",
    y = "Simpson diversity"
  ) +
  scale_color_manual(values = livestock_colors) +
  scale_fill_manual(values = livestock_colors) +
  theme_classic()

(p0 + p1 + p2) +
  plot_layout(guides = "collect")

###--------------------------------------------------------------------------###


## Relative Abundance Indices (RAI) ## 
# compare wildlife abundance and group size between areas with and without livestock 

library(dplyr)
library(vegan)
library(lubridate)
library(MASS)
library(ggplot2)
library(lme4)
library(broom.mixed)
library(car)

## -----------------------------------------------------------------------------
## Load data

deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife.csv")

## -----------------------------------------------------------------------------
## Create site-level livestock lookup and survey effort

# create livestock lookup
livestock_lookup <- wildlife %>%
  group_by(locationName) %>%
  summarise(
    livestock_present = first(livestock_present),
    .groups = "drop"
  )

# Total trap effort per camera
site_effort <- deployments_clean %>%
  group_by(locationName) %>%
  summarise(
    trap_days = sum(trap_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(livestock_lookup, by = "locationName")

## -----------------------------------------------------------------------------
## Detection-based Relative Abundance Index (RAI)

# Define independent wildlife detection events (30-minute threshold)
wildlife_independent <- wildlife %>%
  mutate(datetime = ymd_hms(datetime_start)) %>%
  arrange(locationName, scientificName, datetime) %>%
  group_by(locationName, scientificName) %>%
  mutate(
    time_diff = as.numeric(
      difftime(datetime, lag(datetime), units = "mins")
    ),
    independent_event = if_else(
      is.na(time_diff) | time_diff > 30,
      1,
      0
    ),
    event_id = cumsum(independent_event)
  ) %>%
  ungroup()

# Count independent detections
wildlife_events <- wildlife_independent %>%
  distinct(
    locationName,
    scientificName,
    diet,
    event_id
  ) %>%
  group_by(
    locationName,
    scientificName,
    diet
  ) %>%
  summarise(
    independent_detections = n(),
    .groups = "drop"
  )

# Calculate RAI
wildlife_rai <- wildlife_events %>%
  left_join(site_effort, by = "locationName") %>%
  mutate(
    RAI_100 = independent_detections / trap_days * 100
  )

# Species-level RAI
species_rai <- wildlife_rai %>%
  group_by(scientificName, livestock_present) %>%
  summarise(
    independent_detections = sum(independent_detections),
    trap_days = sum(trap_days),
    RAI_100 = independent_detections / trap_days * 100,
    .groups = "drop"
  )

## -----------------------------------------------------------------------------
## Statistical comparison of detection-based RAI

site_rai <- wildlife_rai %>%
  group_by(locationName, livestock_present, trap_days) %>%
  summarise(
    detections = sum(independent_detections),
    .groups = "drop"
  ) %>%
  mutate(
    RAI_100 = detections / trap_days * 100
  )

# Sample sizes
table(site_rai$livestock_present)

# Wilcoxon rank-sum test
wilcox.test(
  RAI_100 ~ livestock_present,
  data = site_rai
)

# One-way ANOVA
rai_aov <- aov(
  RAI_100 ~ livestock_present,
  data = site_rai
)

summary(rai_aov)

# Check ANOVA assumptions
plot(rai_aov)
shapiro.test(residuals(rai_aov))

# Poisson GLM
rai_glm <- glm(
  detections ~ livestock_present +
    offset(log(trap_days)),
  family = poisson,
  data = site_rai
)

summary(rai_glm)

# Negative binomial GLM
rai_nb <- glm.nb(
  detections ~ livestock_present +
    offset(log(trap_days)),
  data = site_rai
)

summary(rai_nb)

## -----------------------------------------------------------------------------
## Group size analysis

# Maximum number of individuals per independent event
event_counts <- wildlife_independent %>%
  group_by(locationName, scientificName, event_id) %>%
  summarise(
    group_size = max(count, na.rm = TRUE),
    .groups = "drop"
  )

# Mean group size per site
site_group_size <- event_counts %>%
  group_by(locationName, scientificName) %>%
  summarise(
    mean_group_size = mean(group_size, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(livestock_lookup, by = "locationName")

# Wilcoxon rank-sum test
wilcox.test(
  mean_group_size ~ livestock_present,
  data = site_group_size
)

# One-way ANOVA
group_aov <- aov(
  mean_group_size ~ livestock_present,
  data = site_group_size
)

summary(group_aov)

## -----------------------------------------------------------------------------
## Figures

# Species-level RAI
ggplot(
  species_rai,
  aes(
    x = scientificName,
    y = RAI_100,
    fill = livestock_present
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = "Species",
    y = "RAI (detections / 100 trap days)",
    fill = "Livestock present"
  ) +
  theme_bw()

# Site-level RAI
ggplot(
  site_rai,
  aes(
    x = livestock_present,
    y = RAI_100
  )
) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    x = "Livestock present",
    y = "RAI (detections / 100 trap days)"
  ) +
  theme_bw()

# Mean group size
group_size_summary <- site_group_size %>%
  group_by(scientificName, livestock_present) %>%
  summarise(
    mean_group_size = mean(mean_group_size, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  group_size_summary,
  aes(
    x = scientificName,
    y = mean_group_size,
    fill = livestock_present
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Mean group size",
    fill = "Livestock present"
  ) +
  theme_bw()

# Species-level mean RAI with 95% confidence intervals
species_summary <- wildlife_rai %>%
  group_by(scientificName, livestock_present) %>%
  summarise(
    mean_RAI = mean(RAI_100, na.rm = TRUE),
    se = sd(RAI_100, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(
  species_summary,
  aes(
    x = scientificName,
    y = mean_RAI,
    colour = livestock_present
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_RAI - 1.96 * se,
      ymax = mean_RAI + 1.96 * se
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    x = "Species",
    y = "Mean RAI (±95% CI)",
    colour = "Livestock present"
  ) +
  theme_bw()

## -----------------------------------------------------------------------------
## Grouping by dietary niche 

# created new dataset with species traits, called wildlife_traits
# for now, the data is also stored in wildlife 

# herbivore 
herbivore_rai <- wildlife_rai %>%
  filter(diet == "herbivore") %>%
  mutate(
    livestock_present = factor(
      ifelse(livestock_present, "Present", "Absent"),
      levels = c("Absent", "Present")
    )
  )

ggplot(
  herbivore_rai,
  aes(
    x = scientificName,
    y = RAI_100,
    fill = livestock_present
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Present" = "#CDEAC0",
      "Absent"  = "#7FB77E"
    ),
    breaks = c("Present", "Absent")
  ) +
  labs(
    x = "Species",
    y = "RAI (detections / 100 trap days)",
    fill = "Livestock",
    title = "Herbivores"
  ) +
  theme_bw()


# carnivore 
carnivore_rai <- wildlife_rai %>%
  filter(diet == "carnivore ") %>%
  mutate(
    livestock_present = factor(
      ifelse(livestock_present, "Present", "Absent"),
      levels = c("Absent", "Present")
    )
  )

ggplot(
  carnivore_rai,
  aes(
    x = scientificName,
    y = RAI_100,
    fill = livestock_present
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Present" = "#FEE08B",
      "Absent"  = "#D7191C"
    ),
    breaks = c("Present", "Absent")
  ) +
  labs(
    x = "Species",
    y = "RAI (detections / 100 trap days)",
    fill = "Livestock",
    title = "Carnivores"
  ) +
  theme_bw()

# omnivore
omnivore_rai <- wildlife_rai %>%
  filter(diet == "omnivore ") %>%
  mutate(
    livestock_present = factor(
      ifelse(livestock_present, "Present", "Absent"),
      levels = c("Absent", "Present")
    )
  )

ggplot(
  omnivore_rai,
  aes(
    x = scientificName,
    y = RAI_100,
    fill = livestock_present
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Present" = "#CC79A7",
      "Absent"  = "#56B4E9"
    ),
    breaks = c("Present", "Absent")
  ) +
  labs(
    x = "Species",
    y = "RAI (detections / 100 trap days)",
    fill = "Livestock",
    title = "Omnivores"
  ) +
  theme_bw()

## model making 
str(wildlife_rai$livestock_present)
class(wildlife_rai$livestock_present)

wildlife_rai$livestock_present <- factor(
  wildlife_rai$livestock_present,
  levels = c(TRUE, FALSE),
  labels = c("Present", "Absent")
)

# fit one-way anova 
anova_model <- aov(RAI_100 ~ livestock_present, data = wildlife_rai)
summary(anova_model)

# no post-hoc Tukey needed as there are only two groups 

# check normality of residuals 
shapiro.test(residuals(anova_model))

# homogeneity of variance 
leveneTest(RAI_100 ~ livestock_present, data = wildlife_rai)

par(mfrow = c(2,2))
plot(anova_model)

## does feature type affect the RAI of each species 
species_results <- wildlife_rai %>%
  group_by(scientificName) %>%
  group_modify(~{
    
    # Skip species with only one livestock category
    if (length(unique(.x$livestock_present)) < 2) {
      return(tibble(p.value = NA))
    }
    
    model <- aov(RAI_100 ~ livestock_present, data = .x)
    
    tibble(
      p.value = summary(model)[[1]][["Pr(>F)"]][1]
    )
  })

species_results

# check distribution of counts 
hist(wildlife_rai$independent_detections)

mean(wildlife_rai$independent_detections)
var(wildlife_rai$independent_detections)

# variance is much larger than mean, data are overdispersed -> negative binomial GLM for overdispersion 

poisson_model <- glm(
  independent_detections ~ livestock_present +
    offset(log(trap_days)),
  family = poisson,
  data = wildlife_rai
)

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) /
  poisson_model$df.residual

dispersion

# dispersion much greater than 2, supports using a negative binomial model 

nb_model <- glm.nb(
  independent_detections ~ livestock_present +
    offset(log(trap_days)),
  data = wildlife_rai
)

summary(nb_model)

# negative binomial linear mixed model with species as random effect 
model <- glmer.nb(
  independent_detections ~ livestock_present +
    offset(log(trap_days)) +
    (1 | scientificName),
  data = wildlife_rai
)
summary(model)

# add location as random effect 
model_2 <- glmer.nb(
  independent_detections ~ livestock_present +
    offset(log(trap_days)) +
    (1 | scientificName) +
    (1 | locationName),
  data = wildlife_rai
)
summary(model_2)

tidy(model_2,
     effects = "fixed",
     conf.int = TRUE,
     exponentiate = TRUE)

## get predicted detections 
# fit model (site level)
rai_nb <- glm.nb(
  detections ~ livestock_present + offset(log(trap_days)),
  data = site_rai
)

levels(factor(wildlife_rai$livestock_present))
table(wildlife_rai$livestock_present)

# get predicted means + CI
pred <- data.frame(
  livestock_present = c(FALSE, TRUE),
  trap_days = mean(site_rai$trap_days)
)

pred$fit <- predict(rai_nb, newdata = pred, type = "response")

pred_link <- predict(rai_nb, newdata = pred, type = "link", se.fit = TRUE)

pred$lower <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
pred$upper <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

ggplot(pred, aes(x = livestock_present, y = fit)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(
    x = "Livestock present",
    y = "Predicted detections (per trap effort)"
  ) +
  theme_bw()

### ------------------------------------------------------------------------ ### 
### Factor Analysis of Mixed Data (FAMD)
### -------------------------------------------------------------------------### 

install.packages("FactoMineR")
install.packages("factoextra")

library(FactoMineR)
library(factoextra)

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

### -------------------------------------------------------------------------###
wildlife_independent %>%
  group_by(scientificName) %>%
  summarise(
    n_diet = n_distinct(diet),
    n_body_size = n_distinct(body_size),
    .groups = "drop"
  ) %>%
  filter(n_diet > 1 | n_body_size > 1)

# Create species-level mean RAI
species_famd_data <- trait_species_RAI %>%
  group_by(
    scientificName,
    trait_group,
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
  left_join(
    wildlife_independent %>%
      dplyr::select(
        scientificName,
        diet,
        body_size
      ) %>%
      distinct() %>%
      mutate(
        diet = trimws(diet),
        body_size = trimws(body_size)
      ),
    by = "scientificName"
  )

nrow(species_famd_data)

species_famd_data

species_famd_data %>%
  dplyr::select(
    scientificName,
    trait_group,
    diet,
    body_size,
    `High livestock activity`,
    `Low livestock activity`
  )

species_famd_data <- species_famd_data %>%
  mutate(
    diet = trimws(diet),
    body_size = trimws(body_size),
    trait_group = trimws(trait_group)
  )

famd_data <- species_famd_data %>%
  dplyr::select(
    diet,
    body_size,
    `High livestock activity`,
    `Low livestock activity`
  ) %>%
  mutate(
    diet = factor(diet),
    body_size = factor(body_size)
  )

famd <- FactoMineR::FAMD(
  famd_data,
  graph = FALSE
)

famd$eig

famd_species <- as.data.frame(famd$ind$coord) %>%
  mutate(
    scientificName = species_famd_data$scientificName,
    trait_group = species_famd_data$trait_group
  )

nrow(famd_species)

famd$ind$coord
dim(famd$ind$coord)
head(famd_species)
str(famd_species)
summary(famd_species$Dim.1)
summary(famd_species$Dim.2)

ggplot(
  famd_species,
  aes(
    x = Dim.1,
    y = Dim.2,
    colour = trait_group
  )
) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = scientificName),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    x = paste0(
      "Dimension 1 (",
      round(famd$eig[1, 2], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 2 (",
      round(famd$eig[2, 2], 1),
      "%)"
    ),
    colour = "Functional group"
  ) +
  theme_classic()

fviz_famd_var(
  famd,
  repel = TRUE
)

summary(famd)
famd$quanti.var
famd$quali.var

famd$ind$coord
famd$ind$contrib


### -------------------------------------------------------------------------###
species_famd_data <- trait_species_RAI %>%
  group_by(
    scientificName,
    trait_group,
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
  left_join(
    wildlife_independent %>%
      dplyr::select(
        scientificName,
        diet,
        body_size,
        feeding_guild
      ) %>%
      distinct() %>%
      mutate(
        diet = trimws(diet),
        body_size = trimws(body_size),
        feeding_guild = trimws(feeding_guild)
      ),
    by = "scientificName"
  ) %>%
  mutate(
    response_ratio = log(
      (`High livestock activity` + 0.01) /
        (`Low livestock activity` + 0.01)
    )
  )

species_famd_data %>%
  dplyr::select(
    scientificName,
    diet,
    body_size,
    feeding_guild,
    `High livestock activity`,
    `Low livestock activity`,
    response_ratio
  )

species_famd_data %>%
  summarise(
    zero_high = sum(`High livestock activity` == 0, na.rm = TRUE),
    zero_low = sum(`Low livestock activity` == 0, na.rm = TRUE),
    zero_both = sum(
      `High livestock activity` == 0 &
        `Low livestock activity` == 0,
      na.rm = TRUE
    )
  )

species_famd_data %>%
  mutate(
    RR_001 = log(
      (`High livestock activity` + 0.001) /
        (`Low livestock activity` + 0.001)
    ),
    RR_01 = log(
      (`High livestock activity` + 0.01) /
        (`Low livestock activity` + 0.01)
    ),
    RR_1 = log(
      (`High livestock activity` + 1) /
        (`Low livestock activity` + 1)
    )
  ) %>%
  dplyr::select(
    scientificName,
    `High livestock activity`,
    `Low livestock activity`,
    RR_001,
    RR_01,
    RR_1
  ) %>%
  filter(
    `High livestock activity` == 0 |
      `Low livestock activity` == 0
  )

species_famd_data <- species_famd_data %>%
  mutate(
    response_ratio = log(
      (`High livestock activity` + 0.01) /
        (`Low livestock activity` + 0.01)
    )
  )

famd_data <- species_famd_data %>%
  dplyr::select(
    diet,
    body_size,
    response_ratio
  ) %>%
  mutate(
    diet = factor(diet),
    body_size = factor(body_size)
  )

famd <- FactoMineR::FAMD(
  famd_data,
  graph = FALSE
)

famd$eig

famd$var$coord
famd$var$contrib

famd_species <- as.data.frame(famd$ind$coord) %>%
  mutate(
    scientificName = species_famd_data$scientificName,
    diet = species_famd_data$diet,
    body_size = species_famd_data$body_size,
    feeding_guild = species_famd_data$feeding_guild
  )

head(famd_species)
names(famd_species)

ggplot(
  famd_species,
  aes(
    x = Dim.1,
    y = Dim.3,
    colour = diet
  )
) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = scientificName),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    x = paste0(
      "Dimension 1 (",
      round(famd$eig[1, 2], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 3 (",
      round(famd$eig[3, 2], 1),
      "%)"
    ),
    colour = "Diet"
  ) +
  theme_classic()

ggplot(
  famd_species,
  aes(
    x = Dim.1,
    y = Dim.3,
    colour = body_size
  )
) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = scientificName),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    x = paste0(
      "Dimension 1 (",
      round(famd$eig[1, 2], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 3 (",
      round(famd$eig[3, 2], 1),
      "%)"
    ),
    colour = "Body size"
  ) +
  theme_classic()

famd_species <- famd_species %>%
  mutate(
    trait_group = paste(diet, body_size, sep = " – ")
  )

ggplot(
  famd_species,
  aes(
    x = Dim.1,
    y = Dim.3,
    colour = trait_group
  )
) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = scientificName),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    x = paste0(
      "Dimension 1 (",
      round(famd$eig[1, 2], 1),
      "%)"
    ),
    y = paste0(
      "Dimension 3 (",
      round(famd$eig[3, 2], 1),
      "%)"
    ),
    colour = "Diet – body size"
  ) +
  theme_classic()

### -------------------------------------------------------------------------###
species_famd_data <- species_famd_data %>%
  mutate(
    functional_group = case_when(
      diet == "herbivore" ~ paste(feeding_guild, body_size, sep = " – "),
      TRUE ~ paste(diet, body_size, sep = " – ")
    )
  )

ggplot(
  species_famd_data,
  aes(
    x = functional_group,
    y = response_ratio
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 3
  ) +
  ggrepel::geom_text_repel(
    aes(label = scientificName),
    size = 3,
    max.overlaps = Inf
  ) +
  coord_flip() +
  labs(
    x = "Functional group",
    y = "Log response ratio\n(high vs. low livestock activity)"
  ) +
  theme_classic()


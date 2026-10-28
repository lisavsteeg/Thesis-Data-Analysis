## Comparing richness in areas with and without livestock ## 
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# get deployments to later add locations to the observations dataset
deployments_clean <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/deployments_clean.csv")

wildlife <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/wildlife.csv")

## -------------------------------------------------------------------------- ##
head(wildlife)
length(unique(wildlife$scientificName)) #46 species detected
unique(wildlife$scientificName)
nrow(wildlife) #31933 observations 

summary <- wildlife %>%
  count(scientificName, livestock_group) %>%
  pivot_wider(
    names_from = livestock_group,
    values_from = n,
    values_fill = 0
  )

## -------------------------------------------------------------------------- ##
## Compare species richness with high and low livestock activity  ##
richness_df <- wildlife %>%
  group_by(locationName, livestock_group) %>%
  summarise(
    richness = n_distinct(scientificName),
    .groups = "drop"
  ) %>%
  mutate(
    Livestock = factor(
      livestock_group,
      levels = c("Low livestock activity", "High livestock activity")
    )
  )

table(richness_df$Livestock)

## statistics ## 
# check normality 
by(richness_df$richness, richness_df$Livestock, shapiro.test)
# p > 0.05 for both groups, appropriate for t.test, no evidence of non-normality

# visual check
ggplot(richness_df, aes(x = richness)) +
  geom_histogram(bins = 10) +
  facet_wrap(~Livestock)

ggplot(richness_df, aes(sample = richness)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Livestock)
# points relatively close to line, t.test still appropriate

# check variance 
var.test(richness ~ Livestock, data = richness_df)
# p = 0.1257, no evidence that variances differ, two-sample t-test appropriate 

t.test(
  richness ~ Livestock,
  data = richness_df,
  var.equal = TRUE
)

richness_df %>%
  group_by(Livestock) %>%
  summarise(
    n = n(),
    mean_richness = mean(richness),
    sd_richness = sd(richness)
  )

# mean low activity 28.9 SD 4.38 
# mean high activity 26.9, SD 2.78 

# plot richness 
ggplot(richness_df, aes(x = Livestock, y = richness, fill = Livestock)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.7) +
  stat_compare_means(
    method = "t.test",
    aes(group = Livestock),
    label.x = 1.5
  ) +
  scale_fill_manual(values = c(
    "Low livestock activity" = "#B39DDB",
    "High livestock activity" = "#80CBC4"
  )) +
  labs(
    x = "",
    y = "Species richness",
    fill = "Site type"
  ) +
  theme_classic(base_size = 14)





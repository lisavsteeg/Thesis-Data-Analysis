### clean script camera trap data ###

## working directory
setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

## library ## 
library(devtools)
library(frictionless)
library(camtraptor)
library(ctdp)
library(sf)
library(maptiles)
library(vegan)
library(terra)
library(ggspatial)
library(ggplot2)
library(ggrepel)
library(pheatmap)
library(tidyr)
library(iNEXT)
library(dplyr)
library(goeveg)

## ------------------------ ##
## deployments ## 
deployments <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/deployments.csv", stringsAsFactors=FALSE)
View(deployments)
head(deployments)

# Create a boolean vector, which is TRUE if a value of deployments$deploymentID has already appeared once.
dup_locations_bool <- duplicated(deployments$locationName)
dup_locations_bool 
length(unique(deployments$locationName)) #27 names, which is in accordance with the number of cameras in the field

# Subset the deployment using the dup_locations_bool boolean.

# subset columns by column name,
# keep only the placename, lat and long variables.
locations <- deployments[
  !dup_locations_bool,c("locationName","longitude", "latitude")] # the boolean is a vector of trues and falses, and it only keeps the 'TRUE' rows when you put it into the []
nrow(locations)

#Convert our dataframe to a "simple features" object.
#Specify the object to be converted, the columns and the CRS
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326,
                         remove=FALSE)# keep latitude and longitude columns

plot(locations_sf["locationName"])

# plot camera trap locations including names with ggplot #
locations_sf_coords <- cbind(locations_sf,
                             st_coordinates(locations_sf)
)

ggplot(locations_sf_coords) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = locationName)) +
  theme_minimal()

# plot camera trap locations on basemap # 
# Get basemap
locations_basemap <- get_tiles(locations_sf,
                               provider = "OpenTopoMap",
                               zoom = 13
)

# Convert sf to coordinates
locations_sf_coords <- cbind(locations_sf,
                             st_coordinates(locations_sf)
)

# Create wider longitude extent 
bb <- st_bbox(locations_sf)

x_expand <- 0.03
y_expand <- 0.01

bb_expanded <- bb

bb_expanded["xmin"] <- bb["xmin"] - x_expand
bb_expanded["xmax"] <- bb["xmax"] + x_expand
bb_expanded["ymin"] <- bb["ymin"] - y_expand
bb_expanded["ymax"] <- bb["ymax"] + y_expand

# Convert bbox to sf object
bb_sf <- st_as_sfc(bb_expanded)

# Get larger basemap
locations_basemap <- get_tiles(bb_sf,
                               provider = "OpenTopoMap",
                               zoom = 13
)

# Plot Camera Trap Locations
ggplot() +
  
  # Basemap 
  layer_spatial(locations_basemap) +
  
  # Points
  geom_point(
    data = locations_sf_coords,
    aes(x = X, y = Y),
    color = "blue",
    size = 2
  ) +
  
  # Labels
  geom_text_repel(
    data = locations_sf_coords,
    aes(x = X, y = Y, label = locationName),
    size = 3
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl", 
    width_hint = 0.25
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Coordinate limits
  coord_sf(
      xlim = c(bb_expanded["xmin"], bb_expanded["xmax"]),
      ylim = c(bb_expanded["ymin"], bb_expanded["ymax"]),
      expand = FALSE
    ) +
  
  # Remove white margins around map
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0,0,0,0)
  ) +
  
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

## ------------------------ ##
## observations ## 
observations <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/observations.csv", stringsAsFactors=FALSE)

View(observations)
head(observations)
tail(observations)

nrow(observations)

# link the observation to the deployments, via the deploymentID
obs_deploy_index <- match(observations$deploymentID,deployments$deploymentID)

# get each observation's location and add to dataframe
observations$locationName <- deployments$locationName[obs_deploy_index]

head(observations) #extra column with locationName 

## filter data ##
# get only animal observations 
animal_observations <- observations %>%
  filter(observationType == "animal")

# get only mammal observations
mammal_observations <- animal_observations %>%
  filter(
    !scientificName %in% c("Aves",
                           "Rodentia",
                           "Canis lupus familiaris",
                           "Numida meleagris",
                           "Pternistis leucoscepus",
                           "Ardeotis kori",
                           "Sagittarius serpentarius",
                           "Chiroptera",
                           "Xerus rutilus" 
    )
  )

sort(unique(mammal_observations$scientificName)) #check all species to see if only mammals are in the data 

head(mammal_observations) 
length(unique(observations$locationName)) #27 names 
length(unique(mammal_observations$locationName)) #19? Some cameras don't have mammal observations? 

# divide mammals in livestock and wildlife 
# Livestock = Bos taurus, Equus asinus, Capra hircus, Ovis aries 
livestock <- mammal_observations[
  mammal_observations$scientificName %in% c("Bos taurus",
                                            "Equus asinus",
                                            "Capra hircus",
                                            "Ovis aries"
  ),
]
view(livestock)

## add column for livestock presence per camera location ##
livestock_species <- c("Bos taurus",
                       "Equus asinus",
                       "Ovis aries",
                       "Capra hircus"
)

livestock_by_location <- aggregate(scientificName ~ locationName,
                                   data = mammal_observations,
                                   FUN = function(x) any(x %in% livestock_species)
)

names(livestock_by_location)[2] <- "livestock_presentLocation" #TRUE if livestock was ever recorded on that camera (CAR-09, CAR-14, CAR-19, CAR-21, CAR-22, CAR-23, CAR-25, CAR-30, CAR-31, CAR-33, CAR-40, CAR-41, CAR-44)

mammal_observations <- merge(mammal_observations,
                       livestock_by_location,
                       by = "locationName",
                       all.x = TRUE
)
head(mammal_observations)

## extract wildlife species from the dataset
# wildlife = all mammal species >500 g that are not livestock 
wildlife <- mammal_observations[
  !(mammal_observations$scientificName %in% c("Bos taurus",
                                              "Equus asinus",
                                              "Capra hircus",
                                              "Ovis aries"
  )),
]
head(wildlife)
length(unique(wildlife$scientificName)) #34 species detected

## ------------------------ ##
# Number of individuals per deployment 
det_per_deployment <- aggregate(count ~ deploymentID,
                                #number of objects (in a picture) BY deployment
                                FUN = sum,
                                data = wildlife)
head(det_per_deployment)

# adjust code to look at these results by location
det_per_location <- aggregate(count ~ locationName,
                                #number of objects (in a picture) BY location
                                FUN = sum,
                                data = wildlife)
head(det_per_location)

# Some cameras don't have mammal observations?

# get counts for livestock per camera trap location
det_per_locationLivestock <- aggregate(count ~ locationName,
                                    #number of objects (in a picture) BY placename
                                    FUN = sum,
                                    data = livestock)
view(det_per_locationLivestock)

# get counts for wildlife per camera trap location 
det_per_locationWildlife <- aggregate(count ~ locationName,
                                      #number of objects (in a picture) BY placename
                                       FUN = sum,
                                       data = wildlife)
view(det_per_locationWildlife)

# At which location was ‘Cattle’ detected most often?
# Cattle = Bos taurus
cattle <- mammal_observations[mammal_observations$scientificName == "Bos taurus", ]
view(cattle)

det_per_locationCattle <- aggregate(count ~ locationName,
                                    #number of objects (in a picture) BY placename
                                    FUN = sum,
                                    data = cattle)
view(det_per_locationCattle)

## ------------------------ ##
### Calculating species diversity using Simpson index ###
for (locations in locations_sf$locationName) {
  #get observations from that deployment
  locations_obs <-
    wildlife$scientificName[wildlife$locationName == locations]
  #get the unique values of these
  uni_obs <- unique(locations_obs)
  print(uni_obs)
  #count them
  n_uni_obs <- length(uni_obs)
  #add them to the locations dataframe, at the correct location
  locations_sf$n_species[locations_sf$locationName == locations] <-
    n_uni_obs
}

setdiff(
  locations_sf$locationName,
  unique(wildlife$locationName)
)
# empty cameras "CAR-46" "CAR-01 "CAR-26" 

#check cameras to be sure there are no mammal observations 
unique(observations[
  observations$locationName == "CAR-01",
  c("scientificName", "observationType")
]
) 
# unclassified and blank 
# seems to have no mammal observations

# get number of species per camera trap 
locations_sf$n_species

summary(locations_sf$n_species)

# location with most species
max_species <- max(locations_sf$n_species)
deploy_most_species <-
  locations_sf$locationName[locations_sf$n_species == max_species]
deploy_most_species #CAR-10 has most wildlife species 

# Get species counts by location and placename
locations_species_counts <-
  aggregate(count ~ locationName + scientificName,
            FUN = sum,
            data = wildlife)

# Vegan expects data in a certain structure, with each column being a species and each row being a sampling unit. We can generate this structure using the xtabs function
location_species_table <-
  xtabs(count ~ locationName + scientificName,
        data = locations_species_counts)

# Use the vegan package diversity function to calculate the simpson index
per_site_diversity <-
  diversity(location_species_table, index = "simpson")

#This produces a named vector of diversity. We can use the names to place these results in the correct location
placename_matches <- 
  match(locations_sf$locationName, names(per_site_diversity))

# The match function returns a vector giving the indexes where a value first appears in another vector. In this case, we get the indexes of per_site_diversity where the name of per_site_diversity matches a given placename from locations_sf
# We can use these indexes to get the per_site_diversity in the correct order and place it as a column in locations_sf
locations_sf$diversity <- per_site_diversity[placename_matches]
view(locations_sf)

# how the number of species and the Simpson diversity index compare to each other
plot(diversity ~ n_species,
     data = locations_sf,
     xlab = "Number of species",
     ylab = "Simpson diversity index",
     pch = 16
)

text(x = locations_sf$n_species,
     y = locations_sf$diversity,
     labels = locations_sf$locationName,
     pos = 2,      # labels to the right of points
     cex = 0.7     # text size
)

# high simpson value -> diverse community, no dominance by 1 specific species 
# low simpson value -> few species or strong dominance by 1 species 
# top right corner -> high species richness and high evenness 

## ------------------------ ##
## looking at community composition using Sorensen index of similarity 
# create presence/absence matrix
comm_matrix <- xtabs(~ locationName + scientificName,
                     data = wildlife
)

# convert counts >1 to presence/absence 
comm_matrix <- (comm_matrix > 0) * 1

# calculate sorensen dissimilarity
sorensen_dist <- vegdist(comm_matrix,
                         method = "bray",
                         binary = TRUE
)
# 0 = identical, 1 = completely different

# convert to similarity for easier interpretation
sorensen_sim <- 1 - as.matrix(sorensen_dist)
# 0 = completely different, 1 = identical 

# view camera comparisons 
round(sorensen_sim, 2)

# visualize similarities 
pheatmap(sorensen_sim,
         display_numbers = TRUE
)

# cluster similar cameras 
hc <- hclust(sorensen_dist)

plot(hc,
     main = "Camera Dissimilarity (Sørensen)"
)

## ------------------------ ##
# estimate how species richness accumulates with increasing sampling effort, and standardize comparisons so that differences aren't simply due to one camera/group having more detections than another

# create species by camera matrix 
comm_matrix <- xtabs(count ~ locationName + scientificName, 
                     data = wildlife 
) 

camera_list <- apply(comm_matrix, 
                     1, 
                     function(x) x[x > 0] 
) 

barplot(locations_sf$diversity, 
        names.arg = locations_sf$locationName, 
        las = 2, 
        ylab = "Simpson diversity" 
) 

locations_sf[ 
  order(locations_sf$diversity, decreasing = TRUE), 
  c("locationName", "diversity", "n_species") 
] 

## add column for livestock presence per camera location in locations_sf ## 
locations_sf <- merge(locations_sf, 
                      livestock_by_location, 
                      by = "locationName", 
                      all.x = TRUE 
) 

# create abundance vectors 
livestock_counts <- wildlife %>% 
  filter(livestock_present) %>% 
  group_by(scientificName) %>% 
  summarise(n = sum(count, na.rm = TRUE)) 

no_livestock_counts <- wildlife %>% 
  filter(!livestock_present) %>% 
  group_by(scientificName) %>% 
  summarise(n = sum(count, na.rm = TRUE)) 

# check if groups aren't empty
nrow(livestock_counts)
nrow(no_livestock_counts)

#iNEXT input object 
iNEXT_input <- list(Livestock = livestock_counts$n, 
                    No_Livestock = no_livestock_counts$n 
) 
names(iNEXT_input$Livestock) <- livestock_counts$scientificName 
names(iNEXT_input$No_Livestock) <- no_livestock_counts$scientificName

out_richness <- iNEXT(iNEXT_input,
                      q = 0,
                      datatype = "abundance"
)

# plot accumulation curves 
ggiNEXT(out_richness)

table(locations_sf$livestock_present)

# rarefaction is a technique used to estimate and compare species diversity across samples of varying sizes
# Extrapolation is the process of using known data, facts, or trends to make an educated guess or prediction about an unknown situation that lies outside of your current observations

# compare different diversity measures 
out_q0 <- iNEXT(iNEXT_input, q = 0, datatype = "abundance") #richness
out_q1 <- iNEXT(iNEXT_input, q = 1, datatype = "abundance") #shannon
out_q2 <- iNEXT(iNEXT_input, q = 2, datatype = "abundance") #simpson

# coverage based comparison 
ggiNEXT(out_q0, type = 3)
ggiNEXT(out_q1, type = 3)
ggiNEXT(out_q2, type = 3)

# sample coverage = proportion of the community that has effectively been sampled
# Higher diversity in livestock areas across q = 0, 1, and 2, indicating differences in both richness and community structure

## ------------------------ ##
## Non-Metric Multidimensional Scaling (NMDS)
# NMDS takes complex, multivariate data and represents the relationships in a way that is easier for interpretation

# create dataframe with relevant variables
community_df <- wildlife %>%
  group_by(
    locationName,
    scientificName,
    livestock_present
  ) %>%
  summarise(
    Abundance = sum(count),
    .groups = "drop"
  ) %>%
  rename(
    Location = locationName,
    Species = scientificName,
    Livestock = livestock_present
  )

head(community_df)

# visualize community data
ggplot(community_df, aes(x = Livestock, y = Abundance, fill = Species)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic()

# create factors of Locations, Species, and Habitat columns (Livestock)
com_matrix <- community_df %>%
  
  mutate_at(vars(Location), as.factor) %>%
  
  pivot_wider(
    names_from = Species,
    values_from = Abundance,
    values_fill = 0
  ) %>%
  
  column_to_rownames(var = "Location")

head(com_matrix)

# run NMDS
nmds <- vegan::metaMDS(
  com_matrix,
  distance = "bray",
  k = 2
)

print(nmds) 
# stress value is 0.13, ordination is a fair fit, which is accepted but not great
# ordination = rearranging data in a low-dimensional space, arranging samples across axes based on similarity

# stress plot 
stressplot(nmds)
# x axis: dissimilarity or distance in full dataset (all dimensions or attributes considered), # y axis: distance in ordination space (reduced, 2-dimensional space since k = 2)

# scree plot (number of dimensions vs stress value)
rowSums(com_matrix[, 2:9])
sum(is.na(com_matrix[, 2:9]))

com_matrix_clean <- com_matrix[rowSums(com_matrix[, 2:9], na.rm = TRUE) > 0, ]

dimcheck_out <-
  dimcheckMDS(
    com_matrix_clean[, 2:9],
    distance = "bray",
    k = 6
  )

print(dimcheck_out)
# increasing the number of dimensions does not return a large change on the stress value 

# create ordination 
plot(nmds)
# black circles represent locations, red + represent species 

# get ordination by livestock presence/absence  
livestock_presence <- community_df %>% 
  distinct(Location, Livestock)

class(nmds)
find("scores")
getAnywhere(scores)

# Extract NMDS scores for locations 
species_scores <- vegan::scores(nmds, display = "species")
head(species_scores)
rownames(species_scores)

nmds_LocationScores <-
  as.data.frame(vegan::scores(nmds, display = "sites")) %>%
  tibble::rownames_to_column("Location") %>%
  dplyr::left_join(livestock_presence, by = "Location")
nmds_SpeciesScores <- data.frame(
  NMDS1 = species_scores[,1],
  NMDS2 = species_scores[,2],
  species = rownames(species_scores)
)
    
# create a column of species, from the rownames of species.scores
  nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

# plot NMDS scores
ggplot() + 
  
  # add site scores
  geom_point(data = nmds_LocationScores, 
             aes(x=NMDS1, y=NMDS2, colour = Livestock), 
             size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  theme_classic()

# get centroid (mean value for grouping variable)
Livestock_Centroid <- 
  nmds_LocationScores %>% 
  group_by(Livestock) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
livestock.hull <- 
  nmds_LocationScores %>% 
  group_by(Livestock) %>%
  slice(chull(NMDS1, NMDS2))

# get stress value 
nmds_stress <- nmds$stress

# use ggplot to plot final ordination
ggplot() + 
  
  # add site scores
  geom_point(data = nmds_LocationScores, 
             aes(x=NMDS1, y=NMDS2, colour = Livestock), size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  # add centroid 
  geom_point(data = Livestock_Centroid, 
             aes(x = axis1, y = axis2, color = Livestock), 
             size = 5, shape = 17) +
  
  # add convex hull
  geom_polygon(data = livestock.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Livestock, group = Livestock), 
               alpha = 0.30) +
  
  # add stress value
  annotate("text", x = 1.3, y = 1.4, 
           label = paste("2d stress =", round(nmds_stress, 3))) +
  # edit theme
  labs(x = "NMDS1", y = "NMDS2") + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, linewidth = .5),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(.25, "cm")) 

# community composition has some overlap between sites with and without livestock 
# greater dissimilarity in the underlying communities after they’ve been squeezed into a low-dimensional space in areas without livestock (wide spread of points)


## ------------------------ ##
## divide in trophic levels 
# levels: carnivore, herbivore, omnivore
# add trophic levels to the wildlife dataset 
carnivore_species <- c("Mungos mungo",
                       "Panthera pardus",
                       "Galerella sanguinea",
                       "Proteles cristata",
                       "Crocuta crocuta", 
                       "Panthera leo", 
                       "Caracal caracal",
                       "Hyaena hyaena",
                       "Orycteropus afer"
)

herbivore_species <- c("Redunca redunca",
                        "Tragelaphus imberbis",
                        "Nanger granti",
                        "Aepyceros melampus",
                        "Giraffa tippelskirchi", 
                        "Taurotragus oryx", 
                        "Phacochoerus africanus",
                        "Kobus ellipsiprymnus",
                        "Madoqua kirkii",
                        "Syncerus caffer",
                        "Connochaetes taurinus", 
                        "Tragelaphus scriptus",
                        "Potamochoerus larvatus", 
                        "Equus quagga",
                        "Loxodonta africana", 
                        "Hystrix"
)

omnivore_species <- c("Genetta genetta",
                      "Ichneumia albicauda",
                      "Mellivora capensis",
                      "Otocyon megalotis",
                      "Herpestes ichneumon", 
                      "Civettictis civetta", 
                      "Canis mesomelas",
                      "Chlorocebus pygerythrus",
                      "Papio anubis")

wildlife <- wildlife %>%
  mutate(
    trophic_level = case_when(
      scientificName %in% carnivore_species ~ "Carnivore",
      scientificName %in% herbivore_species ~ "Herbivore",
      scientificName %in% omnivore_species ~ "Omnivore",
      TRUE ~ NA_character_
    )
  )

head(wildlife)
table(wildlife$trophic_level, useNA = "ifany")
# 74 carnivore observations, 1436 herbivore observations, 209 omnivore observations

wildlife %>%
  filter(is.na(trophic_level)) %>%
  distinct(scientificName)
# all species have been assigned to one of the trophic leves 

## using iNEXT to compare the different groups 
# create abundance vectors 
trophic_abundance <- wildlife %>%
  filter(!is.na(trophic_level)) %>%
  group_by(
    trophic_level,
    livestock_presentLocation,
    scientificName
  ) %>%
  summarise(
    abundance = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

# iNEXT input list
trophic_abundance <- trophic_abundance %>%
  mutate(
    assemblage = paste(
      trophic_level,
      ifelse(livestock_presentLocation,
             "Livestock",
             "NoLivestock"),
      sep = "_"
    )
  )

iNEXT_input <- split(
  trophic_abundance,
  trophic_abundance$assemblage
)

iNEXT_input <- lapply(iNEXT_input, function(x){
  
  abund <- x$abundance
  names(abund) <- x$scientificName
  
  abund
})

str(iNEXT_input)

# run iNEXT
out <- iNEXT(
  iNEXT_input,
  q = 0,
  datatype = "abundance"
)

# plot richness accumulation curves 
ggiNEXT(
  out,
  type = 1
)

# compare richness at equal sample coverage
ggiNEXT(
  out,
  type = 3
)

# if Carnivore_Livestock has higher richness than Carnivore_NoLivestock at the same coverage, that difference is less likely to be due simply to unequal sampling effort

## incidence-frequency iNEXT to see if increasing sampling sites has an effect
# presence per camera



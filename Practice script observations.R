### Test data phase 2 ### 

## install packages ## 
install.packages("devtools")
devtools::install_github("frictionlessdata/frictionless-r")
devtools::install_github("inbo/camtraptor")
install.packages("vctrs")

devtools::install_gitlab(repo = "camtrap/ctdp", host = "git.wur.nl")

library(devtools)
library(frictionless)
library(camtraptor)
library(ctdp)

setwd("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis")

## Loading data ## 

observations <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/observations.csv", stringsAsFactors=TRUE)

View(observations)
head(observations)
tail(observations)

## Clean data ## 

#List of packages
required_packages <- c("sf", "maptiles", "vegan")
#This code will install all required packages
new_packages <- required_packages[!(required_packages
                                    %in% installed.packages()[, "Package"])]
#if some are not installed, install them
if (length(new_packages)) {
  install.packages(new_packages)
}

library(sf)
library(maptiles)
library(vegan)

#load deployments.csv from the cameratrap_data folder
observations <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/observations.csv", stringsAsFactors=TRUE) 

head(observations)

unique(observations$observationType)
table(observations$observationType)
table(observations$observationType, useNA = "ifany")

animal_observations <- observations %>%
  filter(observationType == "animal")

unclassified <- observations %>% 
  filter(observationType == "unclassified")

mammal_observations <- animal_observations %>%
  filter(
    !scientificName %in% c(
      "Aves",
      "Rodentia",
      "Canis lupus familiaris",
      "Numida meleagris",
      "Pternistis leucoscepus",
      "Ardeotis kori",
      "Sagittarius serpentarius",
      "Chiroptera"
    )
  )

unique(mammal_observations$scientificName)

deployments <- read.csv("~/Library/CloudStorage/OneDrive-WageningenUniversity&Research/Thesis/Data analysis/south-rift-small-carnivores-phase-3-20260413150233/deployments.csv", stringsAsFactors=TRUE)
View(deployments)

# Create a boolean vector, which is TRUE if a value of deployments$placename
# has already appeared once.
dup_locations_bool <- duplicated(deployments$deploymentID)
dup_locations_bool #74 original names 
length(unique(deployments$deploymentID))
# Subset the deployment using the dup_locations_bool boolean.
# Using the ! inverts the boolean
# Inverted, TRUE is the first appearance of a deployments$placename
# Subsetting the rows by this results in a dataframe with one row per placename.
# We also subset columns by column name,
# We keep only the placename, lat and long variables.
locations <- deployments[
  !dup_locations_bool,c("locationName","longitude", "latitude")] # the boolean is a vector of trues and falses, and it only keeps the 'TRUE' rows when you put it into the []
nrow(locations)
#Convert our dataframe to a "simple features" object.
#Specify the object to be converted, the columns and the CRS
locations_sf <- st_as_sf(locations,
                         coords = c("longitude", "latitude"),
                         crs = 4326,
                         remove=FALSE)# keep latitude and longitude columns

#locations[rows,columns]

library(sf)

locations_sf <- st_as_sf(
  locations,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

plot(locations_sf["locationName"])

library(sf)
library(maptiles)
library(terra)

locations_basemap <- get_tiles(locations_sf, provider = "OpenStreetMap")
?get_tiles # other possible maps
plotRGB(locations_basemap)
plot(st_geometry(locations_sf), add = TRUE, col = "red", pch = 16)

# How many locations have more than one deployment? 
dup_locations_bool

length(unique(deployments$placename[dup_locations_bool]))

########### images
head(mammal_observations)

#How many observations are there?
nrow(observations)

# We need to link the observation to the deployments, via the deployment_id
# First we get the numerical index of each observations deployment
# from the deployment dataframe
# We use the match command to get indexes of deployments$deployment_id
# which match the observations$deployment_id
obs_deploy_index <- match(observations$deploymentID,deployments$deploymentID)
# Using this, we can get each observations placename.
# We can attach this to our observation dataframe
observations$locationName <- deployments$locationName[obs_deploy_index]

#create boolean of whether an observation is in our list of unwanted species.
animal_observations <- observations %>%
  filter(observationType == "animal")

unclassified <- observations %>% 
  filter(observationType == "unclassified")

mammal_observations <- animal_observations %>%
  filter(
    !scientificName %in% c(
      "Aves",
      "Rodentia",
      "Canis lupus familiaris",
      "Numida meleagris",
      "Pternistis leucoscepus",
      "Ardeotis kori",
      "Sagittarius serpentarius",
      "Chiroptera"
    )
  )

unique(mammal_observations$scientificName)

bad_obs_bool <-
  observations$full_species %in% c(" ", "Unknown Unknown", "Homo sapiens")
#subset observations by an inverted (!) version of this boolean.
observations <- observations[!bad_obs_bool, ]

det_per_deployment <- aggregate(number_of_objects ~ deployment_id,
                                #number of objects (in a picture) BY placename
                                FUN = sum,
                                #we will add these
                                data = observations)
head(det_per_deployment)

#  Is number of detections the same as number of individuals?
#' I think it is, because i thought the number of objects indicates the number of individuals in a photo

# You can adjust this code to look at these results by location
det_per_location <- aggregate(number_of_objects ~ placename,
                              #number of objects (in a picture) BY placename
                              FUN = sum,
                              #we will add these
                              data = observations)
head(det_per_location)

# At which LOCATION were ‘Oncilla’ detected most often?
# Oncilla = Leopardus tigrinus
det_per_location
unique(observations$full_species)
head(observations)

Leopardus_tig <- observations[observations$full_species == "Leopardus tigrinus", ] # does not work
det_per_locationTig <- aggregate(number_of_objects ~ placename,
                                 #number of objects (in a picture) BY placename
                                 FUN = sum,
                                 #we will add these
                                 data = Leopardus_tig)
head(det_per_locationTig)

### pt 2: Calculating species diversity
#for every locations
for (location in locations_sf$placename) {
  #get observations from that deployment
  location_obs <-
    observations$full_species[observations$placename == location]
  #get the unique values of these
  uni_obs <- unique(location_obs)
  print(uni_obs)
  #count them
  n_uni_obs <- length(uni_obs)
  #add them to the locations dataframe, at the correct location
  locations_sf$n_species[locations_sf$placename == location] <-
    n_uni_obs
}

locations_sf$n_species

#  Which location has the greatest species richness?
max_species <- max(locations_sf$n_species)
deploy_most_species <-
  locations_sf$placename[locations_sf$n_species == max_species]


### Shannon index
# first for example site
location_1 <- data.frame(
  location = 1,
  species_name = c(
    "Vulpes vulpes",
    "Meles meles",
    "Talpa europaea",
    "Mustela erminea",
    "Strix aluco",
    "Oryctolagus cuniculus"
  ),
  observation_count = c(1, 1, 1, 1, 1, 25)
)

location_2 <- data.frame(
  location = 2,
  species_name = c(
    "Vulpes vulpes",
    "Meles meles",
    "Talpa europaea",
    "Mustela erminea",
    "Strix aluco",
    "Oryctolagus cuniculus"
  ),
  observation_count = c(5, 5, 5, 5, 5, 5)
)

# Calculate the proportion of observations for each species
location_1$p_i <- location_1$observation_count/sum(location_1$observation_count)
# Calculate the log of these proportions
location_1$ln_p_i <- log(location_1$p_i)
#multiply values together
location_1$p_i_by_ln_p_i <- location_1$p_i * location_1$ln_p_i
# The Shannon's diversity
(location_1_diversity <- -sum(location_1$p_i_by_ln_p_i))
# Calculate the proportion of observations for each species
location_2$p_i <- location_2$observation_count/sum(location_2$observation_count)
# Calculate the log of these proportions
location_2$ln_p_i <- log(location_2$p_i)
# Multiply values together
location_2$p_i_by_ln_p_i <- location_2$p_i * location_2$ln_p_i
# The Shannon's diversity of the location
(location_2_diversity <- -sum(location_2$p_i_by_ln_p_i))


# now for our real sites
# Get species counts by location and placename
locations_species_counts <-
  aggregate(number_of_objects ~ placename + full_species,
            FUN = sum,
            data = observations)
# Vegan expects data in a certain structure, with each column being a species
# and each row being a sampling unit. We can generate this structure using the
# xtabs function
location_species_table <-
  xtabs(number_of_objects ~ placename + full_species,
        data = locations_species_counts)
# Use the vegan package diversity function to calculate the shannon index
per_site_diversity <-
  diversity(location_species_table, index = "shannon")
#This produces a named vector of diversity. We can use the names to place these
#results in the correct location
placename_matches <-
  match(locations_sf$placename, names(per_site_diversity))
# The match function returns a vector giving the indexes where a value first
# appears in another vector. In this case, we get the indexes of
# per_site_diversity where the name of per_site_diversity matches a given
# placename from locations_sf
# We can use these indexes to get the per_site_diversity in the correct order
# and place it as a column in locations_sf
locations_sf$diversity <- per_site_diversity[placename_matches]

#How do the number of species and the Shannon diversity index compare to each other?
plot(
  diversity ~ n_species,
  data = locations_sf,
  xlab = "Number of species",
  ylab = "Shannon diversity index",
  pch = 16
)

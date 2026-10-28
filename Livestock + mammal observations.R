## livestock + mammal observations ## 
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
                           "Chiroptera"
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

# wildlife = all mammal species >500 g that are not livestock 
wildlife <- mammal_observations[
  !(mammal_observations$scientificName %in% c("Bos taurus",
                                              "Equus asinus",
                                              "Capra hircus",
                                              "Ovis aries"
  )),
]
view(wildlife)

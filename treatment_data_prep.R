## Data cleaning and preparation for Treatment files from 2018 and 2019 SLF protocols
library(rgdal)
library(raster)
library(GISTools)
## read in raster for crs mappingdata
infected_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2018_single_count_pm_prop.tif"
infected <- raster(infected_file)
## read in total treatment data
surveys <- readOGR("H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/TreatmentPlans2019_All_States.shp")
unique_survey_classes <- unique(surveys$FIRST_Site)
surveys <-spTransform(surveys, CRS = crs(infected))
surveys$UID <- seq(1, length(surveys), 1)
## read in 2019 ailanthus treatment
tree_of_heaven <- readOGR("H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/2019_AilanthusTreesTreatmentsPoints2.shp")
# tree_of_heaven <-spTransform(tree_of_heaven, CRS = crs(infected))
toh2 <- data.frame(tree_of_heaven)
toh <- toh[toh$coords.x1 >= -9000000 | toh$coords.x1 <= -7000000, ]
toh <- SpatialPointsDataFrame(toh[,c(25,26)], toh, proj4string = crs(infected))

plot(infected)
plot(toh, add = TRUE)

## add new columns to make data more easily selected for different purposes.
# set host column based on unique values in site plan (FIRST_Site)
surveys$host <- NA
no_host <- unique_survey_classes[c(1,8)]
surveys$host[surveys$FIRST_Site %in% no_host] <- 'no'
unknowns <- unique_survey_classes[c(2,6,9)]
surveys$host[surveys$FIRST_Site %in% unknowns] <- 'unknown'
hosts <- unique_survey_classes[c(3,4,5,7,10,11,13,14,15)]
surveys$host[surveys$FIRST_Site %in% hosts] <- 'yes'
host_unique <- unique(surveys$host)

## add new columns for host counts (this value is calculated from the 2019_AilanthusTreesTreamentPoints.shp file)
surveys$count <- NA
trees <- over(tree_of_heaven, surveys)
trees$treatment_type <- tree_of_heaven$TreatmentP
tp <- over(surveys, tree_of_heaven)
counts <- data.frame(table(trees$UID))
counts <- counts[counts$Freq > 0,]
for (i in 1:nrow(counts)) {
  surveys$count[surveys$UID %in% counts$Var1[i]] <- counts$Freq[i]
  print(i)
}

# set survey column based on unique values in site plan (FIRST_Site)
surveys$survey <- NA
pendings <- unique_survey_classes[c(2,6,9,10,15)]
surveys$survey[surveys$FIRST_Site %in% pendings] <- 'pending'
completes <- unique_survey_classes[c(1,3,4,5,7,8,10,11,13,14,15)]
surveys$survey[surveys$FIRST_Site %in% completes] <- 'complete'
survey_unique <- unique(surveys$survey)

# set treatment column based on unique values in site plan (FIRST_Site) 
surveys$treatment <- NA
nones <- unique_survey_classes[c(1,8)]
surveys$treatment[surveys$FIRST_Site %in% nones] <- 'none'
pendings <- unique_survey_classes[c(4,13)]
surveys$treatment[surveys$FIRST_Site %in% pendings] <- 'pending'
unknowns <- unique_survey_classes[c(2,3,6,9,10,11,14,15)]
surveys$treatment[surveys$FIRST_Site %in% unknowns] <- 'unknown'
completes<- unique_survey_classes[c(5,7)]
surveys$treatment[surveys$FIRST_Site %in% completes] <- 'complete'
treatment_unique <- unique(surveys$treatment)

# set treatment ratio column based on unique values in site plan (FIRST_Site) (this value is calculated from the 2019_AilanthusTreesTreamentPoints.shp file) based on the ratio of herbicide/removal to pesticide trees (e.g. if 5 total trees trees are treated and 4 are herbicide and 1 is pesticide the ratio is 4/5 or 0.8)
surveys$treatment_ratio <- NA
treatment_types <- data.frame(table(trees[,15:16]))
treatment_types <- treatment_types[treatment_types$Freq > 0,]
unique_ids <- unique(treatment_types$UID)
treatment_ratios <- 0
for (i in 1:length(unique_ids)) {
  id <- unique_ids[i]
  removal <- sum(treatment_types$Freq[treatment_types$UID == id & (treatment_types$treatment_type == 'Herbicide' | treatment_types$treatment_type == 'MechanicalRemoval' | treatment_types$treatment_type == 'Mechanical Removal')])
  total <- sum(treatment_types$Freq[treatment_types$UID == id])
  surveys$treatment_ratio[surveys$UID %in% id] <- round(removal/total, digits = 2)
  print(i)
  }

# round(sum(treatment_types$Freq[(treatment_types$treatment_type == 'Herbicide' | treatment_types$treatment_type == 'MechanicalRemoval' | treatment_types$treatment_type == 'Mechanical Removal')])/sum(treatment_types$Freq), digits = 2)

# set year of treatment column based on unique values in site plan (FIRST_Site)
surveys$year <- NA
y2018s <- unique_survey_classes[c(1,2,3,4,5)]
surveys$year[surveys$FIRST_Site %in% y2018s] <- 2018
y2019s <- unique_survey_classes[c(6:15)]
surveys$year[surveys$FIRST_Site %in% y2019s] <- 2019
year_unique <- unique(surveys$year)


writeOGR(obj=surveys, dsn="H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator", layer="surveys", driver="ESRI Shapefile",overwrite_layer=TRUE)

## Make plots of treatment options
toh2$TreatmentP[toh2$TreatmentP == 'MechanicalRemoval'] <- 'Mechanical Removal'
treated_trees <- data.frame(table(toh2$TreatmentP))
names(treated_trees) <- c("Treatment", "Tree_Count")
treated_trees <- treated_trees[1:3,]
library(ggplot2)
ggplot(data=treated_trees, aes(x=Treatment, y=Tree_Count, fill=Tree_Count)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Tree_Count), vjust=1.6, color="black", size=3.5) +
  theme(legend.position="none")

# Plot for Properties and treatments
property_treatments <- data.frame(surveys)
property_treatments <- data.frame(table(property_treatments[,12:13]))
property_treatments <- property_treatments[property_treatments$treatment != 'unknown',]
property_treatments <- property_treatments[property_treatments$treatment != 'none',]

ggplot(data=property_treatments, aes(x=treatment, y=Freq, fill=year)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  geom_text(aes(label=Freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5)



## Make a Treatment raster based on new data.
treatment_parcels <- surveys[surveys$treatment == 'complete',]



## read in treatment data
treatments <- readOGR("H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/onlytreated.shp")
plot(treatments)
infect <- raster(infected_file)
plot(infect)
plot(treatments, add = TRUE)
treatments <- spTransform(treatments, CRS = crs(infect))
treatment_raster <- rasterize(treatments, infect, fun = "last", getCover = TRUE)
writeRaster(treatment_raster, "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_2019.tif", overwrite = TRUE)


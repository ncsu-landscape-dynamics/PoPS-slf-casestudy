library(rgdal)
library(raster)
## read in extent area
#reference_area <- readOGR("C:/Users/cmjone25/Google Drive/California/larger_region_no_west_from_pseodo_mercator_reproj_to_lcc.shp")
reference_area <- readOGR("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/larger_region_no_west_from_pseodo_mercator_in_pseudo_mercator.shp")

## read in tree of heaven maxent predictions and reproject to same as extent area
tree_of_heaven <- raster("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/CONUS_Maxent_Ailanthus_avg_lcc.tif")
tree_of_heaven <- projectRaster(tree_of_heaven, crs = crs(reference_area))

## read in tree of heaven points and project to extent area
# tof_points <- readOGR("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/Ailanthus_Combined_PDA_FIA_Pres11015_GCS.shp")
# tof_points <- spTransform(tof_points, CRS = crs(reference_area))
# 
# ## calculate the maximum number of tree of heaven found in a cell for the maximum to multiply by maxent predictions and remove any prediction less than 0.4
# check_points <- table(cellFromXY(tree_of_heaven, tof_points))
# tree_of_heaven <- tree_of_heaven*max(check_points)
tree_of_heaven[tree_of_heaven < 0.4] <- 0
tree_of_heaven <- round(tree_of_heaven * 100)

## crop to the reference area
tree_of_heaven <- crop(tree_of_heaven, reference_area)
writeRaster(tree_of_heaven, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/tree_of_heaven.tif", overwrite = TRUE)

## create total host map based on maximum number of tree of heaven in a cell
total_hosts <- tree_of_heaven
total_hosts[total_hosts >= 0] <- maxValue(total_hosts)
writeRaster(total_hosts, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/total_hosts.tif", overwrite = TRUE)

## read in slf detections and create initial slf infection maps
host_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/tree_of_heaven.tif"
host <- raster(host_file)
slf_detections <- readOGR("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/slf_detections.shp")
slf_detections_2014 <- slf_detections[slf_detections$Year == 2014,]
slf_detections_2015 <- slf_detections[slf_detections$Year <= 2015,]
slf_detections_2016 <- slf_detections[slf_detections$Year <= 2016,]
slf_detections_2017 <- slf_detections[slf_detections$Year <= 2017,]
slf_2014 <- rasterize(slf_detections_2014, host, fun='count')
slf_2015 <- rasterize(slf_detections_2015, host, fun='count')
slf_2016 <- rasterize(slf_detections_2016, host, fun='count')
slf_2017 <- rasterize(slf_detections_2017, host, fun='count')
slf_2014 <- slf_2014$count2
slf_2015 <- slf_2015$count2
slf_2016 <- slf_2016$count2
slf_2017 <- slf_2017$count2
slf_2014[is.na(slf_2014)] <- 0
slf_2015[is.na(slf_2015)] <- 0
slf_2016[is.na(slf_2016)] <- 0
slf_2017[is.na(slf_2017)] <- 0

infected_years_2015_2016 <- stack(slf_2015, slf_2016)
infected_years_2015_2017 <- stack(slf_2015, slf_2016, slf_2017)
writeRaster(infected_years_2015_2016, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/infected_years_2015_2016.tif", overwrite = TRUE)
writeRaster(infected_years_2015_2017, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/infected_years_2015_2017.tif", overwrite = TRUE)
writeRaster(slf_2014, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2014_single_count_pm.tif", overwrite = TRUE)
writeRaster(slf_2015, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2015_single_count_pm.tif", overwrite = TRUE)
writeRaster(slf_2016, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2016_single_count_pm.tif", overwrite = TRUE)
writeRaster(slf_2017, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2017_single_count_pm.tif", overwrite = TRUE)

## set up a raster of all 0's for fake treatments
treatments <- host
raster::values(treatments) <- 0
writeRaster(treatments, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_0s.tif")

## add quarantine counties to county map
counties <- readOGR("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/slf_county_boundaries.shp")
quarantine <- c("Lebanon", "Lancaster", "Chester", "Delaware", "Philadelphia", "Montgomery", "Bucks", "Berks", "Schuylkill", "Carbon", "Lehigh", "Northampton", "Monroe")
counties$quarantine ="No"
counties$quarantine[counties$NAME %in% quarantine & counties$STATE_NAME == "Pennsylvania"] <- "Yes"

writeOGR(obj=counties, dsn="H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator", layer="slf_county_boundaries", driver="ESRI Shapefile",overwrite_layer=TRUE)

## Weather data setup
january <- seq(1,31,1)
new <- january
for (i in 2:length(dates)){
  new <- new+365
  january <- c(january, new)
}
jan_temp <- tavg_s[[january]]
indices2 <- rep(1,31)
new_ind <- indices2
for (i in 2:length(dates)){
  new_ind <- new_ind+1
  indices2 <- c(indices2, new_ind)
}
crit_temp <- stackApply(jan_temp, indices = indices2, fun=min)

crit_temp <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/crit_temp_slf_1980_2017.tif")
temp_coeff <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/temp_coefficient_slf_1980_2017.tif")
crit_temp <- projectRaster(crit_temp, crs = crs(reference_area))
crit_temp <- resample(crit_temp, res)
crit_temp <- crop(crit_temp, reference_area)
temp_coeff <- projectRaster(temp_coeff, crs = crs(reference_area))
temp_coeff <- resample(temp_coeff, res)
temp_coeff <- crop(temp_coeff, reference_area)
writeRaster(crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/crit_temp_slf_1980_2017_pm.tif")
writeRaster(temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/temp_coefficient_slf_1980_2017_pm.tif")


indices3 <- rep(1,12)
new_ind2 <- indices3
for (i in 2:length(dates)){
  new_ind2 <- new_ind2+1
  indices3 <- c(indices3, new_ind2)
}
temp_coeff_ranking <- stackApply(temp_coeff, indices = indices3, fun = mean)


cm <- c(-Inf, -12, 1, 
        -12, Inf, 2)
cmat <- matrix(cm, ncol=3, byrow=TRUE)
crit_temp_area <- reclassify(crit_temp, cmat)

weather_ranking <- data.frame(year = seq(1980,2017,1), crit_temp_area = rep(0,38), avg_temp_coeff = rep(0,38))
for (i in 1:nlayers(crit_temp_area)) {
  cta <- crit_temp_area[[i]]
  weather_ranking$crit_temp_area[i] <- sum(cta[cta == 1])/ncell(cta)
  weather_ranking$avg_temp_coeff[i] <- mean(temp_coeff_ranking[[i]])
}

## Randomly chose 5 years from the 30% of years (10 years) from 1980 to 2017 best suited for SLF spread (warm winters) in this case years are 1999, 2007, 2004, 1991, 1990
high_spread_ind <- c(seq(1+12*20,12+12*20),seq(1+12*28,12+12*28),seq(1+12*25,12+12*25),seq(1+12*12,12+12*12),seq(1+12*11,12+12*11))
high_spread_temp_coeff <- temp_coeff[[high_spread_ind]]
high_spread_crit_temp <- crit_temp[[c(20,28,25,12,11)]] 
## Randomly chose 5 years from the 30% of years (10 years) from 1980 to 2017 least suited for SLF spread (cold winters) in this case years are 2000,2011, 2016, 1988, 2009
low_spread_ind <- c(seq(1+12*21,12+12*21),seq(1+12*32,12+12*32),seq(1+12*37,12+12*37),seq(1+12*9,12+12*9),seq(1+12*30,12+12*30))
low_spread_temp_coeff <- temp_coeff[[low_spread_ind]]
low_spread_crit_temp <- crit_temp[[c(21, 32, 37,9,30)]]
low_spread_temp_coeff <- projectRaster(low_spread_temp_coeff, crs = crs(reference_area))
low_spread_crit_temp <- projectRaster(low_spread_crit_temp, crs = crs(reference_area))
low_spread_temp_coeff <- resample(low_spread_temp_coeff, res)
low_spread_temp_coeff <- crop(low_spread_temp_coeff , reference_area)
low_spread_crit_temp <- resample(low_spread_crit_temp, res)
low_spread_crit_temp <- crop(low_spread_crit_temp, reference_area)
## Randomly chose 5 years from the 30% of years (10 years) from 1980 to 2017 least suited for SLF spread (cold winters) in this case years are 2000,2011, 2016, 1988, 2009
avg_spread_ind <- c(seq(1+12*35,12+12*35),seq(1+12*1,12+12*1),seq(1+12*22,12+12*22),seq(1+12*18,12+12*18),seq(1+12*19,12+12*19))
avg_spread_temp_coeff <- temp_coeff[[low_spread_ind]]
avg_spread_crit_temp <- crit_temp[[c(35,1,22,18,19)]]
avg_spread_temp_coeff <- projectRaster(avg_spread_temp_coeff, crs = crs(reference_area))
avg_spread_crit_temp <- projectRaster(avg_spread_crit_temp, crs = crs(reference_area))
avg_spread_temp_coeff <- resample(avg_spread_temp_coeff, res)
avg_spread_temp_coeff <- crop(avg_spread_temp_coeff , reference_area)
avg_spread_crit_temp <- resample(avg_spread_crit_temp, res)
avg_spread_crit_temp <- crop(avg_spread_crit_temp, reference_area)

writeRaster(crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/crit_temp_slf_1980_2017.tif")
writeRaster(temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/temp_coefficient_slf_1980_2017.tif")
## write high spread, avg, spread, and low spread scenarios
writeRaster(high_spread_crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_crit_temp_slf_2018_2022.tif")
writeRaster(low_spread_crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_crit_temp_slf_2018_2022_pm.tif", overwrite = TRUE)
writeRaster(high_spread_temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_temp_coefficient_slf_2018_2022.tif", overwrite = TRUE)
writeRaster(low_spread_temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_temp_coefficient_slf_2018_2022_pm.tif", overwrite = TRUE)

writeRaster(avg_spread_crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/avg_spread_crit_temp_slf_2018_2022_pm.tif", overwrite = TRUE)
writeRaster(avg_spread_temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/avg_spread_temp_coefficient_slf_2018_2022_pm.tif", overwrite = TRUE)


## forgot to reproject
res <- raster("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/total_hosts_pm.tif")
high_spread_temp_coeff <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_temp_coefficient_slf_2018_2022.tif")
high_spread_temp_coeff <- projectRaster(high_spread_temp_coeff, crs = crs(reference_area))
high_spread_temp_coeff <- resample(high_spread_temp_coeff, res)
high_spread_temp_coeff <- crop(high_spread_temp_coeff, reference_area)
low_spread_temp_coeff <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_temp_coefficient_slf_2018_2022.tif")
low_spread_temp_coeff <- projectRaster(low_spread_temp_coeff, crs = crs(reference_area))
low_spread_temp_coeff <- resample(low_spread_temp_coeff, res)
low_spread_temp_coeff <- crop(low_spread_temp_coeff , reference_area)
high_spread_crit_temp <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_crit_temp_slf_2018_2022.tif")
high_spread_crit_temp <- projectRaster(high_spread_crit_temp, crs = crs(reference_area))
high_spread_crit_temp <- resample(high_spread_crit_temp, res)
high_spread_crit_temp <- crop(high_spread_crit_temp, reference_area)
low_spread_crit_temp <- stack("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_crit_temp_slf_2018_2022.tif")
low_spread_crit_temp <- projectRaster(low_spread_crit_temp, crs = crs(reference_area))
low_spread_crit_temp <- resample(low_spread_crit_temp, res)
low_spread_crit_temp <- crop(low_spread_crit_temp, reference_area)
writeRaster(high_spread_crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_crit_temp_slf_2018_2022_pm.tif", overwrite = TRUE)
writeRaster(low_spread_crit_temp, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_crit_temp_slf_2018_2022_pm.tif", overwrite = TRUE)
writeRaster(high_spread_temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/high_spread_temp_coefficient_slf_2018_2022_pm.tif", overwrite = TRUE)
writeRaster(low_spread_temp_coeff, "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/low_spread_temp_coefficient_slf_2018_2022_pm.tif", overwrite = TRUE)

## Read in, project, and crop cropscape data to study area
cropscape <- raster("H:/Team Drives/APHIS  Projects/shared resources/data/cropscape_30m_2017/2017_30m_cdls.img")
cropscape <- projectRaster(cropscape, crs = crs(reference_area))

## read in treatment band for APHIS SLF area
aphis_treatment_area <- readOGR("H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/raw_data/LeoBand.shp")
aphis_treatment_area <- spTransform(aphis_treatment_area, crs(reference_area))
writeOGR(obj=aphis_treatment_area, dsn="H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator", layer="aphis_treatment_area", driver="ESRI Shapefile")

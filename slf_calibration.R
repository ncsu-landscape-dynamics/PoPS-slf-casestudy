devtools::install_github("ncsu-landscape-dynamics/rpops", ref = "feature/parallel_calibration", force = TRUE)
library(PoPS)
library(raster)
infected_years_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/infected_years_2015_2016.tif"
num_iterations = 200000
start_reproductive_rate = 1.5
start_short_distance_scale = 55
sd_reproductive_rate = 0.2
sd_short_distance_scale = 0.75

infected_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2014_single_count_pm.tif"
host_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/tree_of_heaven.tif"
total_plants_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/total_hosts.tif"
temperature_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/crit_temp_slf_2014_2017.tif"
temperature_coefficient_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/temp_coefficient_slf_2014_2017.tif"
precipitation_coefficient_file =""
use_lethal_temperature = TRUE
temp = TRUE
precip = FALSE
season_month_start = 6
season_month_end = 11
time_step = "month"
start_time = 2015
end_time = 2016
dispersal_kern = "cauchy"
percent_short_distance_dispersal = 1.0
short_distance_scale = 45
long_distance_scale = 0.0
lethal_temperature = -35
lethal_temperature_month = 1
wind_dir = "NONE"
kappa = 0
random_seed = 42
reproductive_rate = 0.3
treatments_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_0s.tif"
treatment_years = c(0)
management = TRUE
mortality_on = FALSE
mortality_rate = 0
mortality_time_lag = 0

params_slf <- calibrate(infected_years_file, num_iterations, start_reproductive_rate, 
                        start_short_distance_scale, sd_reproductive_rate, sd_short_distance_scale,
                        infected_file, host_file, total_plants_file, reproductive_rate,
                        use_lethal_temperature, temp, precip, management, mortality_on,
                        temperature_file, temperature_coefficient_file, 
                        precipitation_coefficient_file, treatments_file,
                        season_month_start, season_month_end, time_step,
                        start_time, end_time, treatment_years,
                        dispersal_kern, percent_short_distance_dispersal,
                        short_distance_scale, long_distance_scale,
                        lethal_temperature, lethal_temperature_month,
                        mortality_rate, mortality_time_lag,
                        wind_dir, kappa)

hist(params_slf$reproductive_rate, breaks = 100)
hist(params_slf$short_distance_scale, breaks = 100)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

short_distance_scale = getmode(params_slf$short_distance_scale)
reproductive_rate = getmode(params_slf$reproductive_rate)

## set up validation using results from calibration
devtools::install_github("ncsu-landscape-dynamics/rpops", ref = "master", force = TRUE)
library(PoPS)
library(raster)

infected_years_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2017_single_count_pm.tif"
num_iterations = 1000


infected_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2016_single_count_pm.tif"
host_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/tree_of_heaven_new_extent_pm.tif"
total_plants_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/total_hosts_pm.tif"
temperature_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/crit_temp_slf_2014_2017.tif"
temperature_coefficient_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/temp_coefficient_slf_2014_2017.tif"
precipitation_coefficient_file =""
use_lethal_temperature = TRUE
temp = TRUE
precip = FALSE
season_month_start = 6
season_month_end = 11
time_step = "month"
start_time = 2015
end_time = 2016
dispersal_kern = "cauchy"
percent_short_distance_dispersal = 1.0
# short_distance_scale = 45
long_distance_scale = 0.0
lethal_temperature = -35
lethal_temperature_month = 1
wind_dir = "NONE"
kappa = 0
random_seed = 42
# reproductive_rate = 0.3
treatments_file = "H:/Team Drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_0s.tif"
treatment_years = c(0)
management = TRUE
mortality_on = FALSE
mortality_rate = 0
mortality_time_lag = 0

params_slf <- validate(infected_years_file, num_iterations,
                        infected_file, host_file, total_plants_file, reproductive_rate,
                        use_lethal_temperature, temp, precip, management, mortality_on,
                        temperature_file, temperature_coefficient_file, 
                        precipitation_coefficient_file, treatments_file,
                        season_month_start, season_month_end, time_step,
                        start_time, end_time, treatment_years,
                        dispersal_kern, percent_short_distance_dispersal,
                        short_distance_scale, long_distance_scale,
                        lethal_temperature, lethal_temperature_month,
                        mortality_rate, mortality_time_lag,
                        wind_dir, kappa)

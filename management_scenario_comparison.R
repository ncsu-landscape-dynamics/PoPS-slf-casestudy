library(PoPS)
library(raster)

infected_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/initial_infections_2018_single_count_pm_prop.tif"
host_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/tree_of_heaven_0.50.tif"
total_plants_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/total_hosts.tif"
temperature_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/avg_spread_crit_temp_slf_2018_2022_pm.tif"
temperature_coefficient_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/avg_spread_temp_coefficient_slf_2018_2022_pm.tif"
precipitation_coefficient_file =""
use_lethal_temperature = TRUE
temp = TRUE
precip = FALSE
season_month_start = 6
season_month_end = 11
time_step = "month"
start_time = 2019
end_time = 2023
dispersal_kern = "cauchy"
percent_short_distance_dispersal = 1.0
short_distance_scale = 45
long_distance_scale = 0.0
lethal_temperature = -14
lethal_temperature_month = 1
random_seed = NULL
reproductive_rate = 2.1
# treatments_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_0s.tif"
treatments_file = "H:/Shared drives/APHIS  Projects/PoPS/Case Studies/spotted_latternfly/slf_6_state_region_psuedo_mercator/treatment_2019.tif"
treatment_years = c(2019)
treatment_month <- 5
treatment_method <- "ratio"
management = TRUE
mortality_on = FALSE
mortality_rate = 0
mortality_time_lag = 0
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 29
anthropogenic_distance_scale <- 0.0
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0

num_iterations <- 1000
number_cores <- 11


management = TRUE
data <- PoPS::pops_multirun(infected_file, host_file, total_plants_file, 
                   temp, temperature_coefficient_file, 
                   precip, precipitation_coefficient_file, 
                   time_step, reproductive_rate,
                   season_month_start, season_month_end, 
                   start_time, end_time, 
                   use_lethal_temperature, temperature_file,
                   lethal_temperature, lethal_temperature_month,
                   mortality_on, mortality_rate, mortality_time_lag, 
                   management, treatment_years, treatments_file,
                   treatment_method, treatment_month,
                   percent_natural_dispersal,
                   natural_kernel_type, anthropogenic_kernel_type,
                   natural_distance_scale, anthropogenic_distance_scale,
                   natural_dir, natural_kappa, 
                   anthropogenic_dir, anthropogenic_kappa,
                   num_iterations = num_iterations, number_cores = number_cores, 
                   random_seed = NULL)


management = FALSE
data_no_management <- PoPS::pops_multirun(infected_file, host_file, total_plants_file, 
                            temp, temperature_coefficient_file, 
                            precip, precipitation_coefficient_file, 
                            time_step, reproductive_rate,
                            season_month_start, season_month_end, 
                            start_time, end_time, 
                            use_lethal_temperature, temperature_file,
                            lethal_temperature, lethal_temperature_month,
                            mortality_on, mortality_rate, mortality_time_lag, 
                            management, treatment_years, treatments_file,
                            treatment_method, treatment_month,
                            percent_natural_dispersal,
                            natural_kernel_type, anthropogenic_kernel_type,
                            natural_distance_scale, anthropogenic_distance_scale,
                            natural_dir, natural_kappa, 
                            anthropogenic_dir, anthropogenic_kappa,
                            num_iterations = num_iterations, number_cores = number_cores, 
                            random_seed = NULL)

nm_infecteds <- data.frame(t(data_no_management[[3]]))
m_infecteds  <- data.frame(t(data[[3]]))
names(nm_infecteds) <- c("infecteds", "sd")
names(m_infecteds) <- c("infecteds", "sd")
nm_infecteds$year <- seq(2019, 2023, 1)
m_infecteds$year <- seq(2019, 2023, 1)
nm_infecteds$management <- "None"
m_infecteds$management <- "2019"
infecteds <- rbind(m_infecteds[1:3,], nm_infecteds[1:3,])


nm_area <- data.frame(t(data_no_management[[4]]))
m_area  <- data.frame(t(data[[4]]))

names(nm_area) <- c("area", "sd")
names(m_area) <- c("area", "sd")
nm_area$year <- seq(2019, 2023, 1)
m_area$year <- seq(2019, 2023, 1)
nm_area$management <- "None"
m_area$management <- "2019"


areas <- rbind(m_area[1:3,], nm_area[1:3,])
ggplot(data=areas, aes(x=year, y=area, fill=management)) +
  geom_bar(stat="identity", position=position_dodge())

p_area_saved <- data.frame((nm_area$area-m_area$area)/nm_area$area * 100)
p_area_saved$year <- seq(2019, 2023, 1)
names(p_area_saved) <- c("area_saved", "year")
ggplot(data=p_area_saved, aes(x=year, y=area_saved)) +
  geom_bar(stat="identity")

probability_diff <- data_no_management[[1]] - data[[1]]
probability_diff[probability_diff <= 0] <- NA
probability_diff[[1:3]]
plot(probability_diff[[1:3]])
names(probability_diff) <- c("2020 probability of infection difference", "2021 probability of infection difference", "2022 probability of infection difference", "2023 probability of infection difference", "2024 probability of infection difference")

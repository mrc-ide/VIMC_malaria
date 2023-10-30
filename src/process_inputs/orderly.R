# process inputs for all sites -------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL)

library(data.table)
library(countrycode)
library(dplyr)
# outputs from this report  ----------------------------------------------------
orderly2::orderly_artefact('Site file input', 'site_file.rds')
orderly2::orderly_artefact('Coverage input', 'coverage_input.rds')
orderly2::orderly_artefact('Population input (single year)', 'population_input_single_yr.rds')
orderly2::orderly_artefact('Population input (all ages)', 'population_input_all_age.rds')
orderly2::orderly_artefact('Life expectancy input', 'le_input.rds')
orderly2::orderly_artefact('Mortality rate input', 'mort_rate_input.rds')


# pull site data  --------------------------------------------------------------
site_data <- readRDS(paste0('site_files/', iso3c, '.rds'))
saveRDS(site_data, 'site_file.rds')


# pull coverage data -----------------------------------------------------------
coverage_files<- list.files('vimc_inputs/vaccine_coverage/', full.names = T)
coverage_dt<- rbindlist(lapply(coverage_files, read.csv))

saveRDS(coverage_dt, 'coverage_input.rds')

# pull population data (single year) -------------------------------------------
demog_single_yr<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_int_pop_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

saveRDS(demog_single_yr, 'population_input_single_yr.rds')


# pull population data (all ages) ----------------------------------------------
demog_all_ages<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_tot_pop_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

saveRDS(demog_all_ages, 'population_input_all_age.rds')

# pull life expectancy data ----------------------------------------------------
le<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_life_ex_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

saveRDS(le, 'le_input.rds')


# pull mortality rate data  ----------------------------------------------------
mort<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_mort_rate_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

saveRDS(mort, 'mort_rate_input.rds')


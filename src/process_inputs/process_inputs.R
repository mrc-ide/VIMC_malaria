# process inputs for all sites -------------------------------------------------
parms<- orderly2::orderly_parameters(iso3c = NULL)

pkgs <- c( 'data.table', 'countrycode',  'dplyr')
invisible(lapply(pkgs, library, character.only = TRUE))

# outputs from this report  ----------------------------------------------------
orderly2::orderly_artefact('Site file input', 'site_file.rds')
orderly2::orderly_artefact('VIMC input', 'vimc_input.rds')


# pull coverage data -----------------------------------------------------------
# first pull in proxy data 
proxy<- readRDS('proxy_scenario.rds') 
input_params<- read.csv('vaccine_scenario_inputs.csv') 

input_params<- input_params |> dplyr::mutate(iso3c = country_code)

proxy<- merge(proxy, input_params, by= c('iso3c'))
# specify vaccine type based on what is currently being implemented
primary<- copy(proxy) |>
  dplyr::select(-booster) |>
  dplyr::mutate(vaccine = ifelse(vaccine == 'R21', 'R3', 'RTS3'))

secondary<- copy(proxy) |>
  select(-coverage) |>
  rename(coverage = booster) |>
  mutate(vaccine = ifelse(vaccine == 'R21', 'R4', 'RTS4'))

full<- rbind(primary, secondary, fill = TRUE)

# fill in default values for dose timing
full<- full |>
  mutate(dose1= ifelse(is.na(dose1), 6, dose1), #most countries seem to be administering initial dose at 6 months of age
        dose4= ifelse(is.na(dose4), 18, dose4)) # most countries seem to be administering the booster at 18 months of age

full<- full |>
  filter(iso3c == parms$iso3c)

# VIMC input data
coverage_files<- list.files('vimc_inputs/vaccine_coverage/', full.names = T)
coverage_dt<- rbindlist(lapply(coverage_files, read.csv))

coverage_dt <- coverage_dt |>
  filter(country_code == iso3c) |>
  mutate(coverage = coverage / proportion_risk)  # convert coverage to per population instead of per population at risk


novax<- coverage_dt |>           # pull another projection for data table structure and fill with zeroes
  dplyr::filter(country_code == iso3c) |>
  dplyr::filter(scenario == 'malaria-rts3-rts4-default') |>
  mutate(coverage = 0)  |>
  mutate(scenario = 'no-vaccination')

coverage_dt<- rbind(coverage_dt, novax)
coverage_dt<- rbind(coverage_dt, full, fill = TRUE)
# pull population data (single year) -------------------------------------------
demog_single_yr<- read.csv('vimc_inputs/demography/202409malaria-1_dds-202208_int_pop_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

# pull population data (all ages) ----------------------------------------------
demog_all_ages<- read.csv('vimc_inputs/demography/202409malaria-1_dds-202208_tot_pop_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

# pull life expectancy data ----------------------------------------------------
le<- read.csv('vimc_inputs/demography/202409malaria-1_dds-202208_life_ex_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)

# pull mortality rate data  ----------------------------------------------------
mort<- read.csv('vimc_inputs/demography/202409malaria-1_dds-202208_mort_rate_both.csv') |>
  filter(country_code == iso3c) |>
  filter(year >= 2000)


# make a list of all dataframes needed for one country to reduce dependency calls
vimc_input<- list('coverage_input' = coverage_dt,
                  'population_input_single_yr' = demog_single_yr,
                  'population_input_all_age' = demog_all_ages,
                  'le_input' = le,
                  'mort_rate_input' = mort)

saveRDS(vimc_input, 'vimc_input.rds')

# pull site data  --------------------------------------------------------------
site_data<- site::fetch_site(iso3c = {iso3c}, urban_rural = TRUE)
saveRDS(site_data, 'site_file.rds')



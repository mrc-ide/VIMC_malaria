# process site --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             site_name = NULL,
                             ur = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL,
                             quick_run = NULL)

orderly2::orderly_description('Process model outputs')
orderly2::orderly_artefact('Processed output', 'processed_output.rds')
orderly2::orderly_artefact('model output', 'raw_model_output.rds')

# packages  
library(postie)
library(dplyr)
library(data.table)
library(dplyr)

# read inputs ------------------------------------------------------------------
# model input
orderly2::orderly_dependency("launch_models",
                             "latest(parameter:iso3c == this:iso3c && 
                                     parameter:site_name == this:site_name && 
                                     parameter:ur == this:ur && 
                                     parameter:population == this:population &&
                                     parameter:scenario == this:scenario &&
                                     parameter:description == this:description &&
                                     parameter:quick_run == this:quick_run)",
                             c(model_output.rds = "model_output.rds"))
# site file
orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(site_file.rds = "site_file.rds"))
# life expectancy
orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(le_input.rds = "le_input.rds"))
# population
orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(population_input_all_age.rds = "population_input_all_age.rds"))
orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(population_input_single_yr.rds = "population_input_single_yr.rds"))


output<- readRDS('model_output.rds')
site_data <- readRDS('site_file.rds')
le <- readRDS('le_input.rds')
vimc_pop<- readRDS('population_input_all_age.rds')
pop_single_yr<- readRDS('population_input_single_yr.rds')

message('read inputs successfully')

# calculate rates --------------------------------------------------------------
output<- drop_burnin(output, burnin= burnin* 365)
saveRDS(output, 'raw_model_output.rds') # save for diagnostics

output <- postie::get_rates(
  output,
  time_divisor = 365,
  baseline_t = 1999,
  age_divisor = 365,
  scaler = 0.215,
  treatment_scaler = 0.517,
)

# fill rates out to single year age groups
output<- output |>
  dplyr::group_by(t) |>
  tidyr::complete(age_lower = c(1:100)) |>
  select(-age_upper) |>
  dplyr::ungroup() |>
  tidyr::fill(clinical, severe, mortality, yld_pp, yll_pp, dalys_pp, .direction = 'down') |>
  select(-prop_n, -n)
  
#if(quick_run == F){
  
# recalculate YLLs and DALYs based on country-specific life expectancy  --------
output<- output |>
  select(-yll_pp, -dalys_pp) |>
  rename(year = t)

# merge in inputs for expected remaining years of life (to calculate YLLs)
le<- le |>
  filter(country_code == iso3c,
         year >= 2000) |>
  dplyr::group_by(year) |>
  tidyr::complete(age_from = c(1:100)) |>
  dplyr::ungroup() |>
  tidyr::fill(dplyr::all_of(names(le)), .direction = "down")

# fill years out (five year age groups)
le<- le |>
  dplyr::group_by(age_from) |>
  tidyr::complete(year = c(2000:2100))|>
  dplyr::ungroup() |>
  tidyr::fill(dplyr::all_of(names(le)), .direction = "down") |>
  rename(age_lower = age_from,
         remaining_yrs = value) |>
  select(year, age_lower, remaining_yrs) 

# calculate ylls_pp + dalys per person
dt<- merge(output, le, by = c('year', 'age_lower'), all.x = TRUE)

dt<- dt |>
  mutate(ylls_pp = mortality * remaining_yrs) |>
  mutate(dalys_pp = ylls_pp + yld_pp) |>
  select(-remaining_yrs)


# calculate counts  ------------------------------------------------------------
# merge in population from site files (as we only have VIMC inputs for the national level)
# first, separately sum cases by year
total_pop<- site_data$population |>
  group_by(year) |>
  summarise(summed_pop = sum(pop))

# pull the population for the site of interest
pop <- site_data$population |>
  filter(name_1 == site_name & urban_rural == ur) |>
  select(year, pop) |>
  rename(site_file_population = pop)

# merge these two tables together
pops<- merge(pop, total_pop, by= 'year')

# merge in national population from VIMC (available for entire time period)
vimc_pop<- vimc_pop |>
  filter(country_code == iso3c,
         year >= 2000)|>
  rename(national_pop = value)|>
  select(year, national_pop)

# merge in vimc population
pops<- merge(vimc_pop, pops, all.x = T)

# first rescale site file population based on the ratio of (sum of site file pops in country)/ (VIMC country level population)
# should be more or less the same, but should be done for consistency sake

pops<- pops |>
  mutate(vimc_site_population = (site_file_population * national_pop)/summed_pop)

# calculate population ratio as vimc(site)/ vimc(country)
pops<- pops |>
  mutate(pop_ratio = vimc_site_population/ national_pop) |>
  tidyr::fill(pop_ratio, .direction = 'down')

# then calculate vimc_site_population by multiplying this ratio to the national population for the final 50 years
pops<- pops |>
  mutate(vimc_site_population = ifelse(year<= 2050, vimc_site_population, pop_ratio* national_pop))

# subset out site file population for 2000-2100
site_pop<- pops |>
  select(year, vimc_site_population)

# pull in single year population to calculate proportion_n by age group
national_pop<- pops |>
  select(year, national_pop)
pop_single_yr<- merge(pop_single_yr, national_pop, by = c('year'))
pop_single_yr <- pop_single_yr |>
  mutate(prop_n = value/ national_pop) |>
  select(year, age_from, age_to, prop_n) |>
  rename(age_lower = age_from)


# merge in site population
dt<- merge(dt, site_pop, by= 'year')

# merge in prop_n
dt<- merge(dt, pop_single_yr, by = c('year', 'age_lower'))


# calculate counts for entire time period --------------------------------------
dt<- dt |>
  mutate(
    cases = round(clinical * vimc_site_population * prop_n),
    deaths = round(mortality * vimc_site_population * prop_n),
    dalys = round(dalys_pp * vimc_site_population * prop_n),
    population = round(vimc_site_population * prop_n)) |>
  select(-prop_n)

# final formatting  ------------------------------------------------------------
dt <- dt |>
  mutate(
    disease = 'Malaria',
    country = iso3c,
    country_name = countrycode::countrycode(
      sourcevar = iso3c,
      origin = 'iso3c',
      destination = 'country.name'),
    site_name = site_name,
    urban_rural = ur,
    scenario = scenario,
    description = description
  ) |>
  rename(age = age_lower,
         cohort_size = population) |>
  select(
    disease,
    year,
    age,
    country,
    country_name,
    site_name,
    urban_rural,
    scenario,
    description,
    cohort_size,
    cases,
    dalys,
    deaths,
    clinical,
    mortality,
    dalys_pp
  ) |>
  mutate(
    cases = if_else(is.na(cases), 0, cases),
    deaths = if_else(is.na(deaths), 0, deaths),
    dalys = if_else(is.na(dalys), 0, dalys),
    mortality = if_else(is.na(mortality), 0, mortality),
    clinical = if_else(is.na(clinical), 0, clinical),
    dalys = if_else(is.na(dalys), 0, dalys)
  )


saveRDS(dt, 'processed_output.rds')



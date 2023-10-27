# process site --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             site_name = NULL,
                             ur = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL)

# packages  
library(postie)
library(dplyr)
library(data.table)
library(dplyr)
library(wesanderson)
library(ggforce)
library(ggpubr)

source('diagnostics.R')

sites <- readRDS(paste0('site_files/', iso3c, '.rds'))

orderly2::orderly_description('Process model outputs')
orderly2::orderly_artefact('Processed output', 'processed_output.rds')
orderly2::orderly_artefact('model output', 'raw_model_output.rds')

# pull inputs ------------------------------------------------------------------
metadata<- orderly2::orderly_dependency("launch_models",
                                        "latest(parameter:iso3c == this:iso3c && 
                                        parameter:site_name == this:site_name && 
                                        parameter:ur == this:ur && 
                                        parameter:population == this:population &&
                                        parameter:scenario == this:scenario &&
                                        parameter:description == this:description)",
                                        c(model_output.rds = "model_output.rds"))

output<- readRDS('model_output.rds')
message('read input successfully')

# drop burn-in
output<- drop_burnin(output, burnin= burnin* 365)
saveRDS(output, 'raw_model_output.rds') # save for diagnostics

output <- postie::get_rates(
  output,
  time_divisor = 365,
  baseline_t = 2000,
  age_divisor = 365,
  scaler = 0.215,
  treatment_scaler = 0.42,
)

# remove DALYs, as YLLs will need to be recalculated using VIMC country-specific life expectancies
output<- output |>
  select(-yll_pp, -dalys_pp) |>
  rename(year = t)

# merge in population from site files (as we only have VIMC inputs for the national level)
site_data <- readRDS(paste0('site_files/', iso3c, '.rds'))
pop <- site_data$population |>
  filter(name_1 == site_name & urban_rural == ur) |>
  select(year, pop) |>
  rename(site_file_population = pop)

# merge in inputs for expected remaining years of life (to calculate YLLs)
le<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_life_ex_both.csv') 

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

# merge in national population
vimc_pop<- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_tot_pop_both.csv') |>
  filter(country_code == iso3c,
         year >= 2000)|>
  rename(national_pop = value)|>
  select(year, national_pop)

dt <- merge(output, pop, by = 'year', all.x = T)
dt<- merge(dt, le, by = c('year', 'age_lower'))
dt<- merge(dt, vimc_pop, by = 'year', all.x = T)

# calculate ylls_pp + dalys pp
dt<- dt |>
  mutate(ylls_pp = mortality * remaining_yrs) |>
  mutate(dalys_pp = ylls_pp + yld_pp) |>
  select(-remaining_yrs)

# calculate counts up to 2050
dt<- dt |>
  mutate(
    cases = round(clinical * site_file_population * prop_n),
    deaths = round(mortality * site_file_population * prop_n),
    dalys = round(dalys_pp * site_file_population * prop_n),
    population = round(site_file_population * prop_n))

# calculate some scaling factor for population from 2050-21-- in leiu of site-specific pop for these years
later_yrs<- dt |>
  mutate(pop_ratio = site_file_population / national_pop) |>
  tidyr::fill(pop_ratio, .direction = "down") |>
  filter(year >= 2051) |>
  mutate(site_file_population = pop_ratio * national_pop) |>
  mutate(cases = round(clinical * site_file_population * prop_n),
         deaths = round(mortality * site_file_population * prop_n),
         dalys = round(dalys_pp * site_file_population * prop_n),
         population = round(site_file_population * prop_n)) |>
  select(-pop_ratio)

# bid years after 2050 and years before 2050 together
dt<- dt |>
  filter(year <= 2050)
full_dt<- rbind(dt, later_yrs, fill= T)

# final formatting
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
    prop_n,
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



# set parameters  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL, 
                             site_name = NULL,
                             ur = NULL,
                             description = NULL,
                             population = NULL,
                             scenario = NULL,
                             parameter_draw = NULL,
                             burnin= NULL)


orderly2::orderly_description('Set parameters for model run')
orderly2::orderly_artefact('Model input', 'model_input.rds')
orderly2::orderly_artefact('Vaccine plot input', 'vaccine_plot_input.rds')

# packages
library(site)
library(data.table)
library(dplyr)
library(scene)
library(malariasimulation)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(tibble)
# functions
lapply(list.files('functions/', full.names = T), source)

orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(coverage_input.rds = "coverage_input.rds"))

coverage_data<- readRDS('coverage_input.rds') 

scen<- scenario # doesn't work when scenario has the same name

if(scen == 'no-vaccination'){
  
  coverage_data<- coverage_data |>           # pull another projection for data table structure
    filter(country_code == iso3c) |>
    filter(scenario == 'malaria-r3-r4-default') |>
    mutate(coverage == 0)
  
}

coverage_data<- coverage_data |>           # pull another projection for data table structure
  filter(country_code == iso3c) |>
  filter(scenario == scen)

orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(site_file.rds = "site_file.rds"))
# pull site data  
site_data <- readRDS('site_file.rds')
site <- extract_site(site_file = site_data,
                     site_name = site_name,
                     ur = ur)

# specify vaccine coverage based on forecast  ----------------------------------
site<- expand_intervention_coverage(site, 
                                    terminal_year = 2100)

site<- update_coverage_values(site, 
                              coverage_data)

saveRDS(site, 'vaccine_plot_input.rds')
message('formatting')

# pull parameters for this site ------------------------------------------------
params <- site::site_parameters(
  interventions = site$interventions,
  demography = site$demography,
  vectors = site$vectors,
  seasonality = site$seasonality,
  eir = site$eir$eir[1],
  burnin = burnin,
  overrides = list(human_population = population)
)




# set age groups  --------------------------------------------------------------
year<- 365
min_ages = seq(0, 99, by= 1) * year
max_ages = seq(1, 100, by= 1) * year -1

params$clinical_incidence_rendering_min_ages = min_ages
params$clinical_incidence_rendering_max_ages = max_ages
params$severe_incidence_rendering_min_ages = min_ages
params$severe_incidence_rendering_max_ages = max_ages
params$age_group_rendering_min_ages = min_ages
params$age_group_rendering_max_ages = max_ages


# set mortality   --------------------------------------------------------------
message('setting mortality')

# if this is a stochastic run, set parameter draw
if (parameter_draw > 0){
  
params<- params |>
  set_parameter_draw(parameter_draw) |>
  set_equilibrium(init_EIR= params$init_EIR)

}

inputs <- list(
  'param_list' = params,
  'site_name' = site_name,
  'ur' = ur,
  'iso' = iso3c,
  'scenario' = scenario,
  'description' = description,
  'parameter_draw' = parameter_draw
)


message('saving model input')
saveRDS(inputs, 'model_input.rds')










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
orderly2::orderly_artefact('Intervention plot', 'intervention_plot.pdf')

# packages
library(site)
library(data.table)
library(dplyr)
library(scene)
library(malariasimulation)
library(openxlsx)

# functions
source('set_demog.R')
source('set_vaccine_coverage.R')
source('extract_site.R')

# pull site data
site_data <- readRDS(paste0('site_files/', iso3c, '.rds'))

site <- extract_site(site_file = site_data,
               site_name = site_name,
               ur = ur)

# specify vaccine coverage based on forecast  ----------------------------------
site<- set_vaccine_coverage(site, scenario = scenario, 
                            terminal_year = 2050, 
                            rtss_target= 0.8,
                            rtss_year = 2023)
      
# plot input parameters
pdf('intervention_plot.pdf')
plot_interventions_combined(
  interventions = site$interventions,
  population = site$population,
  group_var = c("country", "name_1"),
  include = c("itn_use", "itn_input_dist", "tx_cov", "smc_cov", "pmc_cov", "rtss_cov"),
  labels = c("ITN usage", "ITN model input", "Treatment","SMC", "PMC", "RTSS")
)
dev.off()

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

# if (demog= TRUE){
# params <- set_demog(params)
# 
# }

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










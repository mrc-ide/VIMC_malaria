# VIMC orderly workflow
# author: Lydia Haile
# orderly workflow for VIMC model runs. This script: 
# 1) formats input parameters for model runs
# 2) runs models
# 3) processes country outputs
# 4) generates diagnostics
################################################################################

# packages  
library(orderly2)
library(site)

lapply(list.files('functions/', full.names = T), source)

# obtain list of countries to run model for
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)

dir<- getwd()
source('run_report.R')

# PARAMETERS TO CHANGE FOR REPORTS ---------------------------------------------
iso3c<- 'NGA'                                                                   # country to launch model for
sites<- readRDS(paste0('src/process_inputs/site_files/', iso3c, '.rds'))$sites  # sites for country of interest
population<- 5000                                                               # population size
description<- 'quick_run_rtss'                                                  # reason for model run
draw<- 0                                                                        # parameter draw to run (0 for central runs)
burnin<- 5                                                                      # burn-in in years            
quick_run<- TRUE                                                                # boolean, T or F. If T, makes age groups larger and runs model through 2035.

# if just testing reports for one site:
site_name<- 'Lagos'
ur<- 'urban'

# 2 following reports reports to run (in chronological order)
reports<- c('set_parameters', 'launch_models', 'process_site', 'site_diagnostics', 'process_country')
report_type<- reports[1]   # select a report to run

# scenarios to run (no order)
scenarios<- c('malaria-no-vaccination', 'malaria-r3-default', 'malaria-r3-r4-default', 'malaria-rts3-bluesky', 'malaria-rts3-default', 'malaria-rts3-rts4-bluesky')
scenario<-  'malaria-rts3-rts4-default'   # select a scenario to run per VIMC inputs.

################################################################################
# 1 prepare and save inputs
# unless inputs change, this only needs to be run once for all countries
# for (iso3c in iso3cs){
# 
#   orderly2::orderly_run(
#     'process_inputs',
#     list(iso3c = iso3c),
#     root = dir)
# }
# # 

# cluster setup --------------------------------------------------------------
ctx <- context::context_save("contexts", sources= 'run_report.R')
config <- didehpc::didehpc_config(cluster = "big")
obj <- didehpc::queue_didehpc(ctx, config = config)

# if you have not already, install orderly2, malariasimulation, orderly2, and dplyr
#obj$install_packages('mrc-ide/orderly2')

# run reports for all sites in a country  --------------------------------------
print(report_type) # report to run
print(scenario)

small_models<- obj$lapply(
  1:nrow(sites),
  run_report,
  report_name = report_type,
  path = dir,
  site_data = sites,
  population = population,
  description = description,
  scenario = scenario,
  parameter_draw = draw,
  burnin = burnin,
  quick_run = quick_run
)


# # run report just for one site  ------------------------------------------------
smaller_job<- obj$enqueue(orderly2::orderly_run(
  'site_diagnostics',
  list(
    iso3c = iso3c,
    site_name = site_name,
    ur= ur,
    description = description,
    population = population,
    parameter_draw = draw,
    burnin= burnin,
    scenario = scenario,
    quick_run = quick_run),
  root = dir
))

# # run country aggregation
orderly2::orderly_run(
  'process_country',
  list(
    iso3c = iso3c,
    description = description,
    population = population,
    parameter_draw = draw,
    burnin= burnin,
    projection = proj,
    quick_run = quick_run),
  root = dir
)


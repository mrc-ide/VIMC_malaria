################################################################################
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
library(data.table)
library(dplyr)
lapply(list.files('functions/', full.names = T), source)

# obtain list of countries to run model for
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)

dir<- getwd()

# custom functions
source('run_report.R')
source('remove_zero_eirs.R')
source('make_parameter_map.R')

################################################################################
# 1 prepare and save inputs
# unless inputs change, this only needs to be run once for all countries
for (iso3c in iso3cs){

  orderly2::orderly_run(
    'process_inputs',
    list(iso3c = iso3c),
    root = dir)
}


# PARAMETERS TO CHANGE FOR REPORTS ---------------------------------------------
maps<- make_parameter_maps(
  iso3cs = iso3cs[1:5],                                                        # Pick 10 countries to begin with
  population = 100000,                                                          # population size
  description = 'full_model_runs',                                              # reason for model run (change this for every run if you do not want to overwrite outputs)
  parameter_draw = 0,                                                           # parameter draw to run (0 for central runs)
  burnin= 15,                                                                  # burn-in in years            
  quick_run = FALSE                                                             # boolean, T or F. If T, makes age groups larger and runs model through 2035.
)

# reports to run (in chronological order)
reports <- c('set_parameters', 'launch_models', 'process_site', 'site_diagnostics', 'process_country', 'country_diagnostics')



# # cluster setup ----------------------------------------------------------------
ctx <- context::context_save("contexts", sources= 'run_report.R')
config <- didehpc::didehpc_config(
  use_rrq = FALSE,
  cores = 1,
  cluster = "wpia-hn" ,#"fi--dideclusthn", # , "fi--didemrchnb""fi--didemrchnb"
  template = "AllNodes",  ## use for the wpia cluster
  parallel = FALSE)

obj <- didehpc::queue_didehpc(ctx, config = config)

# # if you have not already, install orderly2, malariasimulation, orderly2, and dplyr
#obj$install_packages('mrc-ide/orderly2')


# run report for all sites locally ---------------------------------------------
lapply(
    1:nrow(maps$site_map),
    run_report,
    report_name = 'set_parameters',
    parameter_map = maps$site_map,
    path = dir
  )
  
# run report for all countries locally  ----------------------------------------
map<- data.table(maps$country_map)[scenario == 'malaria-rts3-rts4-default' | scenario == 'no-vaccination']

lapply(
  1:nrow(map),
  run_report_country,
  report_name = 'process_site',
  parameter_map = map,
  path = dir,
)


# or launch models on cluster --------------------------------------------------
# site reports
obj$lapply(
  1:nrow(maps$site_map),
  run_report,
  report_name = 'set_parameters',
  parameter_map = maps$site_map,
  path = dir,
)

# country reports
obj$lapply(
  1:nrow(maps$country_map),
  run_report_country,
  report_name = 'launch_models',
  parameter_map = maps$country_map,
  path = dir,
)



# check if jobs have completed  ------------------------------------------------
metadata <- orderly2::orderly_metadata_extract(extract = c("name", "parameters", 'files'))



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

#remotes::install_github('mrc-ide/site_vimc')
lapply(list.files('functions/', full.names = T), source)

# obtain list of countries to run model for
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)

dir<- getwd()


# if you have not already, initialize the orderly repository
#orderly2::orderly_init(path = dir)
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


# PARAMETERS TO CHANGE FOR REPORTS ---------------------------------------------
maps<- make_parameter_maps(
  iso3cs = iso3cs,                                                              # Pick 10 countries to begin with
  #scenarios= c('malaria-rts3-rts4-default'),                    # if you only want to run reports for certain scenarios. Default is all 7
  population = 100000,                                                          # population size
  description = 'complete_run',                                                 # reason for model run (change this for every run if you do not want to overwrite outputs)
  parameter_draw = 0,                                                           # parameter draw to run (0 for central runs)
  burnin= 15,                                                                   # burn-in in years            
  quick_run = FALSE                                                             # boolean, T or F. If T, makes age groups larger and runs model through 2035.
)

reports <- c('set_parameters', 'launch_models', 'process_site', 'site_diagnostics', 'process_country', 'country_diagnostics')



# remove duplicate reports before launching
site_map<- remove_duplicate_reports(report_name = 'launch_models', parameter_map = maps$site_map)

# check that the preceding report has completed before you launch next report in chronology
site_map<- generate_parameter_map_for_next_report(report_name = 'set_parameters', parameter_map = site_map)

# # cluster setup ----------------------------------------------------------------
ctx <- context::context_save("contexts", sources= 'functions/run_report.R')
config <- didehpc::didehpc_config(
  use_rrq = FALSE,
  cores = 1,
  cluster = "fi--didemrchnb" ,#"fi--dideclusthn", # , "fi--didemrchnb""fi--didemrchnb"
  #template = "AllNodes",  ## use for the wpia cluster
  parallel = FALSE)

obj <- didehpc::queue_didehpc(ctx, config = config)

# # if you have not already, install packages:
# pkgs<- c('mrc-ide/orderly2@mrc-4724',
#          'mrc-ide/malariasimulation',
#          'mrc-ide/site_vimc',
#          'wesanderson',
#          'ggpubr',
#          'ggforce',
#          'data.table',
#          'dplyr')

# for (pkg in pkgs){
# 
#   obj$install_packages('mrc-ide/site_vimc')
# 
# }


# run report for all sites locally ---------------------------------------------
lapply(
    1:nrow(site_map),
    run_report,
    report_name = 'set_parameters',
    parameter_map = site_map,
    path = dir
  )


# or launch on cluster
models<- obj$lapply(
  1:250,
  run_report,
  report_name = 'launch_models',
  parameter_map = site_map,
  path = dir
)
  
# run report for all countries locally  ----------------------------------------
lapply(
  1:nrow(maps$country_map),
  run_report_country,
  report_name = 'country_diagnostics',
  parameter_map = maps$country_map,
  path = dir
)


# or launch cluster
jobs<- obj$lapply(
  1:nrow(maps$country_map),
  run_report_country,
  report_name = 'launch_models',
  parameter_map = maps$country_map,
  path = dir
)





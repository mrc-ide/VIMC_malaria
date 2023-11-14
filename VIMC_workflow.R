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

lapply(list.files('functions/', full.names = T), source)

# obtain list of countries to run model for
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)

dir<- getwd()
source('run_report.R')
soruce('remove_zero_eirs.R')
# PARAMETERS TO CHANGE FOR REPORTS ---------------------------------------------
iso3c<- 'MDG'                                                                   # country to launch model for
sites<- data.table(readRDS(paste0('src/process_inputs/site_files/', iso3c, '.rds'))$sites)  # sites for country of interest
population<- 50000                                                              # population size
description<- 'quick_run_other_countries'                                                    # reason for model run (change this for every run if you do not want to overwrite outputs)
draw<- 0                                                                        # parameter draw to run (0 for central runs)
burnin<- 15                                                                     # burn-in in years            
quick_run<- FALSE                                                               # boolean, T or F. If T, makes age groups larger and runs model through 2035.

# if just testing reports for one site:
site_name<- 'Sahel'
ur<- 'rural'

# don't launch models for sites with EIR of zero
sites<- remove_zero_eirs(iso3c, sites)

# reports to run (in chronological order)
# reports <-
#   c(
#     'set_parameters',
#     'launch_models',
#     'process_site',
#     'site_diagnostics',
#     'process_country',
#     'country_diagnostics'
#   )
# 
# # scenarios to run (in no order)
# scenarios <-
#   c(
#     'no-vaccination',
#     'malaria-r3-default',
#     'malaria-r3-r4-default',
#     'malaria-rts3-bluesky',
#     'malaria-rts3-default',
#     'malaria-rts3-rts4-bluesky'
#   )

################################################################################
# 1 prepare and save inputs
# unless inputs change, this only needs to be run once for all countries
for (iso3c in iso3cs){

  orderly2::orderly_run(
    'process_inputs',
    list(iso3c = iso3c),
    root = dir)
}


# run reports for all sites in a country locally -------------------------------
lapply(
  1:nrow(sites),
  run_report,
  report_name = 'set_parameters',
  path = dir,
  site_data = sites,
  population = population,
  description = description,
  scenario = 'no-vaccination',
  parameter_draw = draw,
  burnin = burnin,
  quick_run = quick_run
)


# ## run a single report locally      ---------------------------------------------
orderly2::orderly_run(
  'launch_models',
  list(
    iso3c = iso3c,
    site_name = site_name,
    ur= ur,
    description = description,
    population = population,
    parameter_draw = draw,
    burnin= burnin,
    scenario = 'no-vaccination',
    quick_run = quick_run),
  root = dir
)


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

 
# # run a single report on the cluster -------------------------------------------
# report_cluster<- obj$enqueue(orderly2::orderly_run(
#   'launch_models',
#   list(
#     iso3c = iso3c,
#     site_name = site_name,
#     ur= ur,
#     description = description,
#     population = population,
#     parameter_draw = draw,
#     burnin= burnin,
#     scenario = 'malaria-rts3-rts4-default',
#     quick_run = quick_run),
#   root = dir
# ))
# 
# 
# # run a group of reports on the cluster ----------------------------------------
reports_cluster_mdg_novax<- obj$lapply(
  1:nrow(sites),
  run_report,
  report_name = 'launch_models',
  path = dir,
  site_data = sites,
  population = population,
  description = description,
  scenario = 'no-vaccination',
  parameter_draw = draw,
  burnin = burnin,
  quick_run = quick_run
)

reports_cluster_mdg_vax<- obj$lapply(
  1:nrow(sites),
  run_report,
  report_name = 'launch_models',
  path = dir,
  site_data = sites,
  population = population,
  description = description,
  scenario = 'malaria-rts3-rts4-default',
  parameter_draw = draw,
  burnin = burnin,
  quick_run = quick_run
)

# # aggregate country outputs (after all sites in country have finished) ---------
orderly2::orderly_run(
  'process_country',
  list(
    iso3c = 'BFA',
    description = 'quick_run_other_countries',
    population = population,
    parameter_draw = draw,
    burnin= burnin,
    scenario = 'malaria-rts3-rts4-default',
    quick_run = TRUE),
  root = dir
)
# 
# # produce diagnostics at the country level (after processing outputs) ----------
orderly2::orderly_run(
  'country_diagnostics',
  list(
    iso3c = 'MDG',
    description = 'quick_run_other_countries',
    population = population,
    parameter_draw = draw,
    burnin= burnin,
    scenario = 'malaria-rts3-rts4-default',
    quick_run = TRUE),
  root = dir
)

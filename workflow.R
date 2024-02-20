################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################

# initialize orderly repository
source('workflow_functions.R')
library(data.table)
library(dplyr)

# process inputs for each country modelled  (must only be run once)  ----------
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# run analysis for each country + scenario + parameter set
map<- make_parameter_map(iso3cs= iso3cs,
                         #scenarios = c('no-vaccination'),
                          description = 'full_parameter_run',
                          parameter_draws = c(6:10),
                          quick_run= FALSE)
cores<- unique(map$site_number)


# check metadata for completed reports if useful  ------------------------------
# map<- check_reports_completed('process_country', map)
# map<- check_not_a_rerun('scale', map)
#
# # if you are running scale and plot, add a column for whether you want to run diagnostics
# map<- map |>
#   select(-parameter_draw) |>
#   filter(!scenario == 'no-vaccination')
# map<- unique(map)
#
#
# # launch one report locally --------------------------------------------------
# orderly2::orderly_run(name = "scale_and_plot", parameters = inputs[[1]])
#
# # launch many reports locally ------------------------------------------------
# for(index in c(1:nrow(map))){
#
#   message(index)
#   params<- as.list(map[index,])
#   orderly2::orderly_run(name = 'stochastic_plots', parameters = params)
# }



# cluster setup ------
hipercow::hipercow_init(driver = 'windows')
#hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')
hipercow::hipercow_configuration()


# submit groups of jobs by number of cores to submit  --------------------------
lapply(cores, submit_by_core, dt = map)

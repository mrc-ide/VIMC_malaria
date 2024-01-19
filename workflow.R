################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################

# initialize orderly repository
source('workflow_functions.R')
library(data.table)
library(dplyr)
orderly2::orderly_init()

# process inputs for each country modelled  (must only be run once)  ----------
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# for (iso3c in iso3cs){
#   
#   orderly2::orderly_run(
#     'process_inputs',
#     list(iso3c = iso3c),
#     root = dir)
# }

# run analysis for each country + scenario + parameter set
map<- make_parameter_map(iso3cs= 'SLE',
                         scenarios = c('malaria-rts3-rts4-default', 'no-vaccination'),
                          description = 'refactor_testing',
                          parameter_draws = c(2),
                          quick_run= T)

inputs<- purrr::map(.x = c(1:nrow(map)), .f= ~ map[.x,])

test<- lapply(inputs, run_report, report_name = 'scale_and_plot')



# cluster setup ------


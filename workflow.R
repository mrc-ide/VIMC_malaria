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
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-7_malaria-rts3-rts4-bluesky.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()
#run_process_inputs(iso3cs)

# run analysis for each country + scenario + parameter set
map<- make_parameter_map(iso3cs= iso3cs,
                         #scenarios = c('malaria-rts3-default', 'malaria-rts3-rts4-default'),
                          description = 'test_round2_changes',
                          parameter_draws = c(0:50),
                          quick_run= FALSE,
                          pfpr10 = TRUE)
map<- map |> select(-site_number)

#completed<- completed_reports('process_country')
map<- check_reports_completed('process_country', map)
map<- check_not_a_rerun('postprocess', map)
#run_process_inputs(iso3cs)


for(index in c(1:nrow(map))){


  message(index)
  params<- as.list(map[index,])
  orderly2::orderly_run(name = 'postprocess', parameters = params)

}


# launch one report locally
map<- map |>
  select(-site_number)
inputs<- purrr::map(.x = c(1:nrow(map)), .f= ~ as.list(map[.x,]))

orderly2::orderly_run(name = "process_country", parameters = inputs[[1]])

# cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')
hipercow::hipercow_configuration()

cores<- unique(map$site_number)
# submit groups of jobs by number of cores to submit  --------------------------
lapply(cores, submit_by_core, dt = map)

submit_postprocessing(map)

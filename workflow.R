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
map<- make_parameter_map(iso3cs= c('KEN', 'UGA'),
                         #scenarios = c('malaria-rts3-rts4-default', 'no-vaccination'),
                          description = 'quick_quick_run',
                          parameter_draws = c(2),
                          quick_run= TRUE)

inputs<- purrr::map(.x = c(1:nrow(map)), .f= ~ as.list(map[.x,]))


orderly2::orderly_run(name = "process_country", parameters = inputs[[11]])

# cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')

hipercow::hipercow_configuration()

id <- hipercow::task_create_expr(orderly2::orderly_run(name = "process_country", parameters = inputs[[1]]),
                                                       resources = hipercow::hipercow_resources(cores = 32))
hipercow::task_status(id)
hipercow::task_result(id)
hipercow::task_log_show(id)
hipercow::task_info(id)
hipercow::task_log_watch(id)
hipercow::task_wait(id)

id <- hipercow::task_create_expr(foo(2, 4))
hipercow::task_wait(id)
hipercow::task_result(id)


b <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run(
    "process_country",
    parameters = list(iso3c = iso3c,
                      description = description,
                      quick_run = quick_run,
                      scenario = scenario,
                      parameter_draw = parameter_draw)),
  map,
  resources = hipercow::hipercow_resources(cores = 32))



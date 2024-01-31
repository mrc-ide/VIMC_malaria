################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# initialize orderly repository
source('workflow_functions.R')
library(data.table)
library(dplyr)
#orderly2::orderly_init()

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

# run analysis for each country + s.cenario + parameter set
map<- make_parameter_map(iso3cs= iso3cs,
                         #scenarios = c('malaria-r3-default', 'malaria-r3-r4-default'),
                          description = 'full_parameter_run',
                          parameter_draws = c(0),
                          quick_run= FALSE)

completed<- completed_reports('process_country')
map<- check_reports_completed('process_country', map)
map<- check_not_a_rerun('scale_and_plot', map)
inputs<- purrr::map(.x = c(1:nrow(map)), .f= ~ as.list(map[.x,]))

# if you are running scale and plot, add a column for whether you want to run diagnostics
map[, plot:= TRUE]


# launch many reports locally

for(index in c(1:nrow(map))){

  message(index)
  params<- as.list(map[index,])
  orderly2::orderly_run(name = 'scale_and_plot', parameters = params)

}


# launch one report locally
orderly2::orderly_run(name = "scale_and_plot", parameters = inputs[[1]])



# cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')
hipercow::hipercow_configuration()

id <- hipercow::task_create_expr(orderly2::orderly_run(name = "process_country", parameters = inputs[[1]]))
hipercow::task_status(id)
hipercow::task_result(id)
hipercow::task_log_show(id)
hipercow::task_info(id)
hipercow::task_log_watch(id)
hipercow::task_wait(id)

id <- hipercow::task_create_expr(foo(2, 4))
hipercow::task_wait(id)
hipercow::task_result(id)

site_counts<- rbindlist(lapply(iso3cs, pull_site_numbers))

subset<- map
sub<- merge(subset, site_counts, by = 'iso3c')

sub1<- sub[site_number < 16]
sub2<- sub[site_number >= 16]

bsmall3 <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run(
    "process_country",
    parameters = list(iso3c = iso3c,
                      description = description,
                      quick_run = quick_run,
                      scenario = scenario,
                      parameter_draw = parameter_draw)),
  sub1,
  resources = hipercow::hipercow_resources(cores = 16))


bbig3 <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run(
    "process_country",
    parameters = list(iso3c = iso3c,
                      description = description,
                      quick_run = quick_run,
                      scenario = scenario,
                      parameter_draw = parameter_draw)),
  sub2,
  resources = hipercow::hipercow_resources(cores = 32))

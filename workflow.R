################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# initialize orderly repository
source('workflow_functions.R')
source('postprocess_function.R')
library(data.table)
library(dplyr)

# process inputs for each country modelled  (must only be run once)  ----------
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-7_malaria-rts3-rts4-bluesky.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()
#run_process_inputs(iso3cs)

# # run analysis for each country + scenario + parameter set
map<- make_parameter_map(iso3cs= iso3cs,
                         scenarios = c('malaria-rts3-bluesky', 'malaria-rts3-rts4-bluesky'),
                          description = 'test_round2_changes',
                          parameter_draws = c(101:200),
                          quick_run= FALSE,
                          pfpr10 = TRUE)

# map<- map |> select(-site_number)
completed<- completed_reports('process_country')
map<- check_reports_completed('process_country', map, date_time = 0)
map<- check_not_a_rerun('process_country', map, date_time = 0)


site_counts<- unique(rbindlist(lapply(c(1:nrow(map)), pull_site_numbers, map = map)))
map<- merge(map, site_counts, by = c('iso3c', 'scenario'))

# run_local_reports(map, 'postprocess') # if you would like to run postprocessing or diagnostics locally
#
# ctx <- context::context_save("ctxs", sources= 'postprocess_function.R')
# config <- didehpc::didehpc_config(
#   use_rrq = FALSE,
#   cores = 1,
#   cluster = "fi--didemrchnb") #"fi--dideclusthn", # , "fi--didemrchnb""fi--didemrchnb"
# #template = "AllNodes")
#
#
# obj <- didehpc::queue_didehpc(ctx, config = config)
# # launch one report locally
# map<- map |>
#   select(-site_number)
# inputs<- purrr::map(.x = c(1:nrow(map)), .f= ~ as.list(map[.x,]))
#
# orderly2::orderly_run(name = "process_country", parameters = inputs[[1]])
#
# # cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')
hipercow::hipercow_configuration()

cores<- unique(map$site_number)
# # # submit groups of jobs by number of cores to submit  --------------------------
lapply(cores, submit_by_core, dt = map)
# #
# # submit_postprocessing(map)
# #
# #
for(iso in iso3cs){
  tasks<- hipercow::task_create_expr(orderly2::orderly_run('diagnostics', parameters = list(iso3c = iso,
                                                                                    description = 'test_round2_changes')))

}
# #

for(iso in iso3cs){
orderly2::orderly_run('diagnostics', parameters = list(iso3c = iso, description = 'test_round2_changes'))

}
hipercow::task_log_watch(task)
# obj$lapply(iso3cs, run_postprocessing)
# #
# #
# # test<- obj$enqueue(run_postprocessing('TCD'))


obj$lapply(iso3cs, run_postprocessing)

obj$enqueue(run_postprocessing('MDG'))
run_postprocessing('MDG')

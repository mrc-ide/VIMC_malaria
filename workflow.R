################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# setup ------------------------------------------------------------------------
remotes::install_github('mrc-ide/vimcmalaria@paper')
#remotes::install_github('mrc-ide/site@paper')

#orderly2::orderly_init()
library(data.table)
library(dplyr)
library(hipercow)
library(vimcmalaria)
library(tidyr)

source('functions.R')
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# # generate parameter map for analysis ------------------------------------------
map<- make_parameter_map(iso3cs= c(iso3cs),
                         scenarios = c('no-vaccination', 'proxy'),
                          description = 'number_plug_paper',
                          parameter_draws = c(26:50),
                          quick_run= FALSE)

# check function
map<- check_not_rerun(report_name = 'process_country', map = map, date = 0)
map<- check_reports_completed('process_country', map, date_time = 0)
reports<- completed_reports('process_country')

# # STEP 1: run process_inputs report --------------------------------------------
#lapply(iso3cs, function(x) orderly2::orderly_run('process_inputs', parameters = list(iso3c = x)))

# # STEP 2: run process_country for all countries (on cluster)  ------------------
run_local_reports(map, 'process_country') # if needed, test a report locally before full launch

# # cluster setup ------
hipercow::hipercow_init(driver = 'dide-windows')
hipercow::hipercow_configure(driver= 'dide-windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create()
hipercow::hipercow_configuration()

# # submit groups of jobs by number of cores to submit  ------------------------
lapply(unique(map$site_number), submit_by_core, dt = map, test = FALSE)

# STEP 3: run postprocessing on outputs   --------------------------------------
for(iso in iso3cs){

message(iso)

task<- hipercow::task_create_expr(
orderly2::orderly_run(
    "postprocessing",
    parameters = list(
      iso3c = iso,
      description = 'number_plug_paper',
      quick_run = FALSE
    ))
)
}


# # STEP 4: run diagnostic reports for outputs  ----------------------------------
for(iso in iso3cs){

  message(iso)
task<- hipercow::task_create_expr(
  orderly2::orderly_run(
    "diagnostics",
    parameters = list(
      iso3c = iso,
      description = 'number_plug_paper',
      quick_run = FALSE
    ))
)
}

# compile diagnostic outputs to shared filepath
compile_diagnostics(descrip = 'gavi_reruns_2025', date_time = 0)

# compile files for submisxion
final_output<- compile_final_outputs('gavi_reruns_2025')
final_output<- final_output |>
  select(-run_id, -pfpr_threshold)

for(scen in unique(final_output$scenario)){

  message(scen)

  final<- final_output |>
    filter(scenario == scen) |>
    select(-scenario)

  final<- unique(final)
  write.csv(final, paste0('montagu/central-burden-est-', scen, '.csv'), row.names= F)

}

site_output<- lapply(c(1:nrow(completed)), get_site_outputs, map = completed, output_filepath = 'archive/process_country/')







# compile final outputs (for a set of stochastic runs)
task<- hipercow::task_create_expr(vimcmalaria::compile_and_save('booster_update', 'no-vaccination'))
task<- hipercow::task_create_expr(vimcmalaria::compile_and_save('booster_update', 'malaria-r3-r4-default'))
task<- hipercow::task_create_expr(vimcmalaria::compile_and_save('booster_update', 'malaria-rts3-rts4-default'))




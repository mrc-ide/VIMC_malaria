################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# setup ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(hipercow)
library(vimcmalaria)

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-7_malaria-rts3-rts4-bluesky.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# generate parameter map for analysis ------------------------------------------
map<- make_parameter_map(iso3cs= iso3cs,
                         #scenarios = c('malaria-rts3-bluesky', 'malaria-rts3-rts4-bluesky'),
                          description = 'round3',
                          parameter_draws = c(0),
                          quick_run= TRUE)
map<- check_not_a_rerun('process_country', map, date_time = 0)


# STEP 1: run process_inputs report --------------------------------------------
lapply(iso3cs, function(x) orderly2::orderly_run('process_inputs', parameters = list(iso3c = x)))


# STEP 2: run process_country for all countries (on cluster)  ------------------
# if needed, test a report locally before full launch
run_local_reports(map, 'process_country')

# cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources= 'workflow_functions.R')
hipercow::hipercow_configuration()


# submit groups of jobs by number of cores to submit  --------------------------
lapply(unique(map$site_number), submit_by_core, dt = map)


# STEP 3: run postprocessing on outputs   --------------------------------------
for(iso in iso3cs){
  hipercow::task_create_bulk_expr(orderly2::orderly_run(
    "postprocess_bulk",
    parameters = list(
      iso3c = iso,
      description = description,
      quick_run = quick_run,
      scenario = scenario
    ),
    map
  ))
}

# STEP 4: run diagnostic reports for outputs  ----------------------------------
for(iso in iso3cs){
  tasks <-
    hipercow::task_create_expr(orderly2::orderly_run(
      'diagnostics',
      parameters = list(iso3c = iso,
                        description = 'test_round2_changes')
    ))

}



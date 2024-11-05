################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# setup ------------------------------------------------------------------------
#orderly2::orderly_init()
library(data.table)
library(dplyr)
library(hipercow)
library(vimcmalaria)

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# # generate parameter map for analysis ------------------------------------------
map<- make_parameter_map(iso3cs=iso3cs,
                         scenarios = c('no-vaccination', 'malaria-rts3-rts4-default', 'malaria-r3-r4-default'),
                          description = 'fix_booster_coverage',
                          parameter_draws = c(101:200),
                          quick_run= FALSE)
test<- check_not_a_rerun('process_country', map, date_time = 0)

#map<- check_reports_completed('process_country', map, date_time = 0)
# # STEP 1: run process_inputs report --------------------------------------------
#lapply(iso3cs, function(x) orderly2::orderly_run('process_inputs', parameters = list(iso3c = x)))
reports<- vimcmalaria::completed_reports('postprocessing') |> filter(description == 'fix_booster_coverage')
#
# unique(reports)
# # STEP 2: run process_country for all countries (on cluster)  ------------------
# # if needed, test a report locally before full launch
run_local_reports(map, 'process_country')

# # cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create()
hipercow::hipercow_configuration()

#hipercow::task_log_watch(task)

# # submit groups of jobs by number of cores to submit  ------------------------
number_order<- c(32, 30, 2, 28, 4, 24, 8, 23, 9, 20, 12, 18, 13, 17, 15, 16,7,3, 1, 5, 6, 7, 10, 11, 14) # intersperse the tasks so the cluster is at optimal usage
lapply(unique(number_order), submit_by_core, dt = map, test = FALSE)

for(iso in iso3cs)
task<- hipercow::task_create_expr(
  orderly2::orderly_run(
  "process_country",
  parameters = list(iso3c = 'GHA',
                    description = 'fix_booster_coverage',
                    quick_run = FALSE,
                    scenario = 'malaria-rts3-rts4-default',
                    parameter_draw = 0))
  )
# # launch ethiopia calibrations and save somewhere central ----------------------
# STEP 3: run postprocessing on outputs   --------------------------------------
for(iso in iso3cs){

message(iso)

#task<- hipercow::task_create_expr(
orderly2::orderly_run(
    "postprocessing",
    parameters = list(
      iso3c = iso,
      description = 'fix_booster_coverage',
      quick_run = FALSE
    ))
#)
}


# # STEP 4: run diagnostic reports for outputs  ----------------------------------
for(iso in iso3cs){

  message(iso)
task<- hipercow::task_create_expr(
  orderly2::orderly_run(
    "diagnostics",
    parameters = list(
      iso3c = iso,
      description = 'fix_booster_coverage',
      quick_run = FALSE
    ))
)
}

# compile outputs to shared filepath
compile_diagnostics(descrip = 'models_for_paper', date_time = 20240710001352)

files<- list.files('montagu/', full.names = TRUE)
files<- files[files %like% 'r3']

for(scen in c('malaria-r3-bluesky', 'malaria-r3-default', 'malaria-r3-r4-bluesky', 'malaria-r3-r4-default')){

  full<- read.csv(paste0('montagu/central-burden-est-', scen, '.csv'))
  message(nrow(full))
  full<- full |>
    filter(country != 'ETH')



  eth<- compile_final_outputs('fix_rtss_booster') |>
    filter(country== 'ETH') |>
    filter(scenario == scen) |>
    select(-run_id) |>
    select(-scenario)


  full<- rbind(full, eth)#

  message(nrow(full))

  write.csv(full, paste0('montagu/update/central-burden-est-', scen, '.csv'), row.names = FALSE)

}





total<- eth |> select(-run_id)
for(scen in unique(total$scenario)){

  subset<- total |> filter(scenario == scen)
  subset<- subset |> select(-scenario)

  write.csv(subset, paste0('montagu/central-burden-est-', scen, '.csv'), row.names = FALSE)

}



completed<- completed_reports('process_country')
completed<- completed |>
  filter(description == 'stochastic_fix',
         scenario == 'no-vaccination')


site_output<- lapply(c(1:nrow(completed)), get_site_outputs, map = completed, output_filepath = 'archive/process_country/')



#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_site_outputs<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  iso3c<- metadata$iso3c

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))                  # get output file
  sites<- data.table::rbindlist(lapply(output$site_output, function(x) return(x$processed_output))) #pull out processed site_level output


  saveRDS(sites, paste0('J:/malaria_no_more/test_files/', iso3c, '_site_output.rds'))

  return(sites)
}


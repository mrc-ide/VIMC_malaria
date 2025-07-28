################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# setup ------------------------------------------------------------------------
remotes::install_github('mrc-ide/vimcmalaria@paper')
remotes::install_github('mrc-ide/site@paper')

#orderly2::orderly_init()
library(data.table)
library(dplyr)
library(hipercow)
library(vimcmalaria)



coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# # generate parameter map for analysis ------------------------------------------
map<- make_parameter_map(iso3cs= c('KEN'),
                         scenarios = c('no-vaccination'),
                          description = 'number_plug_paper',
                          parameter_draws = c(0:5),
                          quick_run= FALSE)

#map$scenario<- 'proxy'
# check function
test<- check_not_a_rerun(report_name = 'process_country', map = map, date = 0)
map<- check_reports_completed('process_country', map, date_time = 0)
# # STEP 1: run process_inputs report --------------------------------------------
#lapply(iso3cs, function(x) orderly2::orderly_run('process_inputs', parameters = list(iso3c = x)))
#reports<- vimcmalaria::completed_reports('process_country') |> filter(description == 'booster_update')
#
# unique(reports)
# # STEP 2: run process_country for all countries (on cluster)  ------------------
# # if needed, test a report locally before full launch
run_local_reports(map, 'process_country')

completed<- completed_reports('process_country')
# # cluster setup ------
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create()
hipercow::hipercow_configuration()

#hipercow::task_log_watch(task)
# # submit groups of jobs by number of cores to submit  ------------------------
#number_order<- c(32, 30, 2, 28, 4, 24, 8, 23, 9, 20, 12, 18, 13, 17, 15, 16,7,3, 1, 5, 6, 7, 10, 11, 14) # intersperse the tasks so the cluster is at optimal usage
lapply(unique(map$site_number), submit_by_core, dt = map, test = FALSE)


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
for(iso in iso3cs[31]){

message(iso)

#task<- hipercow::task_create_expr(
orderly2::orderly_run(
    "postprocessing",
    parameters = list(
      iso3c = iso,
      description = 'number_plug_paper',
      quick_run = FALSE
    ))
#)
}


# # STEP 4: run diagnostic reports for outputs  ----------------------------------
for(iso in iso3cs[8:31]){

  message(iso)
#task<- hipercow::task_create_expr(
  orderly2::orderly_run(
    "diagnostics",
    parameters = list(
      iso3c = iso,
      description = 'gavi_reruns_2025',
      quick_run = FALSE
    ))
#)
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



test<- output |>
  group_by(year, scenario)|>
  summarise(cases= sum(cases),
            deaths= sum(deaths),
            dalys= sum(dalys),
          .groups = 'keep')  
  #filter(site_name == 'Gambela Peoples')

ggplot()+
  geom_line(data = test, mapping = aes(x= year, y= cases, color= scenario))  +
  #facet_wrap(~site_name, scales= 'free')+
  #geom_line(data= site_data$cases_deaths, mapping = aes(x= year, y= wmr_deaths), color= 'darkgreen')+
  labs(x= 'Time (in years)', y= 'Cases',
       title= paste0('Cases over time, baseline scenario'),
       color= 'Scenario', fill= 'Scenario') +
        geom_vline(xintercept = 2021, linetype = "dotted") +
          geom_vline(xintercept = 2023, linetype = "dotted") +      
  scale_color_manual(values= wes_palette('Darjeeling1', n= 3)) +
  scale_fill_manual(values= wes_palette('Darjeeling1', n= 3)) 


ggplot()+
  geom_line(data = test, mapping = aes(x= year, y= deaths, color= scenario))  +
  #geom_line(data= site_data$cases_deaths, mapping = aes(x= year, y= wmr_cases), color= 'darkgreen')+
  labs(x= 'Time (in years)', y= 'deaths',
       title= paste0('deaths over time'),
       color= 'Scenario', fill= 'Scenario') +
        geom_vline(xintercept = 2021, linetype = "dotted") +
          geom_vline(xintercept = 2023, linetype = "dotted") +      
  scale_color_manual(values= wes_palette('Darjeeling1', n= 3)) +
  scale_fill_manual(values= wes_palette('Darjeeling1', n= 3)) 
  
ggplot()+
  geom_line(data = test, mapping = aes(x= year, y= dalys, color= scenario))  +
  #geom_line(data= site_data$cases_deaths, mapping = aes(x= year, y= wmr_cases), color= 'darkgreen')+
  labs(x= 'Time (in years)', y= 'DALYs',
       title= paste0('DALYs over time'),
       color= 'Scenario', fill= 'Scenario') +
        geom_vline(xintercept = 2021, linetype = "dotted") +
          geom_vline(xintercept = 2023, linetype = "dotted") +      
  scale_color_manual(values= wes_palette('Darjeeling1', n= 3)) +
  scale_fill_manual(values= wes_palette('Darjeeling1', n= 3)) 

  
  
  bl_input<- test |> 
    filter(scenario == 'no-vaccination') |> 
    rename(cases_bl = cases,
           deaths_bl = deaths, 
           dalys_bl = dalys) |>
    select(-scenario)

  
  intvn_input<- test |> filter(scenario!= 'no-vaccination')


  merged<- merge(intvn_input, bl_input, by = c('year')) |>
    mutate(cases_averted = cases_bl - cases,
            deaths_averted = deaths_bl - deaths,
            dalys_averted = dalys_bl - dalys)
  
  
  
    
ggplot()+
  geom_line(data = merged, mapping = aes(x= year, y= cases_averted))  +
  #geom_line(data= site_data$cases_deaths, mapping = aes(x= year, y= wmr_cases), color= 'darkgreen')+
  labs(x= 'Time (in years)', y= 'DALYs',
       title= paste0('DALYs over time'),
       color= 'Scenario', fill= 'Scenario') +
        geom_vline(xintercept = 2021, linetype = "dotted") +
          geom_vline(xintercept = 2023, linetype = "dotted") +      
  scale_color_manual(values= wes_palette('Darjeeling1', n= 3)) +
  scale_fill_manual(values= wes_palette('Darjeeling1', n= 3)) 
  
################################################################################
## title    VIMC workflow
## author   Lydia Haile
## purpose  run models for VIMC
################################################################################


# setup ------------------------------------------------------------------------
remotes::install_github('mrc-ide/vimcmalaria@paper')
#remotes::install_github('mrc-ide/site@paper')
#remotes::install_github('mrc-ide/postie@928cf585a3a995dc16da0a99b1c1e3a7c51d9cae')
#remotes::install_github('mrc-ide/scene@4bc0d154cb63bd8730b9c17d7b8742e9d05349a5')
#orderly2::orderly_init()
library(data.table)
library(dplyr)
library(hipercow)
library(vimcmalaria)
library(tidyr)
source('functions.R')
library(ltc)

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# # generate parameter map for analysis ------------------------------------------
map<- make_parameter_map(iso3cs= c('COD'),
                         scenarios = c('proxy', 'no-vaccination'),
                          description = 'reduced NVMIs',
                          parameter_draws = c(0),
                          quick_run= TRUE)

# check function
map<- check_not_rerun(report_name = 'process_country', map = map, date = 0)
map<- check_reports_completed('process_country', map, date_time = 0)
reports<- completed_reports('process_country') |> filter(iso3c == 'GNB', description== 'number_plug_paper')

map$site_number<- 3
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
      iso3c = 'GNB',
      description = 'number_plug_paper',
      quick_run = FALSE
    ))
)
}


# # STEP 4: run diagnostic reports for outputs  ----------------------------------
for(iso in iso3cs){

  message(iso)
#task<- hipercow::task_create_expr(
  orderly::orderly_run(
    "postprocessing",
    parameters = list(
      iso3c = 'COD',
      description = 'constant coverage',
      quick_run = FALSE
    ))

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




  completed<- completed_reports('process_country')

  descrip<- 'new_site_file_test'

clean_reports<- function(descrip){

  com<- completed |> filter(description== descrip)

  #for(i in c(1:nrow(com))){

    message(i)
    subset<- com[i,]
    dir_name<- subset$directory_name
    print(dir_name)
    filepath<- paste0('archive/process_country/', dir_name, '/')
    print(unlink(filepath, recursive = TRUE))
  }

#}

clean_reports('new_site_file_test')


  com<- completed |> filter(description== 'number_plug_paper')

#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_final_outputs<- function(descrip, quick){

  completed<- completed_reports('postprocessing')
  completed<- completed[description == {{descrip}} & quick_run == {{quick}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, quick_run, .keep_all = TRUE) |> # this is not working because it's pulling both quick run outputs (true or false)
    dplyr::arrange(iso3c, description, quick_run)

  pull_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- data.table::rbindlist(readRDS(paste0('archive/postprocessing/', directory_name, '/final_output.rds')))
    return(output)
  }

  outputs<- data.table::rbindlist(lapply(c(1:nrow(completed)), pull_output, map = completed))
}


## compile final outptuts
nvmis<- compile_final_outputs('reduced NVMIs', quick= FALSE) |>
  mutate(description= 'NVMIs', quick= FALSE)

constant_coverage<- compile_final_outputs('constant coverage', quick= FALSE) |>
  mutate(description= 'constant coverage')

default_settings<- compile_final_outputs('number_plug_paper', quick= FALSE) |>
  mutate(description= 'default') |>
  filter(country== 'COD')


# read the unscaled outputs
fixed<-copy(default_settings) |> 
  mutate(description = 'fixed') |>
 # rename(run_id = parameter_draw) |>
  mutate(scenario = ifelse(scenario == 'proxy', 'Routine', scenario)) |>
  data.table()

# fix the population from 2025 onwards for the fixed outputs
total_fixed<- fixed |>
  mutate(clinical = cases/cohort_size,
  mort= deaths/cohort_size)

# pull age/sex/country specific cohort size to merge onto original dataset and recalculate rates with
initial_demo<- fixed |> 
  filter(year== 2025, run_id == 0) |>
  select(age, country, country_name, cohort_size) |>
  unique() |>
  rename(cohort_size_2025 = cohort_size) # duplicates based on parameter_id 

tot<- merge(total_fixed, 
  initial_demo, 
  by = c('age', 'country', 'country_name'), 
  allow.cartesian = T)

tot<- tot |>
  mutate(cohort_size = if_else(year > 2025, cohort_size_2025, cohort_size)) |>
  mutate(cases = clinical* cohort_size,
  deaths= mort * cohort_size) |>
  select(-clinical, -mort) 



# pull in all of the model outputs by description
full_dt<- bind_rows(tot, nvmis, constant_coverage, default_settings)
unique(full_dt$description) # dynamic is baseline output



comparison_dt <- full_dt |>
  mutate(scenario= ifelse(scenario== 'Routine', 'proxy', scenario)) |>
  select(-pfpr_threshold, -cohort_size_2025) |>
  mutate(scenario = case_when(
    scenario == "proxy"        ~ "intvn",
    scenario == "no-vaccination" ~ "bl",
    TRUE ~ scenario
  )) |>
  filter(scenario %in% c("bl", "intvn")) |>
  pivot_wider(
    names_from = scenario,
    values_from = c(cases, deaths, dalys, yll)
  ) |>
  mutate(
    cases_averted  = cases_bl  - cases_intvn,
    deaths_averted = deaths_bl - deaths_intvn,
    dalys_averted= dalys_bl -dalys_intvn,
    ylls_averted= yll_bl- yll_intvn
  ) |>
  group_by(description, year, run_id) |>
  filter(run_id== 0) |>
  summarise(cases_intvn= sum(cases_intvn),
            cases_averted= sum(cases_averted),
            deaths_averted= sum(deaths_averted),
            dalys_averted= sum(dalys_averted),
          .groups= 'keep') |>
  mutate(description= ifelse(description == 'default', 'default \nparameters', description)) |>
  mutate(description= ifelse(description == 'fixed', 'fixed \ndemography', description)) |>
  mutate(description= ifelse(description == 'constant coverage', '100% constant \nvaccine coverage', description)) |>
  mutate(description= ifelse(description == 'NVMIs', 'reduced \nITNs and IRS \nby 30%', description)) 



p1<- ggplot(comparison_dt,aes(x = year, y = cases_averted, color = description, fill = description)) +
  geom_line(data = filter(comparison_dt, year <= 2024)) +
  geom_smooth(data = filter(comparison_dt, year >= 2024-2), span= 0.3) +
  scale_color_manual(values= ltc('casa_natal', 4, 'discrete')) +
  scale_fill_manual(values= ltc('casa_natal', 4, 'discrete')) +
ggthemes::theme_clean()  +
  xlim(2024, 2100) +
  theme(legend.position= 'left')+
  labs(title= ('Cases averted by parameterization, mean draw, smoothed'))



ltc::palettes

p2<- ggplot(comparison_dt,aes(x = year, y = cases_averted, color = description, fill = description)) +
  geom_line() +
  scale_color_manual(values= ltc('casa_natal', 4, 'discrete')) +
  #scale_fill_manual(values= ltc('remains', 4, 'discrete')) +
ggthemes::theme_clean()  +
  xlim(2024, 2100) +
  theme(legend.position= 'right')+
  labs(title= ('Cases averted by parameterization, mean draw, unsmoothed'))

library(ggpubr)
ggarrange(p1, p2, nrow= 2)


# plot cases of intvn scenario for both



p3<- ggplot(comparison_dt,aes(x = year, y = cases_intvn, color = description, fill = description)) +
  geom_line() +
  scale_color_manual(values= ltc('remains', 4, 'discrete')) +
  #scale_fill_manual(values= ltc('remains', 4, 'discrete')) +
ggthemes::theme_clean()  +
  xlim(2024, 2100) +
  theme(legend.position= 'bottom')+
  labs(title= ('Cases (intvn scenario) by parameterization, mean draw, unsmoothed'))


# restructure the sensitivity analyses

###########################################################################
## title   1_prepare_inputs
## author  Lydia Haile
## purpose pull final outputs for vaccine impact paper and save to folder 
##         for number plugging, tables, and figures
###########################################################################

source('J:/VIMC/VIMC_malaria/analyses/paper/source_code/paper_functions.R')
iso3cs<-read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv') |> pull(country_code) |> unique()

# pull country level output -------------- --------------------------------------
completed <- completed_reports("process_country") |>
  dplyr::filter(
    description == "number_plug_paper",
    quick_run == FALSE
  ) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, .keep_all = TRUE) |>
  dplyr::arrange(iso3c, scenario, parameter_draw)


default<- compile_final_outputs('number_plug_paper') |>
  #filter(country == 'COD') |>
  data.table() |>
  mutate(scenario = ifelse(scenario == 'proxy', 'Routine', scenario))

total<- copy(default) # VIMC output unsummarised
total<- unique(total)

# calculate cases averted (all ages)-------------------------------------------------------------------------------------
  novax<- total |> filter(scenario == 'no-vaccination') |>
    rename(cases_novax = cases,
           deaths_novax = deaths,
           dalys_novax = dalys) |>
    select(year, age, run_id, country, cases_novax, deaths_novax, dalys_novax)

  vax<- total |> filter(scenario == 'Routine')
  vax<- merge(vax, novax, by = c('country', 'age', 'year', 'run_id'))
  vax<- vax |>
    filter(run_id < 51) |>
    #filter(year %in% c(2023:2050)) |>
    #filter(year<= 2050) |>
    mutate(cases_averted = cases_novax- cases,
           deaths_averted = deaths_novax- deaths,
           dalys_averted = dalys_novax - dalys)

vax<- unique(vax)
saveRDS(vax, 'analyses/paper/inputs/vax.rds')


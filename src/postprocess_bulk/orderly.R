# postprocess in bulk
output_filepath<- 'J:/VIMC_malaria/archive/process_country/'
source('postprocessing_functions.R')
source('diagnostic_report_functions.R')


# packages
library(site)
library(data.table)
library(dplyr)
library(malariasimulation)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(tibble)
library(postie)
library(data.table)
library(countrycode)
library(scene)
library(scales)
library(wesanderson)

run_postprocessing<- function(iso3c){
  # pull metadata for completed outputs for this country
  completed<- pull_most_recent_output({{iso3c}})

  max_draw<- max(completed[scenario == 'no-vaccination', parameter_draw])

  # vimc inputs ----
  inputs<- completed_reports('process_inputs') |>
    filter(iso3c == {{iso3c}}) |>
    arrange(desc(date_time))
  inputs<- unique(inputs, by = 'iso3c')

  vimc_input<- readRDS(paste0('J:/VIMC_malaria/archive/process_inputs/', inputs$directory_name, '/vimc_input.rds'))
  site_data<- readRDS(paste0('J:/VIMC_malaria/archive/process_inputs/', inputs$directory_name, '/merged_site_file.rds'))

  coverage_data<- vimc_input$coverage_input
  le <- vimc_input$le
  vimc_pop<- vimc_input$population_input_all_age
  pop_single_yr<- vimc_input$population_input_single_yr
  pop_data<- vimc_input$population_input_all_age

  # pull model outputs for scaling to WMR cases: no-vaccination draw 0
  scaling_filepath<- completed |> filter(parameter_draw == 0,
                                         scenario == 'no-vaccination')

  message('reading in absolute filepath')
  scaling<- readRDS(paste0(output_filepath, scaling_filepath$directory_name, '/outputs.rds'))
  scaling<- scaling$country_output

  # pull model outputs for all scenarios
  intvn_output<- rbindlist(lapply(c(1:nrow(completed)), get_site_output, map = completed))
  dose_output<- rbindlist(lapply(c(1:nrow(completed)), get_dose_output, map = completed), fill = TRUE)

  doses<- dose_output |>
    group_by(scenario, year, parameter_draw) |>
    summarise(doses = sum(doses),
              .groups = 'keep') |>
    filter(year %in% c(2000:2100))

  saveRDS(doses, paste0('J:/VIMC_malaria/outputs/dose_output/', iso3c, 'dose_output.rds'))

  # pull model outputs for all baseline scenarios (as a separate input into intervention processing)
  bl_filepaths<- completed |> filter(scenario == 'no-vaccination')
  bl_output<- rbindlist(lapply(c(1:nrow(bl_filepaths)), get_site_output, map = bl_filepaths))

  # run postprocessing by parameter draw
  for (draw in c(1:max_draw)){

    message(paste0('postprocessing draw ', draw))

    bl<- bl_output |> filter(parameter_draw == draw)
    intvn<- intvn_output |> filter(parameter_draw == draw)

    message('adding low transmission sites')

    low_transmission<- pull_low_transmission_sites(iso3c, site_data, bl)
    intvn<- append_low_transmission_sites(low_transmission, intvn)

    message('aggregating')
    dt<- aggregate_outputs(intvn, pop_single_yr)

    message('scaling cases')
    output<- add_proportions(dt)

    output<- scale_cases(output, scaling_data= scaling,  site_data = site_data)

    # scale cases based on difference between site file PAR and VIMC PAR
    message('scaling PAR')
    processed_output<- scale_par(output, iso3c= {{iso3c}})

    message('formatting')
    # format and save (
    processed_output<- processed_output |>
      mutate(cases = round(cases),
             deaths = round(deaths),
             yll= round(ylls),
             dalys = round(dalys),
             cohort_size = round(cohort_size)) |>
      rename(run_id = parameter_draw) |>
      select(-model_proportion_risk,
             -proportion_risk,
             -scaling_ratio,
             -prop_severe,
             -prop_deaths,
             -dalys_pp,
             -ylls,
             -life_expectancy) |>
      select(run_id,
             disease,
             year,
             age,
             country,
             country_name,
             cohort_size,
             cases,
             dalys,
             deaths,
             yll)

    final_output<- rbind(final_output, processed_output, fill =T)

    saveRDS(final_output, paste0('J:/VIMC_malaria/outputs/stochastic_estimates/', iso3c, '_draw_', draw, '.rds'))
    message(paste0('done with draw ', draw))

    message('done')
  }

}




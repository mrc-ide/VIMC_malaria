# postprocess in bulk
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             quick_run = NULL)


# packages  --------------------------------------------------------------------
library(site)
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
library(vimcmalaria)

# read in dependencies  --------------------------------------------------------
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))

vimc_input<- readRDS('vimc_input.rds')
site_data <- readRDS('site_file.rds')

coverage_data<- vimc_input$coverage_input
le <- vimc_input$le
vimc_pop<- vimc_input$population_input_all_age
pop_single_yr<- vimc_input$population_input_single_yr
pop_data<- vimc_input$population_input_all_age

# pull metadata for completed outputs for this country
completed<- pull_most_recent_output({{iso3c}}, description = {{description}}, quick_run = {{quick_run}})
max_draw<- max(completed[scenario == 'no-vaccination', parameter_draw])

# vimc inputs
inputs<- completed_reports('process_inputs') |>
  filter(iso3c == {{iso3c}}) |>
  arrange(desc(date_time))
inputs<- unique(inputs, by = 'iso3c')

# pull model outputs for scaling to WMR cases: no-vaccination draw 0
scaling_filepath<- completed |> filter(parameter_draw == 0,
                                       scenario == 'no-vaccination',
                                       gfa == FALSE)
scaling<- readRDS(paste0('J:/VIMC_malaria/archive/process_country/', scaling_filepath$directory_name, '/outputs.rds'))
scaling<- scaling$country_output


# postprocess by draw ----------------------------------------------------------
final_postprocessing<- function(draw){

  message(paste0('postprocessing draw ', draw))

  bl_filepaths<- completed |> filter(parameter_draw == draw,
                                     scenario == 'no-vaccination')
  intvn_filepaths<- completed |> filter(parameter_draw == draw)

  # pull model outputs for all scenarios
  intvn<- rbindlist(lapply(c(1:nrow(intvn_filepaths)), get_site_output, map = intvn_filepaths, output_filepath = 'J:/VIMC_malaria/archive/process_country/' ))
  intvn[gfa == TRUE, scenario := paste0(scenario, '_gfa')]
  
  # pull model outputs for all baseline scenarios (as a separate input into intervention processing)
  bl<- rbindlist(lapply(c(1:nrow(bl_filepaths)), get_site_output, map = bl_filepaths, output_filepath = 'J:/VIMC_malaria/archive/process_country/'))
  bl[gfa == TRUE, scenario := paste0(scenario, '_gfa')]
  
  message('adding low transmission sites')

  low<- pull_low_transmission_sites(iso3c, site_data, bl)
  print(nrow(low))
  intvn<- append_low_transmission_sites(low_transmission = low, intvn)

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
           scenario,
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

  return(processed_output)

}

# save -------------------------------------------------------------------------
outputs<- lapply(c(0:max_draw), final_postprocessing)
saveRDS(outputs, 'final_output.rds')

# save aggregated dose output to file
dose_output<- rbindlist(lapply(c(1:nrow(completed)), get_dose_output, map = completed, output_filepath = 'J:/VIMC_malaria/archive/process_country/'), fill = TRUE)
doses<- dose_output |>
  group_by(scenario, year, parameter_draw) |>
  summarise(doses = sum(doses),
            .groups = 'keep') |>
  filter(year %in% c(2000:2100))

saveRDS(doses, 'dose_output.rds')

message('done with postprocessing')
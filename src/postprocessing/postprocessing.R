# postprocess in bulk
orderly2::orderly_parameters(iso3c = 'CMR',
                             description =  'fix_booster_coverage',
                             quick_run = FALSE)


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
completed<- completed_reports('process_country') |>
  dplyr::filter(iso3c == {{iso3c}},
                description == {{description}},
                quick_run == {{quick_run}}) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, .keep_all = TRUE) |>
  dplyr::arrange(iso3c, scenario, parameter_draw)

max_draw<- max(completed[scenario == 'no-vaccination', parameter_draw])

# vimc inputs
inputs<- completed_reports('process_inputs') |>
  filter(iso3c == {{iso3c}}) |>
  arrange(desc(date_time))
inputs<- unique(inputs, by = 'iso3c')

# pull model outputs for scaling to WMR cases: no-vaccination draw 0
scaling_filepath<- completed |> filter(parameter_draw == 0,
                                       scenario == 'no-vaccination')
scaling<- readRDS(paste0('J:/september_runs/VIMC_malaria/archive/process_country/', scaling_filepath$directory_name, '/outputs.rds'))
scaling<- scaling$country_output


# postprocess by draw ----------------------------------------------------------
final_postprocessing<- function(draw){

  message(paste0('postprocessing draw ', draw))

  bl_filepaths<- completed |> filter(parameter_draw == draw,
                                     scenario == 'no-vaccination')
  intvn_filepaths<- completed |> filter(parameter_draw == draw)

  # pull model outputs for all scenarios
  intvn<- rbindlist(lapply(c(1:nrow(intvn_filepaths)), get_site_output, map = intvn_filepaths, output_filepath = 'J:/september_runs/VIMC_malaria/archive/process_country/' ))
  
  # pull model outputs for all baseline scenarios (as a separate input into intervention processing)
  bl<- rbindlist(lapply(c(1:nrow(bl_filepaths)), get_site_output, map = bl_filepaths, output_filepath = 'J:/september_runs/VIMC_malaria/archive/process_country/'))

  # commenting out as now modelling introduction in all sites regardless of transmission intensity
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
           cohort_size = cohort_size) |>
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
max_draw<- 0
outputs<- lapply(c(0:max_draw), final_postprocessing)
saveRDS(outputs, 'final_output.rds')



dose_postprocessing<- function(draw){
  intvn_filepaths<- completed |> filter(parameter_draw == draw)
  
  # pull model outputs for all scenarios
  raw<- bind_rows(lapply(c(1:nrow(intvn_filepaths)), get_raw_output, map = intvn_filepaths, output_filepath = 'J:/september_runs/VIMC_malaria/archive/process_country/' ))
  raw<- raw |>
    mutate(site = paste0(site_name, '_', urban_rural, '_', scenario),
           site_ur = paste0(site_name, '_', urban_rural))
  
  ids<- unique(raw$site)
  
  process_doses<- function(id, raw, processed_output){

    subset<- raw |> filter(site == id)
    scenario<- unique(subset$scenario)
    site_name<- unique(subset$site_name)
    ur<- unique(subset$ur)

    dose_outputs<- pull_doses_output(subset, processed_output)
    dose_outputs<- dose_outputs |>
      mutate(site = id,
            scenario = scenario,
            site_name = site_name,
           ur= ur)
  
    return(dose_outputs)
  }

  doses<- rbindlist(lapply(ids,process_doses, raw= raw, processed_output = processed_output), fill= TRUE)
  
# remove cohort size because this is for the national population
  doses<- doses |>
    select(-cohort_size, -doses)

  case_output<- rbindlist(lapply(ids, site_postprocessing, dt = raw)) |>
    rename(year = t)
  
# merge on population from site files to more accurately calculate cases averted
 populations<- site_data$population |>
   rename(site_name = name_1,
          ur = urban_rural)
    
  
case_output<- merge(case_output, populations, by = c('site_name', 'ur', 'year'))

  case_output<- case_output |>
    rename(age = age_lower) |>
    mutate(cohort = pop * prop_n) 
  
  vax_cohort<- case_output |>
    filter(age == 1) |>
    select(site, year, cohort) |>
    rename(vaccine_cohort = cohort)

  doses<- merge(doses, vax_cohort, by = c('year', 'site')) |>
    mutate(fvp = rate_dosing * vaccine_cohort)
      
  case_output<- case_output |>
    mutate(cases = round(clinical * cohort),
           deaths = round(mortality * cohort)) |>
    group_by(site, year, site_ur, scenario, site_name, ur) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              .groups = 'keep') 
  
  #calculate cases averted by year and site
  novax<- case_output|> 
    filter(site %like% 'no-vaccination') |>
    rename(cases_novax = cases,
           deaths_novax = deaths) |>
    ungroup() |>
    select(-site, -scenario)

  vax<- case_output |> filter(!site %like% 'no-vaccination')
  
  averted<- merge(vax, novax, by = c('year', 'site_ur', 'ur', 'site_name')) |>
    mutate(cases_averted = cases_novax- cases,
           deaths_averted = deaths_novax - deaths)
    
  dose_output<- merge(averted, doses, by = c('year', 'scenario', 'site_name', 'ur', 'site')) 
  
#sum fvps and cases averted over 15 year period
intro_yr<- min(coverage_data[coverage> 0, year])

  test<- dose_output |>
    filter(year %in% c(intro_yr, intro_yr+15)) |>
    group_by(site_name, ur, scenario) |>
    summarise(cases_averted = sum(cases_averted),
              deaths_averted= sum(deaths_averted),
              fvp = sum(fvp),
            .groups = 'keep') |>
    mutate(cases_per = cases_averted/fvp * 100000,
           deaths_per = deaths_averted/fvp * 100000)



  return(dose_output)
  
}

dose_output<- lapply(c(0:max_draw), dose_postprocessing)

saveRDS(dose_output, 'dose_output.rds')

message('done with postprocessing')





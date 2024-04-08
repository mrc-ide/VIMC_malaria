# postprocess in bulk
output_filepath<- 'J:/VIMC_malaria/archive/process_country/'
source('J:/VIMC_malaria/src/postprocess_bulk/postprocessing_functions.R')
#source('diagnostic_report_functions.R')


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
  scaling<- readRDS(paste0(output_filepath, scaling_filepath$directory_name, '/outputs.rds'))
  scaling<- scaling$country_output


  # dose_output<- rbindlist(lapply(c(1:nrow(completed)), get_dose_output, map = completed), fill = TRUE)
  # doses<- dose_output |>
  #   group_by(scenario, year, parameter_draw) |>
  #   summarise(doses = sum(doses),
  #             .groups = 'keep') |>
  #   filter(year %in% c(2000:2100))
  #
  # saveRDS(doses, paste0('J:/VIMC_malaria/outputs/dose_output/', iso3c, 'dose_output.rds'))

  # run postprocessing by parameter draw
  for (draw in c(101:200)){

    message(paste0('postprocessing draw ', draw))

    bl_filepaths<- completed |> filter(parameter_draw == draw,
                                       scenario == 'no-vaccination')
    intvn_filepaths<- completed |> filter(parameter_draw == draw)

    # pull model outputs for all scenarios
    intvn<- rbindlist(lapply(c(1:nrow(intvn_filepaths)), get_site_output, map = intvn_filepaths))
    # pull model outputs for all baseline scenarios (as a separate input into intervention processing)
    bl<- rbindlist(lapply(c(1:nrow(bl_filepaths)), get_site_output, map = bl_filepaths))

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


    # split up outputs by scenario

    for (scen in unique(processed_output$scenario)){

      message('saving')

      saving<- processed_output |>
        filter(scenario == scen) |>
        select(-scenario)

      saveRDS(saving, paste0('J:/VIMC_malaria/outputs/stochastic_estimates/', scen,'/', iso3c, '_draw_', draw, '.rds'))

    }

    message(paste0('done with draw ', draw))

    message('done')
  }

}



completed_reports<- function(report_name){


  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'),  options = orderly2::orderly_search_options(allow_remote = TRUE))

  meta<- meta|>
    mutate(directory_name = id) |>
    tidyr::separate(col = id, into = c('date', 'time'), sep = '-')|>
    mutate(date= as.numeric(date)) |>
    mutate(date_time = as.numeric(paste0(date, time)))

  meta<- data.table(meta)
  meta<- meta[, index:= c(1:.N) ]


  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  pars<- data.table(pars)
  pars<- pars[, index:= c(1:.N)]

  meta<- meta |>
    select(directory_name, index, date_time)
  map<- merge(pars, meta, by = 'index')
  map<- map |>
    select(-index)

  return(map)
}

pull_most_recent_output<- function(iso3c){
  completed<- completed_reports('process_country') |>
    filter(iso3c == {{iso3c}},
           pfpr10 == TRUE,
           description == 'test_round2_changes',
           quick_run == FALSE) |>
    arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, pfpr10, .keep_all = TRUE) |>
    arrange(iso3c, scenario, parameter_draw)


  return(completed)
}


get_site_output<- function(index, map){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  draw<- metadata$parameter_draw

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))             # get output file
  sites<- rbindlist(lapply(output$site_output, function(x) return(x$processed_output))) #pull out processed site_level output
  sites<- sites |>
    mutate(parameter_draw = draw)
  return(sites)
}

get_dose_output<- function(index, map){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  scenario<- metadata$scenario
  draw<- metadata$parameter_draw

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))             # get output file
  output<- output$doses|>
    mutate(parameter_draw = draw,
           scenario = scenario)

  return(output)
}


check_outputs_finished<- function(completed){


  for(draw in c(1:max(completed$parameter_draw))){

    if(nrow(completed |> filter(parameter_draw== draw)) == 5){

      print(paste0('all jobs for draw ', draw, ' have completed'))
      max_draw<- draw
    } else{

      print(paste0('jobs still remaining for draw ', draw))
    }
  }

  return(max_draw)
}


pull_low_transmission_sites<- function(iso3c, site_data, processed_sites){
  # pull site output for no-vaccination for the low transmission settings

  site_data$prevalence<- site_data$prevalence |>
    filter(year == 2019) |>
    mutate(run_model = ifelse(pfpr > 0.10, TRUE, FALSE))

  # make exceptions for Madagascar, Ethiopia, and Sudan
  # hardcode for time's sake but operationalize later
  if(unique(site_data$prevalence$country) == 'Madagascar'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model))
  }

  if(unique(site_data$prevalence$country) == 'Ethiopia'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model))


  }


  if(unique(site_data$prevalence$country) == 'Sudan'){


    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |>
      mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))

  }
  if(length(unique(site_data$prevalence$run_model)) ==  1 & site_data$prevalence$run_model[1] == TRUE){

    return(data.table())

  } else{

    prevalence<- site_data$prevalence |>
      select(name_1, urban_rural, iso3c, run_model) |>
      rename(site_name = name_1,
             ur= urban_rural)


    site_info<- prevalence |>
      filter(iso3c == {{iso3c}},
             run_model == FALSE)

    append<- data.table()
    message(paste0('adding ', nrow(site_info), ' sites'))
    for (i in 1:nrow(site_info)){

      site<- site_info[ i,]

      add<- processed_sites |>
        filter(site_name == site$site_name & urban_rural == site$ur)

      append<- rbind(append, add, fill = T)
    }

    return(append)


  }
}



append_low_transmission_sites <- function(low_transmission, intvn){

  appended<- data.table()

  for(scen in unique(intvn$scenario)){

    message(scen)
    output<- intvn |> filter(scenario == scen)

    if(scen != 'no-vaccination'){

      message('appending')
      append<- copy(low_transmission)

      append<- append |>
        mutate(scenario = scen)

      full<- rbind(output, append, fill = TRUE)
    } else{

      full<- output
    }

    appended<- rbind(full, appended, fill= TRUE)
  }

  return(appended)
}


# country functions ------------------------------------------------------------
#' Aggregate VIMC outputs up to the country level
#' @param   dt     data frame with site-level outputs to aggregate
#' @param   pop    VIMC population input for country level (age- and year-specific)
#' @returns data frame with aggregated outputs up to the country level
#' @export
aggregate_outputs<- function(dt, pop){
  dt<- data.table::data.table(dt)

  dt[, `:=` (
    cases = sum(cases),
    deaths = sum(deaths),
    dalys = sum(dalys)),
    by = c('age', 'year', 'scenario', 'parameter_draw')]

  # remove cohort size, because for sites with some unmodelled locations, sum of cohort size != national population
  dt<- dt |>
    dplyr::select(-.data$cohort_size)
  dt <- unique(dt, by = c('age', 'year', 'scenario', 'parameter_draw'))
  pop<- pop |>
    dplyr::rename(age = age_from,
                  cohort_size = value) |>
    select(year, age, cohort_size)
  dt<- merge(dt, pop, by =c('age', 'year'))


  # calculate rates --------------------------------------------------------------
  dt[, `:=` (
    clinical = NULL,
    mortality = NULL,
    dalys_pp = NULL,
    site_name = NULL,
    urban_rural = 'total'
  )]

  return(dt)
}


#'   scale outputs based on cases from WMR from 2000-2020
#' @param dt  case output at country level
#' @param site_data site file
#' @export
scale_cases<- function(dt, site_data, scaling_data){

  pre_scale<- scaling_data |>
    group_by(year) |>
    summarise(cases = sum(cases))

  #average site file cases across last three years
  site_file_cases<- data.table::data.table(site_data$cases_deaths[, c('year', 'wmr_cases')])
  site_file_cases<- site_file_cases[year >= 2018]
  average_value<- mean(site_file_cases$wmr_cases)

  # calculate ratio in comparison to year 2020 cases in output
  output_cases<- pre_scale |>
    filter(year == 2020) |>
    select(cases)

  ratio<- average_value/output_cases$cases

  # add pre-scaled cases to output df as a new column
  dt<- dt |>
    mutate(pre_scaled_cases = cases)

  dt<- dt |>
    mutate(cases = cases * ratio)

  dt<- dt|>
    mutate(clinical= cases/cohort_size,
           mortality = deaths/ cohort_size,
           dalys_pp = dalys/ cohort_size) |>
    select(-urban_rural)

  return(dt)
}


# calculate the proportion of severe cases + deaths to recalculate severe cases + deaths after scaling
add_proportions<- function(dt){
  dt<- dt |>
    mutate(prop_severe = severe/cases,
           prop_deaths = deaths/ cases) |>
    mutate(prop_severe = if_else(is.na(.data$prop_severe), 0, prop_severe),
           prop_deaths = if_else(is.na(.data$prop_deaths), 0, prop_deaths))



  return(dt)
}


scale_par<- function(processed_output,
                     iso3c){

  pars<- readRDS('J:/VIMC_malaria/postprocessing/par_scaling_vimc.rds')
  le_africa<- readRDS('J:/VIMC_malaria/postprocessing/le_africa.rds')
  print(names(le_africa))
  pars<- pars |>
    filter(iso3c == {{iso3c}}) |>
    mutate(scaling_ratio = proportion_risk/ model_proportion_risk) |>
    rename(country = iso3c)

  processed_output<- merge(pars, processed_output, by = 'country')
  processed_output<- merge(processed_output, le_africa, by = 'age')

  processed_output<- processed_output |>
    mutate(cases = cases * scaling_ratio) |>
    mutate(severe = cases * prop_severe,
           deaths = cases * prop_deaths)


  # downstream, recalculate ylls + ylds
  processed_output<- processed_output |>
    mutate(ylls = deaths * life_expectancy,
           case_dw = ifelse(age <= 5, 0.051, 0.006),
           severe_dw = 0.133) |>
    mutate(ylds = severe * severe_dw * 0.04795 + cases * case_dw * 0.01375) |>
    mutate(dalys = ylds + ylls) |>
    select(-case_dw, -severe_dw)



  return(processed_output)


}


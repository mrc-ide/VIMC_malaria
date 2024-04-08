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



append_low_transmission_sites <- function(low_tranmission, intvn){

  appended<- data.table()

  for(scen in unique(intvn$scenario)){

    message(scen)
    output<- intvn |> filter(scenario == scen)

    if(scen != 'no-vaccination'){


      message('appending')
      append<- low_transmission |>
        mutate(scenario = scen)

      output<- rbind(output, append, fill = TRUE)
    }

    appended<- rbind(output, appended, fill= TRUE)
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

  pars<- readRDS('par_scaling_vimc.rds')
  le_africa<- readRDS('le_africa.rds')
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
pull_processed_output<- function(date_time){

  completed<- completed_reports('postprocess_bulk')
  completed<- completed |>
    arrange(desc(date_time)) |>
    filter(date_time >= {{date_time}})

  completed<- unique(completed, by = c('iso3c', 'description'))

  get_final_output<- function(filename){
    message(filename)
    output<- readRDS(paste0('J:/VIMC_malaria/archive/postprocess_bulk/',filename,"/final_output.rds"))
    return(output)

  }

  final_output<- rbindlist(lapply(completed$directory_name, get_final_output))

  # Encoding(final_output$country_name) <- "UTF-8"
  # final_output$country_name<- iconv(final_output$country_name, from="UTF-8", to="ASCII//TRANSLIT")

  return(final_output)
}


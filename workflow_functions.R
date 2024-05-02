# workflow functions  ----------------------------------------------------------

#' Run orderly report based on input parameters in map
#'
#' @param site one observation in parameter map
#' @param report_name  name of orderly report to extract metadata from
#' @export
run_report<- function(site, report_name){

  message(paste0('running ', site$iso3c, ' ', site$scenario))
  orderly2::orderly_run(name = report_name,
                        parameters = list(
                          iso3c = site$iso3c,
                          description = site$description,
                          parameter_draw= site$parameter_draw,
                          quick_run = site$quick_run,
                          scenario = site$scenario))
}


#'  Make a map of input parameters for VIMC modelling
#' @param iso3cs  countries to run models for you
#' @param scenarios  scenarios to run models for. Default is all scenarios for the current round
#' @export
make_parameter_map<- function(iso3cs,
                              scenarios =  c('no-vaccination',
                                             'malaria-rts3-bluesky',
                                             'malaria-rts3-default',
                                             'malaria-rts3-rts4-bluesky',
                                             'malaria-rts3-rts4-default'),
                              description,
                              parameter_draws,
                              quick_run){

  country_map<- data.table('iso3c' = iso3cs) |>
    mutate(description = description,
           quick_run = quick_run)

  full_map<- data.table()

  for (scen in scenarios){
    for (draw in parameter_draws){

      subset<- country_map|>
        mutate(scenario = scen,
               parameter_draw = draw)

      full_map<- rbind(subset, full_map)
    }}

  site_counts<- unique(rbindlist(lapply(c(1:nrow(full_map)), pull_site_numbers, map = full_map)))

  full_map<- merge(full_map, site_counts, by = c('iso3c', 'scenario'))
  full_map<- setorder(full_map, parameter_draw, -site_number)

  full_map<- full_map |>
    mutate(site_number = ifelse(site_number > 32, 32, site_number))

  return(full_map)
}


#' Return a list of the reports that have already completed from orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @export
completed_reports<- function(report_name){


  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'),  options = orderly2::orderly_search_options(allow_remote = TRUE))

  meta<- meta |>
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


#' Return a list of the reports that have already completed from orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @param map  parameter map for reports you would like to crossreference
#' @param date_time check if reports completed after a certain date, format YYYYMMDDHHMMSS
#' @export
check_reports_completed<- function(report_name, map, date_time){
  map<- map |> select(-site_number)

  completed<- completed_reports(report_name)
  completed<- completed |>
    select(-directory_name)|>
    filter(date_time >= {{date_time}})|>
    select(-date_time)

  intersection<- intersect(map, completed)

  return(intersection)

}


#' Return a list of the reports that are not a rerun based on orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @param map  parameter map for reports you would like to crossreference
#' @param date_time check if reports completed after a certain date, format YYYYMMDDHHMMSS
#' @export
check_not_a_rerun<- function(report_name, map, date_time){

  site_counts<- map |>
    select(scenario, iso3c, site_number)
  site_counts<- unique(site_counts)
  map<- map |> select(-site_number)

  completed<- completed_reports(report_name)
  completed<- completed |>
    select(-directory_name)|>
    filter(date_time >= {{date_time}}) |>
    select(-date_time)

  different<- setdiff(map, completed)
  different<- merge(different, site_counts, by = c('scenario', 'iso3c'))

  return(different)

}


#' Loop through reports locally based on parameters in map
#' @param report_name  name of orderly report
#' @param map  parameter map for reports
#' @export
run_local_reports<- function(map, report_name){
  for(index in c(1:nrow(map))){

    print(index)
    message(index)
    params<- as.list(map[index,])
    orderly2::orderly_run(name = report_name, parameters = params)

  }

  message('done')
}


#' Pull the number of sites to model for country job
#' @param index observation index for parameter map
#' @param map   parameter map
#' @export
pull_site_numbers<- function(index, map){

  map<- map[index,]
  iso3c<- map$iso3c
  scenario<- map$scenario

    site<- readRDS(paste0('src/process_inputs/site_files/new_site_files/', iso3c, '_new_eir.rds'))

    if(scenario == 'no-vaccination'){
      site_number<- nrow(site$sites)

    } else{

      site$prevalence <- site$prevalence |>
        mutate(run_model = ifelse(pfpr > 0.10, TRUE, FALSE))|>
        mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model))|>   # Madagascar exception
        mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model))|> # Ethiopia exception
        mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |> # Sudan exceptions
        mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))

      site_number<- nrow(site$prevalence |> filter(run_model == TRUE, year == 2019))
    }

  return(data.table('iso3c' = iso3c, 'scenario' = scenario, 'site_number' = site_number))
}

#' Submit jobs by core
#' @param core number of cores to request for job
#' @param dt   parameter map for reports
#' @export
submit_by_core<- function(core, dt){

  dt<- dt |>
    filter(site_number == core)
   message(unique(dt$site_number))

  hipercow::task_create_bulk_expr(
    orderly2::orderly_run(
      "process_country",
      parameters = list(iso3c = iso3c,
                        description = description,
                        quick_run = quick_run,
                        scenario = scenario,
                        parameter_draw = parameter_draw)),
    dt,
    resources = hipercow::hipercow_resources(cores = unique(dt$site_number)))

  message('submitted')
}




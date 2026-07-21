# helper functions
# workflow functions -----------------------------------------------------------------------------------------
#'  check what reports completed (must be in orderly parent directory)
#' @param report_name  name of orderly report to extract metadata from
#' @param descrip      corresponds to the description parameter above.
#' @export
completed_reports<- function(report_name, descrip){


  meta <- orderly::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'), options= orderly::orderly_search_options(allow_remote = TRUE))

  meta<- meta |>
    mutate(directory_name = id) |>
    tidyr::separate(col = id, into = c('date', 'time'), sep = '-')|>
    mutate(date= as.numeric(date)) |>
    mutate(date_time = as.numeric(paste0(date, time)))

  meta<- data.table(meta)
  meta<- meta[, index:= c(1:.N) ]

  meta<- hoist(meta, parameters)

  meta<- meta[1:nrow(meta),] #the first report run had different parameters than what is in production now, subset out

  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", stats::setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  pars<- data.table(pars)
  pars<- pars[, index:= c(1:.N)]

  meta<- meta |>
    select(directory_name, index, date_time)
  map<- merge(pars, meta, by = 'index')
  map<- map |>
    select(-index)

  return(map)
}
#' Return a list of the reports that are not a rerun based on orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @param map  parameter map for reports you would like to crossreference
#' @param date check if reports completed after a certain date, format YYYYMMDDHHMMSS
#' @export
check_not_rerun<- function(report_name, map, date){

  counts<- map |>
    select(scenario, iso3c, site_number)
  counts<- unique(counts)
  map<- map |> select(-site_number)

  completed<- completed_reports(report_name)
  completed<- completed |>
    select(-directory_name)|>
    dplyr::filter(date_time >= date) |>
    select(-date_time)

  different<- setdiff(map, completed)
  different<- merge(different, counts, by = c('scenario', 'iso3c'))

  return(different)

}



#'  Make a map of input parameters for VIMC modelling
#' @param iso3cs  countries to run models for you
#' @param scenarios  scenarios to run models for. Default is all scenarios for the current round
#' @param description reason for model run
#' @param parameter_draws draws to run model for
#' @param quick_run quick run setting (boolean)
#' @export
make_param_map<- function(iso3cs,
                              scenarios =  c('no-vaccination',
                                             'proxy'),
                              description,
                              parameter_draws,
                              quick_run){

  country_map<- data.table('iso3c' = iso3cs) |>
    mutate(description = description,
           quick_run = quick_run)

  full_map<- data.table()

  for (scen in scenarios){
    for (draw in parameter_draws){

        subset<- country_map |>
          mutate(scenario = scen,
                 parameter_draw = draw)


      full_map<- rbind(subset, full_map)
    }}

  test <- site_counts |> data.table::as.data.table()
  test<- test[scenario == 'malaria-rts3-rts4-default', scenario := 'proxy']

  full_map<- merge(full_map, test, by = c('iso3c', 'scenario'))
  full_map<- data.table::setorder(full_map, parameter_draw, -site_number)

  full_map<- full_map |>
    mutate(site_number = ifelse(site_number > 32, 32, site_number))

  return(full_map)
}


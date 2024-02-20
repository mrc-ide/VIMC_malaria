# workflow functions  ----------------------------------------------------------
run_process_inputs<- function(iso3cs){
  for (iso3c in iso3cs){

    orderly2::orderly_run(
      'process_inputs',
      list(iso3c = iso3c),
      root = dir)
  }
  message('done')

}


run_report<- function(site, report_name){

  message(paste0('running ', site$iso3c, ' ', site$scenario))
  orderly2::orderly_run(name = report_name,
                        parameters = list(
                          iso3c = site$iso3c,
                          description = site$description,
                          parameter_draw= site$parameter_draw,
                          quick_run = site$quick_run,
                          scenario = site$scenario
                        ))


}

make_parameter_map<- function(iso3cs,
                              scenarios =  c('no-vaccination',
                                             'malaria-r3-default',
                                             'malaria-r3-r4-default',
                                             'malaria-rts3-bluesky',
                                             'malaria-rts3-default',
                                             'malaria-rts3-rts4-bluesky',
                                             'malaria-rts3-rts4-default'),
                              description,
                              parameter_draws,
                              quick_run){


  country_map<- data.table('iso3c' = iso3cs)

  # expand grid out to include input parameters-
  country_map<- country_map |>
    mutate(description = description,
           quick_run = quick_run)


  full_map<- data.table()

  for (scen in scenarios){
    for (draw in parameter_draws){

      subset<- country_map|>
        mutate(scenario = scen,
               parameter_draw = draw)

      full_map<- rbind(subset, full_map)


    }

  }


site_counts<- rbindlist(lapply(iso3cs, pull_site_numbers))
full_map<- merge(full_map, site_counts, by = 'iso3c')
full_map<- setorder(full_map, parameter_draw, -site_number)
full_map<- full_map |>
  select(-site_number)


site_counts<- rbindlist(lapply(iso3cs, pull_site_numbers))

full_map<- merge(full_map, site_counts, by = 'iso3c')
full_map<- full_map |>
  mutate(site_number = ifelse(site_number > 32, 32, site_number))

  return(full_map)
}

completed_reports<- function(report_name){


  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'),  options = orderly2::orderly_search_options(allow_remote = TRUE))

  meta<- meta|>
    tidyr::separate(col = id, into = c('date', 'other'), sep = '-')|>
    mutate(date= as.numeric(date))

  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))

  return(pars)

}

check_reports_completed<- function(report_name, map){

  completed<- completed_reports(report_name)
  intersection<- intersect(map, completed)

  return(intersection)

}

check_not_a_rerun<- function(report_name, map){

  completed<- completed_reports(report_name)
  different<- setdiff(map, completed)

  return(different)

}

pull_site_numbers<- function(iso3c){

  site<-readRDS(paste0('src/process_inputs/site_files/', iso3c, '.rds'))

  site_number<- nrow(site$sites)

  return(data.table('iso3c' = iso3c, 'site_number' = site_number))
}


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


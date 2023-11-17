make_parameter_maps<- function(iso3cs,
                              population,
                              description,
                              parameter_draw,
                              quick_run,
                              burnin){
  
  # could be cleaner but will do for now
  
  map<- data.table()
  for (iso3c in iso3cs){
    
    sites<- data.table(readRDS(paste0('src/process_inputs/site_files/', iso3c, '.rds'))$sites)  # sites for country of interest
    sites<- remove_zero_eirs(iso3c, sites)
    
    site_num<- data.table('iso3c' = iso3c, 'site_name' = sites$name_1, 'ur' = sites$urban_rural)  
    
    map<- rbind(site_num, map)
  }
  
  # expand grid out to include input parameters-
  map<- map |>
    mutate(population = population,
           description = description,
           parameter_draw = parameter_draw,
           quick_run = quick_run,
           burnin = burnin)
  
  # scenarios to run (in no order)
  scenarios <-
    c(
      'no-vaccination',
      'malaria-r3-default',
      'malaria-r3-r4-default',
      'malaria-rts3-bluesky',
      'malaria-rts3-default',
      'malaria-rts3-rts4-bluesky',
      'malaria-rts3-rts4-default'
    )
  
  full_map<- data.table()
  for (scen in scenarios){
    subset<- map|>
      mutate(scenario = scen)
    
    full_map<- rbind(subset, full_map)
  }
  
  
  country_map<- data.table('iso3c' = iso3cs)
  
  # expand grid out to include input parameters-
  country_map<- country_map |>
    mutate(population = population,
           description = description,
           parameter_draw = parameter_draw,
           quick_run = quick_run,
           burnin = burnin)
  
  
  full_country_map<- data.table()
  for (scen in scenarios){
    subset<- country_map|>
      mutate(scenario = scen)
    
    full_country_map<- rbind(subset, full_country_map)
  }
  return(list('site_map' = full_map, 'country_map'= full_country_map))
}



# workflow functions  ----------------------------------------------------------

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
           parameter_draw = parameter_draw,
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
  
  
  return(full_map)
}
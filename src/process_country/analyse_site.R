
analyse_site<- function(site, 
                        site_data, 
                        coverage_data){
  
  model_input<- pull_input_params(site_name = site$site_name,
                                  ur = site$ur, 
                                  site_data = site_data, 
                                  coverage_data = coverage_data,
                                  scenario = site$scenario,
                                  parameter_draw = site$parameter_draw,
                                  quick_run = site$quick_run)
  model<- run_model(model_input)
  output<- process_output(model, site_name = site$site_name, ur = site$ur, scenario = site$scenario)
  
  return(output)
}


make_analysis_map<- function(site_data, 
                             test= F){
  
  site_info<- data.table('site_name' = site_data$sites$name_1, 'ur' = site_data$sites$urban_rural)
  site_info<- site_info |>
    mutate(scenario = scenario,
           quick_run = quick_run,
           parameter_draw = parameter_draw)
  
  if (test == T){
    
    site_info<- site_info[1:2]
    
  }
  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])
  
  return(sites)
}

aggregate_doses<- function(doses_full){
  doses_per_year <- doses_full |>
    dplyr::group_by(year) |>
    summarise(doses=sum(doses)) |>
    mutate(scenario=scenario)
  
  return(doses_per_year)
}


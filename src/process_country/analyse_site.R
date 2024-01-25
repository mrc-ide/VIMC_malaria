
analyse_site<- function(site,
                        site_data,
                        vimc_input){

  model_input<- pull_input_params(site_name = site$site_name,
                                  ur = site$ur,
                                  site_data = site_data,
                                  coverage_data = vimc_input$coverage_input,
                                  scenario = site$scenario,
                                  iso3c = site$iso3c,
                                  parameter_draw = site$parameter_draw,
                                  quick_run = site$quick_run)
  model<- run_model(model_input)
  output<- process_output(model,
                         vimc_input,
                          site_data = site_data,
                                       site_name = site$site_name,
                                       ur = site$ur,
                                       iso3c = site$iso3c,
                                       scenario = site$scenario,
                          quick_run = site$quick_run,
                         description= site$description)

  return(output)
}


make_analysis_map<- function(site_data,
                             test= F){

  site_info<- data.table('site_name' = site_data$sites$name_1, 'ur' = site_data$sites$urban_rural, 'iso3c' = site_data$sites$iso3c)
  site_info<- site_info |>
    mutate(scenario = scenario,
           quick_run = quick_run,
           parameter_draw = parameter_draw)

  if (test) {

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
reformat_output<- function(output){

  processed_results<- data.table()
  doses_full<- data.table()
  prev_full<- data.table()

  for(item in c(1:length(output))){

    subset<- output[[item]]

    processed<- subset$processed_output
    doses<- subset$doses
    prev<- subset$prevalence

    processed_results<- rbind(processed, processed_results, fill =T)
    doses_full<- rbind(doses, doses_full, fill= T)
    prev_full<- rbind(prev, prev_full, fill = T)

  }

  return(list('processed_full' = processed_results,
              'doses_full' = doses_full,
              'prev_full' = prev_full))

}

reformat_output<- function(output){

  processed_results<- data.table()
  doses_full<- data.table()
  prev_full<- data.table()

  for(item in c(1:length(output))){

    subset<- output[[item]]

    processed<- subset$processed_output
    doses<- subset$doses
    prev<- subset$prevalence

    processed_results<- rbind(processed, processed_results, fill =T)
    doses_full<- rbind(doses, doses_full, fill= T)
    prev_full<- rbind(prev, prev_full, fill = T)

  }

  return(list('processed_full' = processed_results,
              'doses_full' = doses_full,
              'prev_full' = prev_full))

}

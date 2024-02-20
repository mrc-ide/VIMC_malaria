
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
                             test= F,
                             scenario){

  site_info<- readRDS('pfpr10plus_admins.rds')
  site_info<- site_info |>
    filter(iso3c == {{iso3c}})

  if(scenario == 'no-vaccination'){

    site_info<- site_info |>
      mutate(run_model = TRUE)
  }


  site_info<- site_info |>
      filter(run_model == TRUE)


  site_info<- site_info |>
    mutate(scenario = {{scenario}},
           quick_run = {{quick_run}},
           parameter_draw = {{parameter_draw}}) |>
    rename(site_name = name_1)

  Encoding(site_info$site_name) <- "UTF-8"

  site_info$site_name<- iconv(site_info$site_name, from="UTF-8", to="ASCII//TRANSLIT")



  if (test) {

    site_info<- site_info[1:2]

  }
  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])

  return(sites)
}



# launch orderly reports (locally or on the cluster)
run_report<- function(i, 
                      report_name,
                      site_data, 
                      path,
                      population, 
                      description,
                      scenario,
                      parameter_draw,
                      burnin,
                      quick_run){
  
  
  message(i)
  site<- site_data[i,]
  
  orderly2::orderly_run(report_name,
                        list(
                          iso3c = site$iso3c,
                          site_name = site$name_1,
                          ur = site$urban_rural,
                          description = description,
                          population = population,
                          burnin = burnin,
                          scenario = scenario,
                          parameter_draw = parameter_draw,
                          quick_run = quick_run),
                          root = path)
  
  
}




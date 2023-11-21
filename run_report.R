# launch orderly reports (locally or on the cluster)
run_report<- function(i, 
                      report_name,
                      parameter_map, 
                      path){
  
  
  message(i)
  run<- parameter_map[i,]
  
  message(run$iso3c)
  
  # add in a check for if the report has already been run
  
  
  
 orderly2::orderly_run(report_name,
                        list(
                          iso3c = run$iso3c,
                          site_name = run$site_name,
                          ur = run$ur,
                          description = run$description,
                          population = run$population,
                          burnin = run$burnin,
                          scenario = run$scenario,
                          parameter_draw = run$parameter_draw,
                          quick_run = run$quick_run),
                          root = path)
  
 message('report complete')
  
}



run_report_country<- function(i, 
                      report_name,
                      parameter_map, 
                      path){
  
  
  message(i)
  run<- parameter_map[i,]
  
  
  orderly2::orderly_run(report_name,
                        list(
                          iso3c = run$iso3c,
                          description = run$description,
                          population = run$population,
                          burnin = run$burnin,
                          scenario = run$scenario,
                          parameter_draw = run$parameter_draw,
                          quick_run = run$quick_run),
                        root = path)
  
  message('report complete')
  
}

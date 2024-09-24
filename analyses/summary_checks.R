# summary checks
# purpose: write up quick checks of model outputs based on key issues + plot 
################################################################################


library(data.table)
library(vimcmalaria)
library(dplyr)
library(ggpubr)
library(ggforce)

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-7_malaria-rts3-rts4-bluesky.csv')
iso3cs<- unique(coverage$country_code)

outputs<- rbindlist(lapply(iso3cs , vimcmalaria::pull_postprocessed_output, description = 'round3', quick_run= FALSE))

retrieve_final<- function(directory){
  
  output<- readRDS(paste0('archive/postprocessing/', directory, '/final_output.rds'))[[1]]

  
  return(output)
}



retrieve_dose<- function(directory){
  
  output<- readRDS(paste0('archive/postprocessing/', directory, '/dose_output.rds'))
                   
  return(output)
}


final<- rbindlist(lapply(outputs$directory_name, retrieve_final))
dose<-  rbindlist(lapply(outputs$directory_name, retrieve_dose))





# run a few checks

# plot WMR cases over incidence over time

# pull WMR cases
pull_wmr<-function (iso3c){
  
  site<- readRDS(paste0('src/process_inputs/site_files/', iso3c, '_new_eir.rds'))
  wmr<- site$cases_deaths
  
  return(wmr)

}

wmr_cases<- rbindlist(lapply(iso3cs, pull_wmr))


wmr_diagnostic<- function(){
  
  outputs<- final[scenario == 'no-vaccination'] |>
    rename(iso3c = country) |>
    group_by(iso3c, year) |>
    summarise(cases= sum(cases), .groups = 'keep')
  
  p1<- ggplot()+
    geom_line(data = outputs, mapping= aes(x= year, y= cases))+
    geom_line(data = wmr_cases, mapping  = aes(x= year, y= wmr_cases, color = 'red'))+
    geom_vline(xintercept = 2018, linetype= 'dotted')+
    geom_vline(xintercept = 2020, linetype= 'dotted')+
    facet_wrap_paginate(~iso3c, nrow = 4, ncol = 4, scales = 'free', page = 1) +
    # plotting_theme + 
    theme(legend.position = 'none')+
    labs(title = 'World Malaria Report cases vs. modelled outputs')
  
  p2<- ggplot()+
    geom_line(data = outputs, mapping= aes(x= year, y= cases))+
    geom_line(data = wmr_cases, mapping  = aes(x= year, y= wmr_cases, color = 'red'))+
    geom_vline(xintercept = 2018, linetype= 'dotted')+
    geom_vline(xintercept = 2020, linetype= 'dotted')+
    facet_wrap_paginate(~iso3c, nrow = 4, ncol = 4, scales = 'free', page = 2) +
    # plotting_theme + 
    theme(legend.position = 'none')+
    labs(title = 'World Malaria Report cases vs. modelled outputs')
  
  print(p1)
  print(p2)
}
final<- outputs
pdf('wmr_diagnostic4.pdf', width = 12, height = 12)
wmr_diagnostic()
dev.off()

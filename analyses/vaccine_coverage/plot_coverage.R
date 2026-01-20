### plot coverage inputs

library(data.table)
library(tidyr)
library(ggplot2)

r21<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv')
rtss<- read.csv('src/process_inputs/vimc_inputs/sept_inputs/coverage_202409malaria-1_malaria-rts3-rts4-default.csv')

data<- rbind(r21, rtss)
ctys<- unique(r21$country)

plot_coverage<- function(cty){


  subset<- r21 |> filter(country == cty) |>
    mutate(coverage = coverage / proportion_risk)  # convert coverage to per population instead of per population at risk

  subset<- subset |> mutate(vaccine = ifelse(vaccine == 'R3', 'Primary series', 'Booster dose'))
  p<- ggplot(data= subset, mapping = aes(x= year, y= coverage, color = vaccine))+
    geom_line()+
    labs(title = 'Vaccine coverage forecast',
          subtitle = cty) +
    ggthemes::theme_solarized()

  print(p)
}

pdf('coverage_inputs_montagu_per_pop.pdf')
lapply(ctys, plot_coverage)

dev.off()



  subset<- subset |> mutate(vaccine = ifelse(vaccine == 'R3', 'Primary series', 'Booster dose'))
  p<- ggplot(data= subset, mapping = aes(x= year, y= coverage, color = vaccine, fill= vaccine))+
    geom_line()+
    labs(title = 'Vaccine coverage forecast', color= 'Series', fill ='Series', y= 'Vaccine coverage', x= 'Year') +
    ggthemes::theme_few(base_size = 16)

  print(p)



site<- site::fetch_site('SEN')
scene::plot_interventions(interventions= site$interventions, 
  population =site$population$population_total,
   group_var = 'country', text_size = 14)


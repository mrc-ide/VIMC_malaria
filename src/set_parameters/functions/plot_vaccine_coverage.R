
plot_vaccine_coverage<- function(site){
  # pull in vaccine forecast
  plotting_theme<- theme_bw(base_size = 12) +
    theme( legend.position = 'bottom',
           strip.text.x = element_text(size = rel(0.8)),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1), 
           #axis.text.y = element_blank(),
           text = element_text(family= 'Arial Narrow'),
           axis.ticks.y= element_blank(), 
           panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  intvns<- melt(data.table(site$interventions), 
                measure.vars = c('rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'))
  
  p<- ggplot(data= intvns, mapping= aes(x= year, y= value, color= variable))+
    geom_line()+
    plotting_theme +
    labs(y= 'Coverage value', x= 'Year', title= 'Vaccine coverage over time during model run',
         subtitle = paste0('Vaccine type: ', unique(intvns$vaccine), 
                           ' site: ', site_name, 
                           ' ', ur, 
                           ', model run: ', description, 
                           ' projection: ', scenario))
  
  
  print(p)
}

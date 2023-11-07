
plot_vaccine_coverage<- function(coverage_data){

  p<- ggplot(data= coverage_data, mapping= aes(x= year, y= coverage, color= vaccine))+
    geom_line()+
    plotting_theme +
    labs(y= 'Coverage value', x= 'Year', title= 'Vaccine coverage over time (VIMC input)',
         subtitle = paste0('Vaccine scenario: ', 
                           ' country: ', unique(coverage_data$country), 
                           ', model run: ', description, 
                           ' projection: ', unique(coverage_data$scenario)))
  
  
  print(p)
}

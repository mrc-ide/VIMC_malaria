# diagnostics for a single site  --------------------------------------------

library(ggplot2)
library(wesanderson)
library(ggforce)
library(ggpubr)
library(extrafont)

plotting_theme<- theme_bw(base_size = 12) +
  theme( legend.position = 'bottom',
         strip.text.x = element_text(size = rel(0.8)),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1), 
         #axis.text.y = element_blank(),
         text = element_text(family= 'Arial Narrow'),
         axis.ticks.y= element_blank(), 
         panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

population_diagnostic_model<- function(dt, pg, intro_yr, rows= 5, cols = 5){
  
  p<-   ggplot(data= dt, mapping = aes(x= year, y= cohort_size, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5)  +
    facet_wrap_paginate(~age, ncol= cols, nrow= rows, page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', 
         y= 'Population',
         title= paste0('Population over time: site ', unique(dt$iso3c), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
  
}

incident_cases_diagnostic<- function(dt, pg, intro_yr, cols= 5, rows= 5){
  
  p<-   ggplot(data= dt, mapping = aes(x= year, y= cases, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5)  +
    facet_wrap_paginate(~age, scales= 'free',
                        ncol= cols, nrow= rows, 
                        page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Clinical cases', 
         title= paste0('Incident clinical cases over time: site ', unique(dt$iso3c), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
  
}

incidence_rate_diagnostic<- function(dt, pg, intro_yr, cols= 5, rows= 5){
  
  p<-  ggplot(data= dt, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5)  +
    facet_wrap_paginate(~age,  nrow= rows, ncol= cols, page = pg) +
    labs(x= 'Time (in years)', 
         y= 'Incidence rate', 
         title= paste0('Incidence rate over time: ', unique(dt$iso3c), ', ', description),
         color= 'Scenario', 
         fill= 'Scenario') +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
}



mortality_diagnostic<- function(dt, pg, intro_yr, cols= 5, rows= 5){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= deaths, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5)  +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    facet_wrap_paginate(~age, scales = 'free', ncol= cols, nrow= rows, page = pg) +
    labs(x= 'Time (in years)', 
         y= 'Deaths', 
         title= paste0('Deaths over time: ', unique(dt$iso3c), ', ', description),
         color= 'Scenario', 
         fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
  
}
mortality_rate_diagnostic<- function(dt, pg, intro_yr, rows= 5, cols= 5){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= mortality, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5) +
    geom_vline(xintercept= 2023, linetype= "dotted") +
    facet_wrap_paginate(~age, ncol= cols, nrow= rows, page= pg) +
    labs(x= 'Time (in years)',
         y= 'Mortality rate', 
         title= paste0('Mortality rate over time: ', unique(dt$iso3c)),
         color= 'Scenario', 
         fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
  
}


daly_diagnostic<- function(dt, pg, intro_yr, rows= 5, cols =5){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= dalys, color= scenario, fill= scenario))+
    geom_line(alpha= 0.5)  +
    facet_wrap_paginate(~age,scales = 'free', ncol= cols, nrow= rows, page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'DALYs', title= paste0('DALYs over time: ', unique(dt$iso3c), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    theme_minimal()+
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
}



plot_cases_averted<- function(dt){
  
  
  iso3c<- unique(dt$iso3c)

  intvn<- dt|> subset(scenario %like% 'malaria')
  intvn<- intvn |> 
    rename(cases_intvn = cases,
           dalys_intvn = dalys,
           deaths_intvn = deaths)
  
  bl<- dt|> subset(scenario== 'no-vaccination')
  
  merged<- merge(intvn, bl, by= c('year', 'age'))
  
  merged<- merged |>
    mutate(cases_averted = cases - cases_intvn,
           deaths_averted = deaths - deaths_intvn,
           dalys_averted = dalys - dalys_intvn)
  
  merged<- data.table(merged)
  merged[, cases_averted := sum(cases_averted), by= 'age']
  merged<- unique(merged, by = 'age')
  
  ggplot(data= merged, mapping= aes(x= age, y= cases_averted))+
    geom_col() +
    plotting_theme +
    labs(title= paste0('Cases averted by age for site: ', iso3c),
         x= 'Age (years',
         y= 'Cases averted, 2035-2050')
}





# site file diagnostics --------------------------------------------------------

library(scales)
library(ggpubr)
library(wesanderson)
library(tidyr)





# plot cases + deaths

# cases and deaths  ------------------------------------------------------------
cases_deaths_diagnostic<- function(site){
  
  dt<- data.table(site$cases_deaths)
  
  site_name<- site$sites$name_1
  ur<- site$sites$urban_rural
  
  lower <-
    melt(dt,
         measure.vars = c('wmr_cases_l', 'wmr_deaths_l', 'incidence_l', 'mortality_l'))  |>
    select(year, variable, value) |>
    rename(lower = value)
  
  upper <-
    melt(dt,
         measure.vars = c('wmr_cases_u', 'wmr_deaths_u', 'incidence_u', 'mortality_u')) |>
    select(year, variable, value) |>
    rename(upper = value)
  
  median <-
    melt(dt,
         measure.vars = c('wmr_cases', 'wmr_deaths', 'incidence', 'mortality')) |>
    select(year, variable, value)
  
  lower[variable %like% 'cases', variable := 'cases']
  lower[variable %like% 'deaths', variable := 'deaths']
  lower[variable %like% 'mortality', variable := 'mortality']
  lower[variable %like% 'incidence', variable := 'incidence']
  
  upper[variable %like% 'cases', variable := 'cases']
  upper[variable %like% 'deaths', variable := 'deaths']
  upper[variable %like% 'mortality', variable := 'mortality']
  upper[variable %like% 'incidence', variable := 'incidence']
  
  median[variable %like% 'cases', variable := 'cases']
  median[variable %like% 'deaths', variable := 'deaths']
  median[variable %like% 'mortality', variable := 'mortality']
  median[variable %like% 'incidence', variable := 'incidence']
  
  plot <- Reduce(merge, list(lower, median, upper))
  
  p<- ggplot(data = plot) +
    geom_line(mapping = aes(x = year, y = value, color = variable)) +
    geom_ribbon(
      mapping = aes(
        x = year,
        ymin = lower,
        ymax = upper,
        fill = variable
      ),
      alpha = 0.2
    ) +
    facet_wrap( ~ variable, ncol = 4) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = wes_palette('Royal2', n = 4)) +
    scale_fill_manual(values = wes_palette('Royal2', n = 4))  +
    plotting_theme +
    labs(
      y = 'Value',
      x = 'Year',
      title = 'Cases, deaths, mortality, and incidence over time',
      subtitle = paste('Site: ', site_name, ' ', ur)
    )
  
  return(p)
}


# prevalence -------------------------------------------------------------------
prevalence_diagnostic<- function(site){
  
  dt<- melt(data.table(site$prevalence), measure.vars= c('pfpr', 'pvpr'))
  
  site_name<- site$sites$name_1
  ur<- site$sites$urban_rural
  
  p<- ggplot(dt, mapping = aes(x= year, y= value, color= variable))+
    geom_line()+
    plotting_theme+
    labs(x= 'Year', y= 'Prevalence value', color= 'Prevalence type',
         title= 'Parasite prevalence over time',
         subtitle = paste0('Site: ', site_name, ' ', ur))
  
  print(p)
}




# population  ------------------------------------------------------------------

population_diagnostic<- function(site){
  
  dt<- melt(site$population, measure.vars = c('par_pf', 'par_pv'))
  
  p1<- ggplot(data=dt)+
    geom_col(position = 'stack', mapping= aes(x= year, y= value, fill= variable)) +
    scale_fill_manual(values = wes_palette('Royal2', n = 2))  +
    geom_line(mapping= aes(x= year, y=par)) +
    plotting_theme+
    labs(title= 'Population at risk over time', 
         subtitle= paste0('Site: ', site_name, ' ', ur),
         y= 'Population', x= 'Year',
         fill= 'Population at risk') +
    scale_y_continuous(labels= comma)
  
  
  dt<- melt(site$population, measure.vars = c('pop', 'par'))
  
  p2<- ggplot(data=dt)+
    geom_line(mapping= aes(x= year, y= value, color= variable)) +
    plotting_theme+
    labs(title= 'Population over time', 
         subtitle= paste0('Site: ', site_name, ' ', ur),
         y= 'Population', x= 'Year') +
    scale_y_continuous(labels= comma)
  
  plots<- ggarrange(p1, p2, ncol= 2)
  
  return(plots)         
}

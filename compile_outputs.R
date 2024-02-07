# compile reports-- if results are missing, impute zeroes for now

path <-"J:/reclone/VIMC_malaria/archive/scale/"
library(ggpubr)
library(ggforce)
library(wesanderson)
library(extrafont)
library(scene)
library(purrr)
library(data.table)
library(dplyr)

extrafont::font_import()
plotting_theme<- theme_bw(base_size = 12) +
  theme( legend.position = 'bottom',
         strip.text.x = element_text(size = rel(0.8)),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
         #axis.text.y = element_blank(),
         text = element_text(family= 'Arial Narrow'),
         axis.ticks.y= element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank())



compile_stochastic_outputs<- function(filepath, description){
  filenames <-list.files(path)
  date = as.numeric(substring(filenames,1,8))

  list.files(paste0(path,"/",filenames[1]))

  # some functions
  get_country <- function(filename) {
    site_data <-readRDS(paste0(path,"/",filename,"/site_file.rds"))

    return(data.frame(iso = site_data$sites$iso3c[1], country=site_data$sites$country[1]))
  }


  get_scenario <- function(filename) {
    country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
    return(data.frame(scenario = country_output$scenario[1],
                      parameter_draw= country_output$parameter_draw[1],
                      description = country_output$description[1]))
  }

  get_output <- function(filename) {
    country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
    return(country_output)

  }
  #### compile country data
  cres <-data.frame(filenames = filenames, date = date,
                    date_time = as.numeric(paste0(substring(filenames,1,8),
                                                  substring(filenames, 10,13)))
  )
  cres<- data.table(cres)
  cres <- cbind(cres, bind_rows(map(cres$filenames, get_country)))
  cres <- cbind(cres, bind_rows(map(cres$filenames, get_scenario)))

  dim(cres)
  cres <- cres |>
    filter(description == {{description}}) |>
    arrange(desc(date_time)) |>
    dplyr::distinct(iso, country,parameter_draw, scenario, .keep_all = TRUE) |>
    arrange(country, scenario, parameter_draw)


  # bind outputs together
  outputs<- rbindlist(lapply(cres$filenames, get_output))

  # final formatting to template  ------------------------------------------------

  final<- outputs |>
    select(-pre_scaled_cases,
           -clinical,
           -mortality,
           -dalys_pp) |>
    select(disease,
           year,
           age,
           country,
           country_name,
           cohort_size,
           cases,
           dalys,
           deaths,
           ylls,
           scenario,
           parameter_draw)


  Encoding(final$country_name) <- "UTF-8"
  final$country_name<- iconv(final$country_name, from="UTF-8", to="ASCII//TRANSLIT")

  return(list('for_vetting' = outputs, 'to_submit' = final))
}


output<- compile_stochastic_outputs(filepath = {{filepath}},  description= 'full_parameter_run')
#
# for (scen in unique(final$scenario)){
#
#   print(scen)
#
#   saving<- final |>
#     filter(scenario == scen) |>
#     rename(run_id = parameter_draw)
#   print(length(unique(saving$country)))
#   saving<- saving |>
#     select(-scenario)
#
#   write.csv(saving, paste0('montagu/', 'stochastic-burden-est-', scen, '.csv'))
# }



# write up summary plots for all locations  -------------------------------------------
output<- output$for_vetting


scenario_comparison_plot<- function(output, country_name){

  dt<- output |>
    filter(country_name == {{country_name}},
           !is.na(scenario)) |>
    group_by(scenario, parameter_draw, country_name) |>
    summarise(cases= sum(cases),
              cohort_size = sum(cohort_size), .groups = 'keep') |>
    ungroup() |>
    mutate(cases_averted = cases[scenario == 'no-vaccination']- cases) |>
    mutate(prop_cases_averted = 1- cases/ cases[scenario== 'no-vaccination']) |>
    mutate(scenario = factor(scenario, levels =c("malaria-rts3-default",
                                                 "malaria-rts3-bluesky",
                                                 "malaria-rts3-rts4-default",
                                                 "malaria-rts3-rts4-bluesky",
                                                 "malaria-r3-default",
                                                 "malaria-r3-r4-default",
                                                 "no-vaccination")))

dt<- dt[!is.na(scenario)]
  p<- ggplot(data = dt, mapping = aes(x= prop_cases_averted, y= scenario, color = scenario, fill = scenario)) +
    geom_col(position= 'dodge') +
    facet_wrap(~parameter_draw, scales = 'free') +
    labs(title= 'Proportion of cases averted over 100-year period by age and scenario, faceted by parameter draw:',
         subtitle = {{country_name}})

return(p)
}


incidence_over_time<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, year, country_name, parameter_draw) |>
    select(country_name, scenario, year, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

    p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= year, y= cases, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= year, y= cases, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Cases',
         title= paste0('Cases over time'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

return(p)
}

incidence_over_age<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, age, country_name, parameter_draw) |>
    select(country_name, scenario, age, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= age, y= cases, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= age, y= cases, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Cases',
         title= paste0('Cases over age'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

  return(p)
}
incidence_rate_over_time<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, year, country_name, parameter_draw) |>
    select(country_name, scenario, year, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= year, y= cases/cohort_size, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= year, y= cases/cohort_size, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Incidence rate',
         title= paste0('Incidence rate over time'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme


  return(p)
}

incidence_rate_over_age<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, age, country_name, parameter_draw) |>
    select(country_name, scenario, age, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= age, y= cases/cohort_size, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= age, y= cases/cohort_size, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Incidence rate',
         title= paste0('Incidence rate over age'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme


  return(p)
}
deaths_over_time<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, year, country_name, parameter_draw) |>
    select(country_name, scenario, year, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)


  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= year, y= deaths, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= year, y= deaths, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Deaths',
         title= paste0('Deaths over time'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

  return(p)
}
deaths_over_age<- function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, age, country_name, parameter_draw) |>
    select(country_name, scenario, age, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')


  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)


  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= age, y= deaths, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= age, y= age, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Deaths',
         title= paste0('Deaths over age'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

  return(p)
}


dalys_over_time<-  function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, year, country_name, parameter_draw) |>
    select(country_name, scenario, year, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')

  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= year, y= dalys, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= year, y= dalys, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'DALYs',
         title= paste0('DALYs over time'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

  return(p)
}

dalys_over_age<-  function(output, scenario, country_name){

  dt<- output |>
    filter(country_name == {{country_name}}) |>
    filter(scenario == {{scenario}} | scenario == 'no-vaccination') |>
    group_by(scenario, age, country_name, parameter_draw) |>
    select(country_name, scenario, age, cases, deaths, dalys, cohort_size, parameter_draw) |>
    summarise(cases = sum(cases),
              deaths= sum(deaths),
              dalys = sum(dalys),
              cohort_size = sum(cohort_size),
              .groups= 'keep')

  stochastic_run<- dt |>
    filter(parameter_draw != 0)
  median_run<- dt |>
    filter(parameter_draw == 0)

  p<-   ggplot()+
    geom_line(data = median_run, mapping = aes(x= age, y= dalys, color =scenario))  +
    geom_line(data = stochastic_run, mapping = aes(x= age, y= dalys, color = scenario, group = parameter_draw), alpha = 0.2)  +
    facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'DALYs',
         title= paste0('DALYs over age'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

  return(p)
}
plot_intvn_coverage<- function(filepath){
    site_data<- readRDS(site)
    p<- plot_interventions_combined(
      interventions = site_data$interventions,
      population = site_data$population,
      group_var = c("country", "name_1"),
      include = c("itn_use", "itn_input_dist", "tx_cov", "smc_cov",'irs_cov', "pmc_cov", "rtss_cov"),
      labels = c("ITN usage", "ITN model input", "Treatment","SMC", "IRS", "PMC", "RTSS")
    )

return(p)
}



plot_vaccine_coverage<- function(coverage_data,country){

  coverage_data<- coverage_data |>
    filter(country == {{country}})
  p<- ggplot(data= coverage_data, mapping= aes(x= year, y= coverage, color= vaccine))+
    geom_line()+
    facet_wrap(~scenario) +
    plotting_theme +
    labs(y= 'Coverage value', x= 'Year', title= 'Vaccine coverage over time (VIMC input)',
         subtitle = paste0('Vaccine scenario: ',
                           ' country: ', unique(coverage_data$country),
                           ', model run: ', description,
                           ' projection: ', unique(coverage_data$scenario)))


  print(p)
}



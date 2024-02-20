# compile reports-- if results are missing, impute zeroes for now

path <-"J:/reclone/VIMC_malaria/archive/scale_and_plot/"

filenames <-list.files(path)
library(purrr)
date = as.numeric(substring(filenames,1,8))

list.files(paste0(path,"/",filenames[1]))

# some functions
get_country <- function(filename) {
  site_data <-readRDS(paste0(path,"/",filename,"/site_file.rds"))

  return(data.frame(iso = site_data$sites$iso3c[1], country=site_data$sites$country[1]))
}


<<<<<<< HEAD
output<- compile_stochastic_outputs(filepath = {{filepath}},  description= 'full_parameter_run')
#
for (scen in unique(output$to_submit$scenario)){

  print(scen)

  saving<- output$to_submit |>
    filter(scenario == scen) |>
    rename(run_id = parameter_draw)
  print(length(unique(saving$country)))
  saving<- saving |>
    select(-scenario)

  write.csv(saving, paste0('montagu/', 'stochastic-burden-est-', scen, '.csv'))
}



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
=======
get_scenario <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
  return(data.frame(scenario = country_output$scenario[1],
                    parameter_draw= country_output$parameter_draw[1]))
>>>>>>> parent of 2ac2b63 (rearranged a few reports, writing full run diagnostics)
}

get_output <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
  return(country_output)

<<<<<<< HEAD
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
    #facet_wrap_paginate(~scenario) +
    #geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Cases',
         title= paste0('Cases over time'),
         subtitle = {{country_name}},
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Darjeeling1', n= 2)) +
    scale_fill_manual(values= wes_palette('Darjeeling1', n= 2))  +
    plotting_theme

return(p)
=======
>>>>>>> parent of 2ac2b63 (rearranged a few reports, writing full run diagnostics)
}
#### compile country data
cres <-data.frame(filenames = filenames, date = date,
                  date_time = as.numeric(paste0(substring(filenames,1,8), substring(filenames, 10,13)))
)
cres<- data.table(cres)
cres<- cres[date_time >= 202401291731]

cres <- cbind(cres, bind_rows(map(cres$filenames, get_country)))
cres <- cbind(cres, bind_rows(map(cres$filenames, get_scenario)))

dim(cres)
cres <- cres |>
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

for (scen in unique(final$scenario)){

  print(scen)

  saving<- final |>
    filter(scenario == scen) |>
    rename(run_id = parameter_draw)
  print(length(unique(saving$country)))
  saving<- saving |>
    select(-scenario)

  write.csv(saving, paste0('montagu/', 'stochastic-burden-est-', scen, '.csv'))
}





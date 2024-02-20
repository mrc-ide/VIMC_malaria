# postprocess  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = 'NGA',
                             scenario = 'malaria-rts3-rts4-default',
                             quick_run = FALSE,
                             parameter_draw = 0,
                             description = 'full_parameter_run')

orderly2::orderly_artefact('Final output', 'processed_output.rds')

# dependencies  ----
library(site)
library(data.table)
library(dplyr)
library(malariasimulation)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(tibble)
library(postie)
library(data.table)
library(countrycode)

files<- list.files('functions/', full.names = T)

invisible(lapply(files, source))

bl_scenario<- 'no-vaccination'
scenario_name <- scenario
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))

orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == this:scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == this:parameter_draw)",
                             c(outputs.rds = "outputs.rds"))



if(scenario_name != 'no-vaccination'){

  orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == environment:bl_scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == this:parameter_draw)",
                               c(bl_output.rds = "outputs.rds"))

  # workflow outputs  ----
  bl_output<-  readRDS('bl_output.rds')
  bl_results<- bl_output$country_output
  bl_prev<- bl_output$prevalence
  processed_sites<- rbindlist(lapply(bl_output$site_output, function(x) return(x$processed_output)))

}



intvn_output<- readRDS('outputs.rds')
intvn_results<- intvn_output$country_output
doses<- intvn_output$doses
int_prev<- intvn_output$prevalence

# vimc inputs ----
vimc_input<- readRDS('vimc_input.rds')
site_data <- readRDS('site_file.rds')

coverage_data<- vimc_input$coverage_input
le <- vimc_input$le
vimc_pop<- vimc_input$population_input_all_age
pop_single_yr<- vimc_input$population_input_single_yr
pop_data<- vimc_input$population_input_all_age

if(scenario_name == 'no-vaccination'){

  # scale cases up to 2020 values based on ratio from no-vaccination scenario
  output<- scale_cases(intvn_results, site_data)
  processed_output<- output

} else{
  # bind intervention and baseline outputs together
  low_transmission<- pull_low_transmission_sites(iso3c, processed_sites)
  intvn_results<- rbind(intvn_results, low_transmission)

  # aggregate up high transmission and low-transmission sites
  dt<- aggregate_outputs(intvn_results, pop_single_yr)

  # add in baseline results for scaling
  dt<- rbind(bl_results, intvn_results,fill= T)
  dt<- dt[scenario!= TRUE]

  # scale cases up to 2020 values based on ratio from no-vaccination scenario
  output<- scale_cases(dt, site_data)
  processed_output<- output


}

scale_par<- function(processed_output, iso3c){

  pars<- readRDS('par_scaling_vimc.rds')
  pars<- pars |>
    filter(iso3c == {{iso3c}}) |>
    mutate(scaling_ratio = proportion_risk/ model_proportion_risk)

  processed_output<- merge(pars, processed_output, by = 'iso3c')

  processed_output<- processed_output |>
    mutate(cases = cases * scaling_ratio)    #severe cases / DALYS?


  return(processed_output)


}

processed_output<- scale_par(processed_output, iso3c)

# format and save
processed_output<- processed_output |>
  filter(scenario == scenario_name) |>
  mutate(description = description,
         parameter_draw = parameter_draw,
         quick_run = quick_run)

saveRDS(processed_output, 'processed_output.rds')




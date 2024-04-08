# postprocess  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             scenario = NULL,
                             quick_run = NULL,
                             parameter_draw = NULL,
                             description = NULL,
                             pfpr10 = NULL)

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

scenario_name <- scenario
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "merged_site_file.rds"))

orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == this:scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == this:parameter_draw &&
                                                 parameter:pfpr10 == this:pfpr10)",
                             c(outputs.rds = "outputs.rds"))
# pull in no vaccination outputs for parameter draw 0 to scale outputs to WMR cases
param_draw<- 0
bl_scenario<- 'no-vaccination'

orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == environment:bl_scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == environment:param_draw)",
                             c(scaling_output.rds = "outputs.rds"))

# if this is an intervention scenario, read in no-vaccination outputs to bind to moderate-to-high transmission site outputs
if(!(scenario_name == 'no-vaccination')){

  orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == environment:bl_scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == this:parameter_draw)",
                               c(bl_output.rds = "outputs.rds"))

  bl_output<-  readRDS('bl_output.rds')
  bl_sites<- rbindlist(lapply(bl_output$site_output, function(x) return(x$processed_output)))
  bl_sites<- bl_sites |>
    mutate(parameter_draw = {{parameter_draw}})

}



# workflow outputs  ----
scaling_output<-  readRDS('scaling_output.rds')$country_output

intvn_output<- readRDS('outputs.rds')
intvn_sites<- rbindlist(lapply(intvn_output$site_output, function(x) return(x$processed_output)))
intvn_sites<- intvn_sites |>
  mutate(parameter_draw = {{parameter_draw}})

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


if(scenario_name != 'no-vaccination'){

  # bind intervention and baseline outputs together
  low_transmission<- pull_low_transmission_sites(iso3c, site_data, bl_sites)
  #rename scenario from low-transmission outputs so they are now attributed to the vaccine scenario of interest
  low_transmission<- low_transmission |>
    mutate(scenario = {{scenario_name}})

  site_output<- rbind(intvn_sites, low_transmission, fill = T)

}else{

  site_output<- intvn_sites
}


dt<- aggregate_outputs(site_output, pop_single_yr)

# scale cases up to 2020 values based on ratio from no-vaccination scenario
output<- scale_cases(dt, scaling_data= scaling_output,  site_data = site_data)
output<- add_proportions(output)


# scale cases based on difference between site file PAR and VIMC PAR
processed_output<- scale_par(output, iso3c= {{iso3c}})


# format and save (only outputs for the scenario of interest)
processed_output<- processed_output |>
  filter(scenario == scenario_name) |>
  mutate(description = description,
         parameter_draw = parameter_draw,
         quick_run = quick_run)

saveRDS(processed_output, 'processed_output.rds')




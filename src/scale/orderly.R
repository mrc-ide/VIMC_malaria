# scale  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             scenario = NULL,
                             quick_run = NULL,
                             parameter_draw = NULL,
                             description = NULL)


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
orderly2::orderly_artefact('Final output', 'processed_output.rds')

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
  dt<- rbind(bl_results, intvn_results,fill= T)
  dt<- dt[scenario!= TRUE]

  # scale cases up to 2020 values based on ratio from no-vaccination scenario
  output<- scale_cases(dt, site_data)
  processed_output<- output


}


processed_output<- processed_output |>
  filter(scenario == scenario_name) |>
  mutate(description = description,
         parameter_draw = parameter_draw,
         quick_run = quick_run)

saveRDS(processed_output, 'processed_output.rds')




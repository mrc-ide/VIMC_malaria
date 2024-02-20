# plot  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             scenario = NULL,
                             quick_run = NULL,
                             description = NULL)


# dependencies  ----
source('diagnostics.R')
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


completed<- completed_reports('process_country')
completed<- completed |>
  filter(iso3c== {{iso3c}},
         scenario == {{scenario}},
         description == {{description}},
         quick_run == {{quick_run}})

draws<- max(completed$parameter_draw)

intvn<- data.table()
baseline<-data.table()

for (draw in c(0:5)){

  message(draw)

  metadata<- orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == this:scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == environment:draw)",
                               c('itvn_output_${draw}.rds' = "outputs.rds"))


  dt<- readRDS(metadata$files$here)$country_output
  dt <- dt |>
    mutate(parameter_draw = draw)

  intvn<- rbind(intvn, dt, fill = T)



  metadata<- orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == environment:bl_scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == environment:draw)",
                               c('bl_output_${draw}.rds' = "outputs.rds"))



  dt<- readRDS(metadata$files$here)$country_output
  dt <- dt |>
    mutate(parameter_draw = draw)
  baseline<- rbind(baseline, dt, fill = T)
}


# workflow outputs  ----
# vimc inputs ----
vimc_input<- readRDS('vimc_input.rds')
site_data <- readRDS('site_file.rds')

coverage_data<- vimc_input$coverage_input
le <- vimc_input$le
vimc_pop<- vimc_input$population_input_all_age
pop_single_yr<- vimc_input$population_input_single_yr
pop_data<- vimc_input$population_input_all_age

# bind intervention and baseline outputs together
dt<- rbind(baseline, intvn,fill= T)
dt<- dt[scenario!= TRUE]

# scale cases up to 2020 values based on ratio from no-vaccination scenario
output<- scale_cases(dt, site_data)
processed_output<- output

# format outputs for plotting
descriptive_dt<- format_descriptive_data()

input_data<- format_input_data()

  # render report ------
  rmarkdown::render(input= 'stochastic_report.Rmd',
                    output_file = 'stochastic_report',
                    output_format = 'html_document',
                    params= list('descriptive_data' = descriptive_dt,
                                 'input_data' = input_data))




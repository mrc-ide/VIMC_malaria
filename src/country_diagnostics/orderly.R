# produce country diagnostic report ------------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL,
                             quick_run= NULL)


orderly2::orderly_description('Produce country diagnostic report for site')


print("Country ISO = ")
print(iso3c)
print("Scenario = ")
print(scenario)

library(dplyr)
library(data.table)
lapply(list.files('functions/', full.names = T), source)

# pull processed output
intvn_metadata<- orderly2::orderly_dependency("process_country",
                                              "latest(parameter:iso3c == this:iso3c 
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == this:scenario
                                                          && parameter:parameter_draw == this:parameter_draw
                                                          && parameter:quick_run == this:quick_run)",
                                              c(intvn_output.rds = "country_output.rds"))

# pull baseline output
baseline_scenario_name<- 'no-vaccination'
baseline_metadata<- orderly2::orderly_dependency("process_country",
                                                 "latest(parameter:iso3c == this:iso3c
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == environment:baseline_scenario_name
                                                          && parameter:parameter_draw == this:parameter_draw
                                                          && parameter:quick_run == this:quick_run)",
                                                 c(baseline_output.rds = "country_output.rds"))

# VIMC inputs
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(coverage_input.rds = "coverage_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(mort_rate_input.rds = "mort_rate_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(population_input_all_age.rds = "population_input_all_age.rds"))

# read in VIMC inputs ----------------------------------------------------------
site_data<- readRDS('site_file.rds')
coverage_data<- readRDS('coverage_input.rds')
mort<- readRDS('mort_rate_input.rds')
population_data<- readRDS('population_input_all_age.rds')

# read in model inputs ---------------------------------------------------------
intvn_output<- readRDS('intvn_output.rds')
baseline_output<- readRDS('baseline_output.rds')

# bind intervention and baseline outputs together
processed_output<- rbind(baseline_output, intvn_output, fill = T)
processed_output<- data.table(processed_output)
processed_output<- processed_output[scenario!= TRUE]


# subset VIMC inputs to country of interest ------------------------------------
intvn_scenario<- scenario
coverage_data<- coverage_data |>
  filter(scenario == intvn_scenario,
         country_code == iso3c)

population_data<- population_data |>
  filter(country_code == iso3c)
message('read inputs successfully')


# Cases averted per 100,000 vaccinated were:
# outcomes_averted <- round(pull_outcomes_averted_per_100k_vacc(intvn_output , baseline_output , doses_output ))

# render report
rmarkdown::render(input= 'diagnostic_report_country.Rmd',
                  output_file = 'country_diagnostic_report',
                  output_format = 'html_document',
                  params= list('iso3c' = iso3c,
                               'description'= description,
                               'population' = population,
                               'quick_run' = quick_run,
                               'burnin' = burnin,
                               'parameter_draw' = parameter_draw,
                               'processed_output' = processed_output,
                               'site_data' = site_data,
                               'coverage_data' = coverage_data,
                               'population_data' = population_data,
                               'mort' = mort))


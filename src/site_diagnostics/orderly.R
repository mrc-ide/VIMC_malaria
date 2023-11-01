# produce diagnostic report ------------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             site_name = NULL,
                             ur = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL)


orderly2::orderly_description('Produce diagnostic report for site')
#orderly_artefact('Diagnostic report', 'site_diagnostic_report.html')

library(dplyr)
lapply(list.files('functions/', full.names = T), source)

# pull processed output
intvn_metadata<- orderly2::orderly_dependency("process_site",
                                              "latest(parameter:iso3c == this:iso3c 
                                                          && parameter:site_name == this:site_name 
                                                          && parameter:ur == this:ur 
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == this:scenario
                                                          && parameter:parameter_draw == this:parameter_draw)",
                                              c(processed_output.rds = "processed_output.rds"))

# pull baseline output
# baseline_metadata<- orderly2::orderly_dependency("process_site",
#                                               "latest(parameter:iso3c == this:iso3c 
#                                                           && parameter:site_name == this:site_name 
#                                                           && parameter:ur == this:ur 
#                                                           && parameter:population == this:population
#                                                           && parameter:description == this:description
#                                                           && parameter:scenario == 'no-vaccination'
#                                                           && parameter:parameter_draw == this:parameter_draw)",
#                                               c(baseline.rds = "baseline_output.rds"))
# raw model output
raw_output<- orderly2::orderly_dependency("process_site",
                                          "latest(parameter:iso3c == this:iso3c 
                                                          && parameter:site_name == this:site_name 
                                                          && parameter:ur == this:ur 
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == this:scenario
                                                          && parameter:parameter_draw == this:parameter_draw)",
                                          c(raw_model_output.rds = "raw_model_output.rds"))
# site file inputs
vaccine_coverage_input<- orderly2::orderly_dependency("set_parameters",
                                                      "latest(parameter:iso3c == this:iso3c 
                                                          && parameter:site_name == this:site_name 
                                                          && parameter:ur == this:ur 
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == this:scenario
                                                          && parameter:parameter_draw == this:parameter_draw)",
                                                      c(vaccine_plot_input.rds = "vaccine_plot_input.rds"))

orderly2::orderly_dependency("process_inputs",
                                        "latest(parameter:iso3c == this:iso3c)",
                                        c(site_file.rds = "site_file.rds"))

orderly2::orderly_dependency("process_inputs",
                                         "latest(parameter:iso3c == this:iso3c)",
                                         c(coverage_input.rds = "coverage_input.rds"))

orderly2::orderly_dependency("process_inputs",
                                         "latest(parameter:iso3c == this:iso3c)",
                                         c(mort_rate_input.rds = "mort_rate_input.rds"))
site_data<- readRDS('site_file.rds')
coverage_data<- readRDS('coverage_input.rds')
mort<- readRDS('mort_rate_input.rds')

# read in inputs to pass into report as parameters
model_input<- readRDS('vaccine_plot_input.rds')
raw_output<- readRDS('raw_model_output.rds')
processed_output<- readRDS('processed_output.rds')

message('read inputs successfully')


# render report
rmarkdown::render(input= 'M:/Lydia/VIMC_malaria/src/site_diagnostics/diagnostic_report.Rmd',
                  output_file = 'site_diagnostic_report',
                  output_format = 'html_document',
                  params= list('iso3c' = iso3c,
                               'description'= description,
                               'site_name' = site_name,
                               'ur' = ur,
                               'population' = population,
                               'burnin' = burnin,
                               'parameter_draw' = parameter_draw,
                               'model_input' = model_input,
                               'raw_output' = raw_output,
                               'processed_output' = processed_output,
                               'site_data' = site_data,
                               'coverage_data' = coverage_data,
                               'mort' = mort))


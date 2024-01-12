# scale and plot  --------------------------------------------------------------
orderly2::orderly_parameters(iso3c = 'BDI', 
                             scenario = 'malaria-rts3-rts4-default',
                             quick_run = TRUE,
                             parameter_draw = 2,
                             description =  'refactor_testing')

# dependencies  ----
bl_scenario<- 'no-vaccination'

orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))

orderly2::orderly_dependency("process_country", "latest(parameter:iso3c == this:iso3c &&
                                                 parameter:scenario == this:scenario &&
                                                 parameter:quick_run == this:quick_run &&
                                                 parameter:description == this:description &&
                                                 parameter:parameter_draw == this:parameter_draw)", 
                             c(outputs.rds = "outputs.rds"))

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

# bind intervention and baseline outputs together
dt<- rbind(bl_results, intvn_results,fill= T)
dt<- dt[scenario!= TRUE] 

# scale cases up to 2020 values based on ratio from no-vaccination scenario
output<- scale_cases(dt, site_data)

# plot outputs
descriptive_dt<- format_descriptive_data()
input_data<- format_input_data

# render report ------
rmarkdown::render(input= 'diagnostic_report_country.Rmd',
                  output_file = 'country_diagnostic_report',
                  output_format = 'html_document',
                  params= list('descriptive_data' = descriptive_dt,
                               'input_data' = input_dt))



# analyse country  -------------------------------------------------------------
# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'MDG', 
                             scenario = 'malaria-rts3-rts4-default',
                             quick_run = TRUE,
                             parameter_draw = 0,
                             description =  'runtime_test')

orderly2::orderly_description('Analyze vaccine impact at the site level')
orderly2::orderly_artefact('Processed output', 'outputs.rds')

# packages and functions ----
pkgs <- c( 'site', 'data.table',  'dplyr', 'scene', 'malariasimulation', 'openxlsx', 'ggplot2', 'tidyr', 'tibble', 'postie', 'data.table', 'countrycode')
invisible(lapply(pkgs, library, character.only = TRUE))

function_files<- list.files('J:/vimcmalaria/R/', full.names = T)
invisible(lapply(function_files, source))

# functions ----
source('analyse_site.R')


# read in dependencies  ----
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))

vimc_input<- readRDS('vimc_input.rds') 
site_data <- readRDS('site_file.rds')

# vimc inputs ----
coverage_data<- vimc_input$coverage_input
le <- vimc_input$le
vimc_pop<- vimc_input$population_input_all_age
pop_single_yr<- vimc_input$population_input_single_yr

# make a map of input parameters for site function
site_df<- remove_zero_eirs(iso3c, site_data)
map<- make_analysis_map(site_df, test = T)

# run analysis function for each site + urban/rural combination ----
output<- (lapply(map,
                  analyse_site,
                  site_data= site_data,
                  coverage_data=coverage_data))

reformat_output<- function(output){
  
  processed_results<- data.table()
  doses_full<- data.table()
  prev_full<- data.table()
  
  for(item in c(1:length(output))){
    
    subset<- output[[item]]
    
    processed<- subset$processed_output
    doses<- subset$doses
    prev<- subset$prevalence
    
    processed_results<- rbind(processed, processed_results, fill =T)
    doses_full<- rbind(doses, doses_full, fill= T)
    prev_full<- rbind(prev, prev_full, fill = T)
    
  }
  
  return(list('processed_full' = processed_results, 
              'doses_full' = doses_full, 
              'prev_full' = prev_full))
  
}

test<- reformat_output(output)

processed_results<- test$processed_full
doses_full<- test$doses_full
prev_full<- test$prev_full


# aggregate outputs up to country level
dt<- aggregate_outputs(processed_results, pop_single_yr)

if (scenario != 'no-vaccination'){
  doses_full<- aggregate_doses(doses_full)
}


#save every output to one dependency 
outputs<- list('country_output' = dt, 'site_output' = output, 'doses' = doses_full, 'prevalence' = prev_full )


saveRDS(outputs, 'outputs.rds')
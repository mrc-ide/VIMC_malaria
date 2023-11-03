# launch models ----------------------------------------------------------------
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL,
                             site_name = NULL,
                             ur = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL,
                             quick_run = NULL)



orderly2::orderly_description('Launch malariasimulation model')
orderly2::orderly_artefact('Model output', 'model_output.rds')

orderly2::orderly_dependency("set_parameters",
                             "latest(parameter:iso3c == this:iso3c 
                             && parameter:site_name == this:site_name 
                             && parameter:ur == this:ur 
                             && parameter:scenario == this:scenario 
                             && parameter:description == this:description 
                             && parameter:population == this:population
                             && parameter:parameter_draw == this:parameter_draw
                             && parameter:quick_run == this:quick_run)",
                             c(model_input.rds = "model_input.rds"))



library(dplyr)
library(malariasimulation)

model_input <- readRDS("model_input.rds")

params <- model_input$param_list
params$progress_bar <- TRUE


timesteps <<- model_input$param_list$timesteps


message('running the model')
model <- malariasimulation::run_simulation(timesteps = params$timesteps,
                                    parameters = params)

# add identifying information to output
model <- model |>
  mutate(site_name = site_name,
         urban_rural = ur,
         iso = iso3c,
         description = description, 
         scenario = scenario,
         parameter_draw = parameter_draw)



# save model runs somewhere
message('saving the model')
saveRDS(model, 'model_output.rds')

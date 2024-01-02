# packages  
library(orderly2)
library(site)
library(data.table)
library(dplyr)
library(malariasimulation)
lapply(list.files('functions/', full.names = T), source)

# obtain list of countries to run model for
coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)

# get metadata for previous runs and site diagnostics
meta_sitediag <-readRDS("post_processing_LO/meta_sitediag.rds")
metalaunch <- readRDS('post_processing_LO/meta_launch_models.rds')

### Get total run time for the most recent site runs:

metalaunch_final<-metalaunch |> 
  filter(description=='complete_run' & quick_run==F) |>
  dplyr::mutate(other=as.numeric(other)) |>
  ungroup() |>
  dplyr::group_by(iso3c, site_name, ur, scenario) |>
  dplyr::filter(date==max(date)) |> 
  filter(other==max(other))


dir<- getwd()


### Some test sites - find the original model inputs

test<-meta_sitediag |> filter(iso3c=='SDN' & site_name=='Al Qadarif')
ids <- metalaunch_final |> filter(iso3c=='SDN' & site_name=='Al Qadarif' & ur=='rural') |>
  pull(id2)
#list.files(paste0('archive/launch_models/', ids[1]))

# read in and change params to see if faster:
model_input <- readRDS(paste0('archive/launch_models/', ids[1],"/model_input.rds"))

params <- model_input$param_list
params$progress_bar <- TRUE
params_original <- params

params <- model_input$param_list
params$progress_bar <- TRUE
params$severe_incidence_rendering_min_ages <- c(0:9, 10,12,14, 17,20,25,35,50)*365
params$severe_incidence_rendering_max_ages <- 
  c(params$severe_incidence_rendering_min_ages[2:length(params$severe_incidence_rendering_min_ages)] -1,
    200*365)
params$clinical_incidence_rendering_min_ages <- params$severe_incidence_rendering_min_ages
params$clinical_incidence_rendering_max_ages <- params$severe_incidence_rendering_max_ages
params$age_group_rendering_min_ages <- NULL
params$age_group_rendering_max_ages <- NULL
params_new <- params


###### set up cluster
ctx <- context::context_save("ctxs4", sources= 'functions/run_report.R')
config <- didehpc::didehpc_config(
  use_rrq = FALSE,
  cores = 1,
  cluster = "wpia-hn", #"fi--dideclusthn", # , "fi--didemrchnb""fi--didemrchnb"
  template = "AllNodes")

obj <- didehpc::queue_didehpc(ctx, config = config)


###### launch and save
timesteps <<- model_input$param_list$timesteps

model1 <- obj$enqueue(malariasimulation::run_simulation(timesteps = params_original$timesteps,
                                           parameters = params_original))
model2 <- obj$enqueue(malariasimulation::run_simulation(timesteps = params_new$timesteps,
                                                        parameters = params_new))
model1$status()
model2$status()

model1$log()
model1 <- obj$enqueue(malariasimulation::run_simulation(timesteps = 42340,
                                                        parameters = params_original))

t1<-Sys.time()
malariasimulation::run_simulation(timesteps = params_original$timesteps,
                                  parameters = params_original)
t2<-Sys.time()
malariasimulation::run_simulation(timesteps = params_new$timesteps,
                                  parameters = params_new)
t3<- Sys.time()
print(t2-t1)
print(t3-t2)

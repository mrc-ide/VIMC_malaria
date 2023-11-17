# process country --------------------------------------------------------------
orderly2::orderly_parameters(iso3c =  NULL,
                             description = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             scenario = NULL,
                             quick_run = NULL)


library(postie)
library(dplyr)
library(data.table)

orderly2::orderly_description('Process model outputs')
orderly2::orderly_artefact('Processed output', 'country_output.rds')


# read in model outputs for all sites in country
orderly2::orderly_dependency("process_inputs",
                             "latest(parameter:iso3c == this:iso3c )",
                             c(site_file.rds = "site_file.rds"))
site_data <- readRDS('site_file.rds')
sites<- site_data$sites
  
output<- data.table()
  
for (i in 1:nrow(sites)) {
    
    site<- sites[i,]
    site_name<- site$name_1
    ur<- site$urban_rural


    message(i)
    metadata<-orderly2::orderly_dependency("process_site", quote(latest(parameter:iso3c == this:iso3c &&
                                                                   parameter:description == this:description &&
                                                                   parameter:population == this:population &&
                                                                   parameter:scenario == this:scenario &&
                                                                   parameter:burnin == this:burnin &&
                                                                   parameter:site_name == environment:site_name &&
                                                                   parameter:ur == environment:ur &&
                                                                   parameter:parameter_draw == this:parameter_draw &&
                                                                   parameter:quick_run == this:quick_run)),
                                           c('processed_output_${site_name}_${ur}.rds' = "processed_output.rds"))
    
    dt<- readRDS(metadata$files$here)
    output<- rbind(output, dt, fill = T)
    
}

# sum cases up to country level ------------------------------------------------
dt<- copy(output)
dt<- data.table(dt)

dt[, `:=` (
  cases = sum(cases),
  deaths = sum(deaths),
  dalys = sum(dalys),
  cohort_size = sum(cohort_size)
),
by = c('age', 'year', 'scenario')]

dt <- unique(dt, by = c('age', 'year', 'scenario'))


# calculate rates --------------------------------------------------------------
dt[, `:=` (
  clinical = NULL,
  mortality = NULL,
  dalys_pp = NULL,
  prop_n = NULL,
  site_name = iso3c,
  urban_rural = 'total'
)]

dt<- dt|>
  mutate(clinical= cases/cohort_size,
         mortality = deaths/ cohort_size,
         dalys_pp = dalys/ cohort_size) |>
  select(-site_name, -urban_rural)


# # scale outputs based on cases from WMR from 2000-2020
# # first sum cases by year in model output and compare
# pre_scale<- dt |>
#   group_by(year) |>
#   summarise(cases = sum(cases)) 
#   
# scaling<- merge(pre_scale, site_data$cases_deaths[, c('year', 'wmr_cases')], by= 'year') 
# scaling<- scaling |>
#   mutate(ratio = wmr_cases / cases)

# save outputs  ----------------------------------------------------------------
saveRDS(dt, 'country_output.rds')
message('done')


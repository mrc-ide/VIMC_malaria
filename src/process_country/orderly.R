# process country --------------------------------------------------------------
orderly2::orderly_parameters(iso3c =  NULL,
                             description = NULL,
                             population = NULL,
                             burnin = NULL,
                             parameter_draw = NULL,
                             projection = NULL)


library(postie)
library(dplyr)
library(data.table)

orderly2::orderly_description('Process model outputs')
orderly2::orderly_artefact('Processed output', 'country_output.rds')


# read in model outputs for all sites in country
sites <- readRDS(paste0('site_files/', iso3c, '.rds'))
sites<- sites$sites
  
output<- data.table()
  
  for (i in 1:nrow(sites)) {
    
    site<- sites[i,]
    site_name<- site$name_1
    ur<- site$urban_rural


    message(i)
    metadata<-orderly2::orderly_dependency("process_site", quote(latest(parameter:iso3c == this:iso3c &&
                                                                   parameter:description == this:description &&
                                                                   paramter:population == this:population &&
                                                                   parameter:scenario == this:scenario &&
                                                                   parameter:burnin == this:burnin &&
                                                                   parameter:site_name == environment:site_name &&
                                                                   parameter:ur == environment:ur)),
                                           c(file.rds = "processed_output.rds"))
    
    dt<- readRDS(metadata$files$here)
    output<- rbind(output, dt, fill = T)
    
}

# sum cases up to country level ------------------------------------------------
dt<- copy(output)

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
         dalys_pp = dalys/ cohort_size)

# save outputs  ----------------------------------------------------------------
saveRDS(dt, 'country_output.rds')
message('done')


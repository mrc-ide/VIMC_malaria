## read in current site files for all countries
library(purrr)
sitefilenames<- list.files('src/process_inputs/site_files/')
sitefiles<-map(paste0('src/process_inputs/site_files/',sitefilenames), readRDS)

for(i in 1:length(sitefiles)) {
  sitefiles[[i]]$interventions <- sitefiles[[i]]$interventions |>
    mutate(smc_cov=0)
}
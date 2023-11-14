# check that the sites you seek to launch have non-zero EIRs
remove_zero_eirs<- function(iso3c, sites){
  eirs<- data.table(readRDS(paste0('src/process_inputs/site_files/', iso3c, '.rds'))$eir)  # sites for country of interest
  eirs<- eirs[spp == 'pf' & eir == 0]
  remove<- eirs[, c('name_1', 'urban_rural')]
  
  for (i in 1:nrow(remove)){
    
    message(paste0('removing site ', i))
    
    sites<- sites[!(name_1== remove[i, name_1] & urban_rural == remove[i, urban_rural])]
    
  }
  return(sites)
}


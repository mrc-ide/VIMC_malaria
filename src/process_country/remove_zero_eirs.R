# check that the sites you seek to launch have non-zero EIRs
remove_zero_eirs<- function(iso3c, sites, eirs){
  
  eirs<- data.table::data.table(eirs)
  eirs<- eirs[spp == 'pf' & eir == 0]
  remove<- eirs[, c('name_1', 'urban_rural')]
  
  sites<- data.table::data.table(sites)
  if(nrow(remove) > 0){
    for (i in 1:nrow(remove)){
      
      message(paste0('removing site ', i))
      
      sites<- sites[!(name_1== remove[i, name_1] & urban_rural == remove[i, urban_rural])]
      
    }
    
  } else{
    message('No zero eir sites to remove')
  }
  return(sites)
}


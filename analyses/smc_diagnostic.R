# SMC modification diagnostic
# Chad example 

library(data.table)
library(ggplot2)
library(dplyr)

chad<- data.table(readRDS('src/process_inputs/smc_inputs/smc_cov_adm1TCD.rds'))
chad_site_file<- readRDS('src/process_inputs/site_files/TCD.RDS')
chad_intvns<- data.table(chad_site_file$interventions)


pdf('smc_diagnostic.pdf')

for(name in unique(chad$name_1)){
  message(name)
  
  p1<- ggplot(data = chad[name_1 == name], mapping = aes(x = month, y = smc_cov)) +
    geom_line() +
    facet_wrap( ~ year) +
    labs(title = paste0('SMC coverage in updated data, ', name))
  
  
  p2<- ggplot(data = chad_intvns[name_1 == name], mapping = aes(x = year, y = smc_cov, color = smc_n_rounds)) +
    geom_line() +
    labs(title = paste0('SMC coverage in site file, ', name))
  
  
  print(p1)
  print(p2)
}


dev.off()

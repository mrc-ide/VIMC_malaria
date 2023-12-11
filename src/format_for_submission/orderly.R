# format for submission
orderly2::orderly_parameters(scenario = NULL,
                             description = NULL)

coverage<- read.csv('N:/Lydia/VIMC_malaria/src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)


output<- data.table()

for (iso3c in iso3cs){
  
  
  
  
  message(i)
  metadata<-orderly2::orderly_dependency("process_site", quote(latest(parameter:iso3c == environment:iso3c &&
                                                                        parameter:description == this:description &&
                                                                        parameter:scenario == this:scenario)),
                                         c('country_output_${iso3c}.rds' = "country_output.rds"))
  
  dt<- readRDS(metadata$files$here)
  output<- rbind(output, dt, fill = T)
  
  
}



# remove extraneous columns

# save final submission file

write.xlsx(output, 'final_output.xlsx')
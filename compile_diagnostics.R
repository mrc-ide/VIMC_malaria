# compile diagnostic reports so easier to peruse

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-1_malaria-r3-default.csv')
iso3cs<- unique(coverage$country_code)
dir<- getwd()

# for(iso3c in iso3cs){
#   dir.create(paste0('diagnostics/', iso3c))
# }


completed<- completed_reports('plot')
completed<- completed |>
  filter(description == 'full_parameter_run',
         parameter_draw== 0)


organize_output<- function(ind){

  info<- completed[index== ind]
  message(ind)

  file.copy(from = paste0('archive/plot/', info$id, '/country_diagnostic_report.html'),
            to= paste0('diagnostics/', info$iso3c, '/'))

  file.rename(from= paste0('diagnostics/', info$iso3c, '/country_diagnostic_report.html'),
              to= paste0('diagnostics/', info$iso3c, '/', info$scenario, '_', info$parameter_draw, '_', 'report.html'))

}




lapply(completed$index, organize_output)

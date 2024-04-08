# compile reports-- if results are missing, impute zeroes for now
path <-"J:/VIMC_malaria/archive/postprocess/"
setwd('J:/VIMC_malaria')
library(ggpubr)
library(ggforce)
library(wesanderson)
library(extrafont)
library(scene)
library(purrr)
library(data.table)
library(dplyr)

coverage<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202310gavi-7_malaria-rts3-rts4-bluesky.csv')
iso3cs<- unique(coverage$country_code)

scenarios<- c('malaria-rts3-rts4-bluesky', 'malaria-rts3-bluesky', 'malaria-rts3-default', 'malaria-rts3-rts4-default', 'no-vaccination')
for (scenario in scenarios){

  message(scenario)
  files<- list.files(paste0('J:/VIMC_malaria/outputs/stochastic_estimates/', scenario), full.names = TRUE)

  for(draw in c(101:200)){

    message(draw)
    draw_files<- files[files %like% paste0('draw_', draw, '.rds')]

    outputs<- rbindlist(lapply(draw_files, readRDS))

    # outputs<- outputs |>
    #   select(-run_id)

    message(length(unique(outputs$country)))
    write.csv(outputs, paste0('montagu/stochastic/', 'stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_', scenario, '_draw_', draw, '.csv'), row.names = F)
    #write.csv(outputs, paste0('montagu/central/', 'central-burden-est-', scenario, '.csv'), row.names = F)

  }


}


# save outputs in a directory per country for diagnostics

for (iso in iso3cs){

  message(iso)

  country_output<- data.table()
  for(scenario in scenarios){

    files<- list.files(paste0('J:/VIMC_malaria/outputs/stochastic_estimates/', scenario), full.names = TRUE)
    files<- files[files %like% iso]
    outputs<- rbindlist(lapply(files, readRDS))
    outputs<- outputs |>
       mutate(scenario = {{scenario}})
    country_output<- rbind(country_output, outputs, fill = TRUE)


  }

  saveRDS(country_output, paste0('outputs/stochastic_estimates/by_country/', iso, '.rds'))

}


# compile outputs by draw


  message(iso)

  country_output<- data.table()
  for(scenario in scenarios){

    files<- list.files(paste0('J:/VIMC_malaria/outputs/stochastic_estimates/', scenario), full.names = TRUE)
    files<- files[files %like% 'draw_0.rds']
    outputs<- rbindlist(lapply(files, readRDS))
    outputs<- outputs |>
      mutate(scenario = {{scenario}})
    country_output<- rbind(country_output, outputs, fill = TRUE)


  }

  saveRDS(country_output, paste0('outputs/stochastic_estimates/by_draw/draw_0.rds'))





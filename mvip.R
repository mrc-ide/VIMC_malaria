# MVIP data 
library(dplyr)
library(data.table)
library(ggplot2)
library(vimcmalaria)
cols<- c('#CE5137', '#1B4F72', '#45B39D', '#BA4A00')

setwd('J:/september_runs/VIMC_malaria')
# pick one country to start
completed <- completed_reports("process_country") |>
  dplyr::filter(
    description == "booster_update",
    scenario %in% c('malaria-r3-r4-bluesky', 'no-vaccination'),
    quick_run == FALSE,
    parameter_draw == 0
  ) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, .keep_all = TRUE) |>
  dplyr::arrange(iso3c, scenario, parameter_draw)


#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_site_outputs<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  iso3c<- metadata$iso3c
  scenario<- metadata$scenario

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))                  # get output file
  sites<- data.table::rbindlist(lapply(output$site_output, function(x) return(x$processed_output))) #pull out processed site_level output


  saveRDS(sites, paste0('J:/september_runs/VIMC_malaria/analyses/draft/site_output/', iso3c, '_', scenario, '_site_output.rds'))

  return(sites)
}
# pull VIMC mortality results
# admin level site results
site_output<- lapply(c(1:nrow(completed)), get_site_outputs, map = completed, output_filepath = 'archive/process_country/')


files<- list.files('analyses/draft/site_output/', full.names = T)
output<- rbindlist(lapply(files, readRDS))
output<- output |>
  mutate(id = paste0(site_name, '_', urban_rural))
#merge on pfpr data 

plot_mortality<- function(){

# summarize mortality as all-age
test<- output |> 
  filter(id == 'Bengo_both',
         year < 2051) |>
  group_by(scenario, year) |>
  summarise(cohort_size= sum(cohort_size),
            deaths= sum(deaths),
            .groups = 'keep') |>
  mutate(mortality= deaths/cohort_size)
  
  #pull mortality from 2022 and mortality from 2030
  mort_pre<- test |> filter(year == 2022)
  more_post<- test |> filter(year == 2030)
  
  
ggplot(test, aes(x=year, y = mortality, color= scenario))+
  geom_line()+
  scale_color_manual(values= cols)+
  labs(title = 'mortality rate by scenario')

}






# figures and analyses  --------------------------------------------------------------------------------------------------------
library(malariasimulation)
library(site)
library(vimcmalaria)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
# functions -----------------------------------------------------------------------------------------------------------------------
cols<- c('#CE5137', '#1B4F72', '#45B39D', '#BA4A00')

plotting_theme<- theme_bw(base_size = 11) +
  theme( legend.position = 'bottom',
         strip.text.x = element_text(size = rel(0.8)),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
         text = element_text(family= 'TT Arial'),
         axis.ticks.y= element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_final_outputs<- function(descrip){

  completed<- completed_reports('postprocessing')
  completed<- completed[description == {{descrip}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, quick_run, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description, quick_run)

  pull_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- rbindlist(readRDS(paste0('archive/postprocessing/', directory_name, '/final_output.rds')))
    return(output)
  }

  outputs<- rbindlist(lapply(c(1:nrow(completed)), pull_output, map = completed))
}
save_site_output<- function(index){
  
  output<- get_site_output(index, map = completed, output_filepath = 'archive/process_country/')

  map<- completed[index,]
  scenar<- map$scenario
  iso3c<- map$iso3c
  draw<- map$parameter_draw
  saveRDS(output, paste0('analyses/draft/site_output/', iso3c, '_', scenar, '_', draw, '.rds'))
  
  
}
#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_dose_output<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  draw<- metadata$parameter_draw

  message(directory)

  output<- rbindlist(readRDS(paste0(output_filepath, directory, '/dose_output.rds')))                  # get output file
  return(output)
}

extract_site_data<- function(directory){
  
  site_file<- readRDS(paste0('archive/process_inputs/', directory, '/site_file.rds'))

  # extract prevalece data
  prev<- site_file$prevalence |>
    filter(year %in% c(2000, 2019)) |>
    mutate(id = paste0(name_1, '_', urban_rural)) |>
    data.table()
  
  prevs<- dcast(prev, id  ~ year, value.var = 'pfpr') |>
    rename(pfpr_2000 = `2000`,
           pfpr_2019 = `2019`)

  
  #extract intervention coverage data for last available year
  intvns<- site_file$interventions |>
    filter(year == 2022) |>
      mutate(id = paste0(name_1, '_', urban_rural)) |>
    select(id, itn_use, tx_cov, smc_cov, pmc_cov, irs_cov)

  site_info<- merge(prevs, intvns, by = 'id')
  
  return(site_info)
}


# pull metadata
# pull metadata for completed outputs for this country

# pull country level output -------------- --------------------------------------
total<- compile_final_outputs('fix_booster_coverage')

# pull site level output  ------------------------------------------------------
completed<- completed_reports('process_country') |>
  dplyr::filter(description == 'fix_booster_coverage',
                quick_run == FALSE) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, .keep_all = TRUE) |>
  dplyr::arrange(iso3c, scenario, parameter_draw)


lapply(c(1:nrow(completed)), save_site_output) # this will take a long time

files<- list.files('analyses/draft/site_output/', full.names = TRUE)
outputs<- rbindlist(lapply(files, readRDS))
outputs<- outputs |>
  mutate(id = paste0(site_name, '_', urban_rural))


# extract dose output   --------------------------------------------------------
completed<- completed_reports('postprocessing') |>
  dplyr::filter(description == 'fix_booster_coverage',
                quick_run == FALSE) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, quick_run, description, .keep_all = TRUE)

doses<- rbindlist(lapply(c(1:nrow(completed)), get_dose_output, map = completed, output_filepath = 'archive/postprocessing/'))

# Extract site file prevalence data for 2000 and 2019 and merge onto site level data-set
completed<- completed_reports('process_inputs') |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, .keep_all = TRUE) 


prevs<- rbindlist(lapply(completed$directory_name, extract_site_data))
output_prev<- merge(outputs, prevs, by = 'id', allow.cartesian = TRUE)

# make a list of outputs and save to list for easy access
reference<- list('country_output' = total,
                 'site_output' = outputs,
                 'dose_output' = doses,
                 'prevalence_output' = prevs)

saveRDS(reference, 'analyses/draft/reference.rds')


reference<- readRDS('analyses/draft/reference.rds')



# plot outputs for paper  ----------------------------------------------------------------------------------------------
# plot 1: cases and deaths averted by country
comparison<- total |>
  group_by(country, scenario, run_id) |>
  summarise(cases = sum(cases),
            deaths= sum(deaths), 
            dalys= sum(dalys), 
          cohort_size = sum(cohort_size),
        .groups = 'keep')

control<- comparison |> filter(scenario == 'no-vaccination') |> ungroup() |> rename(cases_control= cases, deaths_control= deaths, dalys_control= dalys) |> select(-scenario)
vaccine<- comparison |> filter(scenario %in% c('malaria-r3-r4-default', 'malaria-rts3-rts4-default'))

comparison<- merge(control, vaccine, by = c('run_id', 'country')) |>
  mutate(cases_averted= cases_control- cases,
         deaths_averted = deaths_control-deaths,
         dalys_averted = dalys_control - dalys) |>
  group_by(country, scenario) |>
  summarise(cases_averted_med = median(cases_averted),
            cases_av_upper = quantile(cases_averted, 0.975),
          cases_av_lower = quantile(cases_averted, 0.025),
          deaths_averted = median(deaths_averted),
          deaths_av_upper= quantile(cases_averted, 0.975),
          deaths_av_lower = quantile(cases_averted, 0.025),
        .groups = 'keep')

# plot cases averted by country
ggplot(data= comparison, aes(x= reorder(country, cases_averted_med), y= cases_averted_med, fill = scenario)) +
  geom_col(position = 'dodge', stat= 'identity')+
  geom_errorbar(mapping = aes(ymin= cases_av_lower, ymax= cases_av_upper), position= 'dodge') +
  plotting_theme +
  theme(legend.position = 'none') +
  scale_fill_manual(values = cols)+
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Cases averted',
       x= 'Country',
       y= 'Cases averted per 100,000 fully vaccinated people')



doses<- reference$dose_output
dose_output<- reference$dose_output
prevs<- reference$prevalence_output |>
  rename(site_ur = id)
# make plot of cases averted per dose across all sites
dose_output<- doses |>
  filter(scenario %in% c('malaria-r3-r4-bluesky'))
# summarise cases averted per fvp for first 15 years 
dose_output<- dose_output |>
  filter(year <= 2037) |>
  group_by(site_ur) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            fvp = sum(fvp),
            doses_total= sum(doses_total),
            .groups = 'keep') 


dose_output<- merge(dose_output, prevs, by = 'site_ur', allow.cartesian = TRUE)
dose_output$pfpr_2000<- cut(dose_output$pfpr_2000, breaks = c(0, 0.03, 0.05, 0.10, 0.15, 0.20, 0.25, 0.35, 0.45, 0.55, 0.65, 2))
dose_output$pfpr_2019<- cut(dose_output$pfpr_2019, breaks = c(0, 0.03, 0.05, 0.10, 0.15, 0.20, 0.25, 0.35, 0.45, 0.55, 0.65, 2))

dose_output<- dose_output |> filter(!is.na(pfpr_2000))

p1<-ggplot(dose_output, mapping = aes(x= pfpr_2000, y= cases_averted*100000/fvp)) +
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Cases averted per 100k FVPs by PFPR in 2000',
       y= 'Cases averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 

p2<- ggplot(dose_output, mapping = aes(x= pfpr_2019, y= cases_averted*100000/fvp)) +
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Cases averted per 100k FVPs by PFPR in 2019',
       y= 'Cases averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 

p3<- ggplot(dose_output, mapping = aes(x= pfpr_2000, y= deaths_averted*100000/fvp)) +
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Deaths averted per 100k FVPs by PFPR in 2000',
       y= 'Deaths averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 

p4<- ggplot(dose_output, mapping = aes(x= pfpr_2019, y= deaths_averted*100000/fvp)) +
  geom_boxplot()+
  theme_minimal() +
  labs(title = 'Deaths averted per 100k FVPs by PFPR in 2019',
       y= 'Deaths averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 
  


ggarrange(p1, p2, p3, p4, nrow= 2,ncol = 2)


ggplot(dose_output, mapping = aes(x= pfpr_2019, y= cases_averted*100000/fvp)) +
  geom_point()+
  theme_minimal() +
  labs(title = 'Cases averted per 100k FVPs by PFPR in 2019',
       y= 'Cases averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 
  
ggplot(dose_output, mapping = aes(x= pfpr_2019, y= deaths_averted*100000/fvp)) +
  geom_point()+
  theme_minimal() +
  labs(title = 'Deaths averted per 100k FVPs by PFPR in 2019',
       y= 'Deaths averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 



#run some regressions?

mod<- glm(deaths_averted/fvp ~pfpr_2019  + itn_use  + tx_cov, data= dose_output)
summary(mod)

stargazer(mod, type="text",  out="models.htm")


mod<- glm(cases_averted/fvp ~pfpr_2019  + itn_use  + smc_cov + tx_cov, data= dose_output)
summary(mod)



dose_output$smc_cov<-  cut(dose_output$smc_cov, breaks = c(-.01, 0.6, 0.8, 1 ))
dose_output$itn_cov<-  cut(dose_output$smc_cov, breaks = c(-.01, 0.3, 0., 1 ))

ggplot(dose_output, mapping = aes(x= smc_cov, y= deaths_averted*100000/fvp, color= pfpr_2019)) +
  geom_boxplot()+
 facet_wrap(~pfpr_2019) +
  theme_minimal() +
  labs(title = 'Deaths averted per 100k FVPs by PFPR in 2019',
       y= 'Deaths averted per 100k FVP') +
  scale_y_continuous(labels = scales::comma) 






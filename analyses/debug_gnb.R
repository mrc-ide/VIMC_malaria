# debug Guinea-Bissau
# find reason why model runs in GNB are eliminating in all settings at around 2010



library(tidyr)
library(data.table)
library(ggplot2)
library(cali)
library(malariasimulation)
library(site)
library(vimcmalaria)
library(dplyr)

#read in model outptus from latest runs
# read in model outputs from previous runs
setwd('J:/VIMC/VIMC_malaria/')
completed_new<- completed_reports('process_country') |>
  filter(description == 'number_plug_paper') |>
 filter(scenario == 'no-vaccination') |>
  filter(parameter_draw == 0,
  iso3c== 'GNB') |>
  filter(date_time == max(date_time))

site_file_new<- readRDS(paste0('J:/VIMC/VIMC_malaria/archive/process_country/', completed_new$directory_name, '/site_file.rds'))
output_new<- readRDS(paste0('J:/VIMC/VIMC_malaria/archive/process_country/', completed_new$directory_name, '/outputs.rds'))
raw_new<- output_new$raw_output

# read in model outputs from previous runs
setwd('J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/')
completed_old<- completed_reports('process_country') |>
  filter(description == 'booster_update') |>
 filter(scenario == 'no-vaccination') |>
  filter(parameter_draw == 0,
  iso3c== 'GNB')

site_file_old<- readRDS(paste0('J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/archive/process_country/', completed_old$directory_name, '/site_file.rds'))
output_old<- readRDS(paste0('J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/archive/process_country/', completed_old$directory_name, '/outputs.rds'))
raw_old<- output_old$raw_output



#plot basic vars for old vs new
new<- raw_new |> filter(site_name == 'Oio' & urban_rural == 'rural') |> mutate(scenario= 'new')
old<- raw_old |> filter(site_name == 'Oio' & urban_rural == 'rural') |> mutate(scenario = 'old')

full<- bind_rows(new, old)

# the old runs equilibriate, the new runs elimininate
ggplot(full)+
  geom_line(aes(x= timestep/365, y= n_detect_pcr_730_3649/ n_age_730_3649, color = scenario )) +
  facet_wrap(~scenario) +
  labs(y= 'PFPR', x= 'timestep', title= 'PFPR in new model runs vs old') +
  ggthemes::theme_clean()

ggplot(full)+
  geom_line(aes(x= timestep/365, y= n_inc_clinical_730_1094/ n_age_730_1094, color = scenario )) +
  facet_wrap(~scenario) +
  labs(y= 'Incidence (730-1094 days)', x= 'year', title= 'Incidence (2-3 yr olds) in new model runs vs old') +
  ggthemes::theme_clean()


ggplot(raw_new) +
  geom_line(aes(x= timestep/365, y= n_detect_pcr_730_3649/ n_age_730_3649 )) +
facet_wrap(~site_name + ur) +
  ggthemes::theme_clean()+
  labs(title= 'No vaccination, all sites, GNB, 2000-2100', x= 'Year', y= 'PFPR')


ggpubr::ggarrange(p2, p3, p4)

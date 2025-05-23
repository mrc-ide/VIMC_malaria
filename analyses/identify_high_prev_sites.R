
### Identify high prevalence (>10%) sites:

library(foresite)
library(purrr)
library(dplyr)
library(ggplot2)

## get list of iso3cs
vimc_par<- read.csv('vimc_inputs/coverage_202310gavi-4_malaria-rts3-default.csv')
isos <-unique( vimc_par |>
                pull(country_code))

site_files<- map(isos, function(x) eval(parse(text=x)))

prev <- bind_rows(map(site_files, 'prevalence')) |>
  filter(year==2019)
pop <- bind_rows(map(site_files, 'population')) |>
  filter(year==2019)
dim(prev) # 926

high_prev <- filter(prev, pfpr>=0.1)
dim(high_prev)  ## 622

prev <- left_join(prev, pop, by=c('country', 'iso3c', 'name_1', 'urban_rural', 'year'))
prev <- prev |>
  mutate(run_model = ifelse(pfpr<=0.1, FALSE, TRUE))

saveRDS(prev, 'vimc_inputs/pfpr10plus_admins.rds')

model_par <- prev |>
  group_by(iso3c) |>
  mutate(country_pop=sum(pop)) |>
  ungroup() |>
  group_by(iso3c, country_pop) |>
  summarise(model_par=sum(par_pf[pfpr>=0.1])) |>
  mutate(model_proportion_risk = model_par/country_pop)
  


model_par <- left_join(model_par, vimc_par |> filter(year==2030) |> select(country_code, proportion_risk) |> rename(iso3c=country_code),
                  by=c('iso3c'))



ggplot(model_par, aes(x=proportion_risk, y=model_proportion_risk)) +
  geom_point() +
  theme_bw() +
  xlab('VIMC PAR') +
  ylab('model PAR') +
  expand_limits(x=0,y=0)


#### Malawi, Ethiopia, Sudan are too far off:
## Decide Malawi admin units
mwi <- prev |> filter(iso3c=='MWI')

## find admin units with PAR = VIMC
mwi_par <- model_par$proportion_risk[which(model_par$iso3c=='MWI')] # 20.5%
mwi <- mwi |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(pop) / sum(pop),
         run_model = ifelse(cumu_pop<round(mwi_par, digits=2), TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='MWI'] <- as.numeric(mwi |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))


## find admin units with PAR = VIMC
eth <- prev |> filter(iso3c=='ETH')
eth_par <- vimc_par$proportion_risk[which(vimc_par$country_code=='ETH' & vimc_par$year==2030)] 
eth <- eth |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(pop) / sum(pop),
         run_model = ifelse(cumu_pop<eth_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='ETH'] <- as.numeric(eth |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

## find admin units with PAR = VIMC
sdn <- prev |> filter(iso3c=='SDN')
sdn_par <- vimc_par$proportion_risk[which(vimc_par$country_code=='SDN' & vimc_par$year==2030)] 
sdn <- sdn |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(pop) / sum(pop),
         run_model = ifelse(cumu_pop<sdn_par+0.035, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='SDN'] <- as.numeric(sdn |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

saveRDS(model_par, 'vimc_inputs/par_scaling_vimc.rds')



prev <- prev |>
  filter(!(iso3c %in% c('ETH','SDN','MWI')))

prev <- bind_rows(prev, sdn, eth, mwi)
saveRDS(prev, 'vimc_inputs/pfpr10plus_admins.rds')



############# repeat for new merged site files:
filenames <- list.files('Y:/nas5_malaria/Lucy/VIMC/VIMC_malaria/site_files_merged')
filenames <-filenames[grep('_new_eir', filenames)]
site_files<- map(paste0('Y:/nas5_malaria/Lucy/VIMC/VIMC_malaria/site_files_merged/', filenames), readRDS)

prev <- bind_rows(map(site_files, 'prevalence')) |>
  filter(year==2019)
pop <- bind_rows(map(site_files, 'population')) |>
  filter(year==2019)
dim(prev) # 926

high_prev <- filter(prev, pfpr>=0.1)
dim(high_prev)  ## 622

prev <- left_join(prev, pop, by=c('country', 'iso3c', 'name_1', 'urban_rural', 'year'))
prev <- prev |>
  mutate(run_model = ifelse(pfpr<=0.1, FALSE, TRUE))

saveRDS(prev, 'vimc_inputs/pfpr10plus_admins_newsitefiles.rds')

model_par <- prev |>
  group_by(iso3c) |>
  mutate(country_pop=sum(pop)) |>
  ungroup() |>
  filter(pfpr>=0.1) |>
  group_by(iso3c, country_pop) |>
  summarise(model_par=sum(par_pf)) |>
  mutate(model_proportion_risk = model_par/country_pop)


model_par <- left_join(model_par, vimc_par |> filter(year==2030) |> select(country_code, proportion_risk) |> rename(iso3c=country_code),
                       by=c('iso3c'))

saveRDS(model_par, 'vimc_inputs/par_scaling_vimc_newsitefiles.rds')


ggplot(model_par, aes(x=proportion_risk, y=model_proportion_risk)) +
  geom_point() +
  theme_bw() +
  xlab('VIMC PAR') +
  ylab('model PAR') +
  expand_limits(x=0,y=0)

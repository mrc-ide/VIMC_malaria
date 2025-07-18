
### Identify high prevalence (>10%) sites:

library(purrr)
library(dplyr)
library(ggplot2)
library(site)
library(htmlwidgets)
## get list of iso3cs
vimc_par<- read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-rts3-rts4-default.csv')
iso3cs <-unique( vimc_par |>
                pull(country_code))

fetch_prevalence<- function(iso3c){
  site<- site::fetch_site(iso3c)
  prev<- site$prevalence |>
    filter(year == 2024)

  return(prev)
}
fetch_pops<- function(iso3c){
  site<- site::fetch_site(iso3c)
  pop<- site$population$population_total

  return(pop)
}
prev <- rbindlist(lapply(iso3cs, fetch_prevalence))
pop <- rbindlist(lapply(iso3cs, fetch_pops))
dim(prev) # 926

high_prev <- filter(prev, pfpr>=0.1)
dim(high_prev)  ## 622

prev <- left_join(prev, pop, by=c('country', 'iso3c', 'name_1', 'urban_rural', 'year'))
prev <- prev |>
  mutate(run_model = ifelse(pfpr<=0.1, FALSE, TRUE)) 
    # mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model)) |> #hardcoded exceptions
    #   mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model)) |>
    #   mutate(run_model = ifelse(name_1 %like% 'Bay', TRUE, run_model)) |>
    #   mutate(run_model = ifelse(name_1 %like% 'Nouakchott', TRUE, run_model)) |>
    #   mutate(run_model = ifelse(name_1 %like% 'Bolama', TRUE, run_model)) |>
    #   mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |>
    #   mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))
  
#saveRDS(prev, 'src/process_inputs/pfpr10plus_admins.rds')

model_par <- prev |>
  group_by(iso3c) |>
  mutate(country_pop=sum(pop)) |>
  ungroup() |>
  group_by(iso3c, country_pop) |>
  summarise(model_par=sum(par_pf[pfpr>=0.1])) |>
  mutate(model_proportion_risk = model_par/country_pop)
  


model_par <- left_join(model_par, vimc_par |> filter(year==2030) |> select(country_code, proportion_risk) |> rename(iso3c=country_code),
                  by=c('iso3c'))



p<- ggplot(model_par, aes(x=proportion_risk, y=model_proportion_risk, color= iso3c)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('VIMC PAR') +
  ylab('model PAR') +
  expand_limits(x=0,y=0)

saveWidget(ggplotly(p), file = "vimc_pars.html")
ggplotly(p)

#### Malawi, Ethiopia, Sudan are too far off:
## Ethiopia, Somalia, MRT, KEN, GNB,SDN


## find admin units with PAR = VIMC
eth <- prev |> filter(iso3c=='ETH')
eth_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='ETH' & vimc_par$year==2030)])
eth <- eth |>
  mutate(run_model= ifelse(name_1 == 'Gambela Peoples', TRUE, run_model)) |> #change this to Gambela 
  arrange(desc(run_model)) |>
  mutate(prop = par/ sum(par),
         cumu_pop = cumsum(par) / sum(par)) 

model_par$model_proportion_risk[model_par$iso3c=='ETH'] <- as.numeric(eth |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

## find admin units with PAR = VIMC
som <- prev |> filter(iso3c=='SOM')
som_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='SOM' & vimc_par$year==2030)])
som <- som |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
         run_model = ifelse(cumu_pop<som_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='SOM'] <- as.numeric(som |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

#guinea bissau
gnb <- prev |> filter(iso3c=='GNB')
gnb_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='GNB' & vimc_par$year==2030)])
gnb <- gnb |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
         run_model = ifelse(cumu_pop<gnb_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='GNB'] <- as.numeric(gnb |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

# kenya
ken <- prev |> filter(iso3c=='KEN')
ken_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='KEN' & vimc_par$year==2030)])
ken <- ken |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
         run_model = ifelse(cumu_pop<ken_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='KEN'] <- as.numeric(ken |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))


#mauritania
mrt <- prev |> filter(iso3c=='MRT')
mrt_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='MRT' & vimc_par$year==2030)])
mrt <- mrt |>
  mutate(run_model= ifelse(name_1 == 'Nouakchott', TRUE, run_model)) |> #hardcoding one more exception bc otherwise par is much too low
  arrange(desc(run_model)) |>
  mutate(cumu_pop = cumsum(par) / sum(par))) 

model_par$model_proportion_risk[model_par$iso3c=='MRT'] <- as.numeric(mrt |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))


# madagascar
mdg <- prev |> filter(iso3c=='MDG')
mdg_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='MDG' & vimc_par$year==2030)])
mdg <- mdg |>
  mutate(run_model= ifelse(name_1 == 'Toamasina' & urban_rural== 'urban', TRUE, FALSE)) |>
  arrange(desc(run_model)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
prop = par/sum(par)) 

model_par$model_proportion_risk[model_par$iso3c=='MDG'] <- as.numeric(mdg |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))


tza <- prev |> filter(iso3c=='TZA')
tza_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='TZA' & vimc_par$year==2030)])
tza <- tza |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
         run_model = ifelse(cumu_pop<tza_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='TZA'] <- as.numeric(tza |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))


sdn <- prev |> filter(iso3c=='SDN')
sdn_par <- unique(vimc_par$proportion_risk[which(vimc_par$country_code=='SDN' & vimc_par$year==2030)])
sdn <- sdn |>
  arrange(desc(pfpr)) |>
  mutate(cumu_pop = cumsum(par) / sum(par),
         run_model = ifelse(cumu_pop<sdn_par, TRUE, FALSE)) 

model_par$model_proportion_risk[model_par$iso3c=='SDN'] <- as.numeric(sdn |>
  filter(run_model==TRUE) |>
  summarise(cumu_pop=max(cumu_pop)))

saveRDS(model_par, 'analyses/par_scaling_vimc.rds')



prev <- prev |>
  filter(!(iso3c %in% c('ETH','SDN','TZA', 'MDG', 'SOM', 'MRT', 'GNB')))

prev <- bind_rows(prev, eth, sdn, tza, mdg, som, mrt, gnb)
prev<- prev |> select(-cumu_pop)
saveRDS(prev, 'analyses/pfpr10plus_admins.rds')


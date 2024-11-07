###cases averted per FVP diagnostic
library(malariasimulation)
library(ggplot2)
library(dplyr)
library(data.table)


doses<- readRDS('doses_test.rds')

saveRDS(raw, 'raw_kenya_outputs.rds')
raw<-  readRDS('raw_kenya_outputs.rds')

# filter to one site to get sense of dosage rate
output<- raw |> filter(site_name == 'Bungoma' & scenario == 'malaria-r3-r4-default')

output$year <- floor(output$timestep / 365) + 2000
output<- data.table(output)

View(output[is.na(n_pev_epi_booster_1)])
doses_per_year <-output |>
  dplyr::group_by(.data$year) |>
  dplyr::summarise(n_model=mean(n_age_365_729),    ## average number of people in the eligible age grp (?best way to do this)
                   doses_model=sum(n_pev_epi_booster_1)) |>
  mutate(rate_dosing = .data$doses_model/.data$n_model)







# Plot dose timing (look fine? try sud_rural)
plot_doses <- function(output){
  yr<- 365
  cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  output$yr <- ceiling(output$timestep / yr)
  doses <- output |> 
    select(n_pev_epi_booster_1,
           n_pev_epi_dose_1,
          n_pev_epi_dose_2,
        n_pev_epi_dose_3,
        yr)
  
  doses<- doses |>
    group_by(yr) |>
    summarise(n_pev_epi_dose_1= sum(n_pev_epi_dose_1, na.rm = TRUE), 
              n_pev_epi_dose_2= sum(n_pev_epi_dose_2, na.rm = TRUE),
              n_pev_epi_dose_3= sum(n_pev_epi_dose_3, na.rm = TRUE),
              n_pev_epi_booster_1= sum(n_pev_epi_booster_1, na.rm = TRUE)) |>
    data.table()
  
 doses_long<- melt(doses, id.vars = 'yr', value.name = 'dose_count') 
  
  
 p<-  ggplot(doses_long, aes(x= yr, y= dose_count, fill= variable))+
    geom_col(position= 'stack')+
    scale_fill_manual(values= cols)


  return(p)

}

raw<- data.table(raw)


#rhe
site_doses<- site_doses |>
  mutate(val = cases_averted/fvp)

averted<- data.table(averted)
ggplot(data= averted[site_ur== 'Centre_rural'], aes(x= year, y= cases_averted, group = site))+
  geom_line() +
  facet_wrap(~site_ur)

ggplot(data = dose_output, mapping = aes(x= pfpr, y = (cases_averted/fvp) *100000, color = scenario, shape = site_ur)) +
  geom_point()+
  plotting_theme +
  theme(legend.position = 'none')+
  labs(title = 'Unscaled cases averted per 100,000 FVP, 15 yrs after introduction',
       x= 'PFPR(2-10) in 2019',
       y = 'Cases averted per 100k FVPs')



output<- raw |> filter(site_ur == 'Bungoma_both')
output<- data.table(output)

plot_doses(subset)

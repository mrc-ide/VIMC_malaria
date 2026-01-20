# recalibrate GNB for paper
# title   ethiopia recalibration
# author  Lydia Haile
# purpose recalibrate sites in Ethiopia to match MAP prevalence after turning IRS off

library(vimcmalaria)
library(cali)
library(malariasimulation)
library(dplyr)
library(data.table)
library(tidyr)
library(site)
library(data.table)
library(dplyr)
library(scene)
library(malariasimulation)
library(tidyr)
library(tibble)
library(postie)
library(countrycode)
library(vimcmalaria)
library(ggplot2)
# read inputs for guinea-bissau
# try one to start

site_data<- site::fetch_site('GNB', urban_rural= TRUE)
input<- readRDS('J:/VIMC/VIMC_malaria/analyses/recalibrations/original_inputOio_rural.rds')
site_name<- 'Oio'
ur<- 'rural'
yr<- 2022

input$param_list$smc_coverages
  
recalibrate<- function(input, yr){
  
  
  summary_mean_pfpr_2_10 <- function (x) {
    message('calibrating')
    x<- data.table(x)
    x<- drop_burnin(x, burnin= burnin* 365)

    # Calculate the PfPR2-10:
    prev_2_10 <- mean(x[timestep %in% c(((yr-2000)*365):((yr+1-2000)*365))]$n_detect_pcr_730_3649/x[timestep %in% c(((yr-2000)*365):((yr+1-2000)* 365))]$n_age_730_3649) # average over a year 
    
    # Return the calculated PfPR2-10:
    return(prev_2_10)
  }
  
  

  # pull target pfpr from corresponding yr for corresponding site
  target_pfpr <- site_data$prevalence |> filter(year == yr, name_1 == input$site_name, urban_rural== input$ur) |> pull(pfpr)
  
  print(paste0('target pfpr ', target_pfpr ))
  
  # Add a parameter to the parameter list specifying the number of timesteps 
  simparams<- input$param_list
  simparams$timesteps <- (yr-2000+5 + burnin) * 365
  burnin<- 15
  simparams$burnin<- 15
  simparams$human_population<- 20000 # make the population very small to start
  simparams$progress_bar<- TRUE
  # Establish a tolerance value:
  pfpr_tolerance <- 0.01
  
  # Set upper and lower EIR bounds for the calibrate function to check
  lower_EIR <- 1; upper_EIR <- 50
  
  #output<- run_simulation(timesteps = simparams$timesteps, parameters = simparams)


# plot this against parasite prevalence to see how ther original calibration looks

  # Run the calibrate() function:
  cali_EIR <- calibrate(target = target_pfpr,
                        summary_function = summary_mean_pfpr_2_10,
                        parameters = simparams,
                        tolerance = pfpr_tolerance, 
                        low = lower_EIR, high = upper_EIR)
  
  print(paste0('calibrated EIR for site ', input$site_name, ' :', cali_EIR))
  simparams<- set_equilibrium(simparams, init_EIR = cali_EIR)

  return(list('cali_eir'= cali_EIR, 'params'= simparams))
}


calibrated_params<- recalibrate(model_input, yr= 2022)

model_input$param_list$human_population<- 100000
output<- run_simulation(timesteps = model_input$param_list$timesteps, parameters = model_input$param_list)
raw_output<- drop_burnin(output, burnin= unique(model_input$param_list$burnin)* 365)


output<- run_simulation(timesteps = calibrated_params$param_list$timesteps, parameters = calibrated_params$param_list)
raw_output<- drop_burnin(output, burnin= unique(calibated_params$param_list$burnin)* 365)

cols <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7","#F0E442", "#0072B2", "#D55E00")
# pull map prevalence 
map_prev<- site_data$prevalence
map_prev<- map_prev |>
  mutate(site_ur = paste0(name_1, '_', urban_rural)) |>
  data.table()

# plot map prevalence by site
ggplot()+
  geom_line(data= map_prev, mapping = aes(x= year, y = pfpr), color = '56B4E9') +
  facet_wrap(~site_ur) 

plot<- raw_output |>
  mutate(year = as.integer(timestep/365)+ 1999) |>
  group_by(year) |>
  summarise(pfpr = mean(n_detect_pcr_730_3649 /n_age_730_3649),
            .groups = 'keep')

# plot modelled prevalence by site
ggplot()+
  geom_line(data= plot, mapping = aes(x= year, y = pfpr), color = 'darkred') +
  geom_line(data= map_prev[site_ur == 'Oio_rural'], mapping = aes(x= year, y = pfpr), color = 'darkblue') +
  facet_wrap(~site_ur) +
  #plotting_theme +
  #scale_fill_manual(values = cols) +
  labs(title = 'MAP prevalence vs modelled prevalence, Oio, Guinea-Bissau, post calibration',
subtitle= 'Red is modelled (malariasimulation), blue is data (Malaria Atlas Project)')

ggplot()+
  geom_line(data= output, mapping = aes(x= timestep, y = n_detect_pcr_730_3649 /n_age_730_3649), color = 'darkred') +
  #geom_line(data= map_prev[site_ur == 'Oio_rural'], mapping = aes(x= year, y = pfpr), color = 'darkblue') +
  #facet_wrap(~site_ur) +
  #plotting_theme +
  #scale_fill_manual(values = cols) +
  labs(title = 'MAP prevalence vs modelled prevalence, Oio, Guinea-Bissau, post calibration',
subtitle= 'Red is modelled (malariasimulation), blue is data (Malaria Atlas Project)')


ggplot()+
  geom_line(data= output, mapping = aes(x= timestep, y= n_infections))



ggplot()+
  geom_line(data= x, mapping = aes(x= timestep, y= n_age_730_3649))



prev_2_10 <- mean(x[timestep %in% c(((yr-2000)*365):((yr+1-2000)*365))]$n_detect_pcr_730_3649/x[timestep %in% c(((yr-2000)*365):((yr+1-2000)* 365))]$n_age_730_3649) # average over a year 

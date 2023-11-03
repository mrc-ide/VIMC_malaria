# diagnostics for a single site  --------------------------------------------

library(ggplot2)
library(wesanderson)
library(ggforce)
library(ggpubr)
library(extrafont)

plotting_theme<- theme_bw(base_size = 12) +
  theme( legend.position = 'bottom',
         strip.text.x = element_text(size = rel(0.8)),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1), 
         #axis.text.y = element_blank(),
         text = element_text(family= 'Arial Narrow'),
         axis.ticks.y= element_blank(), 
         panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

population_diagnostic_model<- function(dt, pg, intro_yr){
  
  p<-   ggplot(data= dt, mapping = aes(x= year, y= cohort_size, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5)  +
    facet_wrap_paginate(~age, ncol= 5, nrow= 5, page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', 
         y= 'Population',
         title= paste0('Population over time: site ', unique(dt$site_name), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
  
}

incident_cases_diagnostic<- function(dt, pg, intro_yr){
  
  p<-   ggplot(data= dt, mapping = aes(x= year, y= cases, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5)  +
    facet_wrap_paginate(~age, scales= 'free',
                        ncol= 5, nrow= 5, 
                        page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'Clinical cases', 
         title= paste0('Incident clinical cases over time: site ', unique(dt$site_name), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
  
}

incidence_rate_diagnostic<- function(dt, pg, intro_yr){
  
  p<-  ggplot(data= dt, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5)  +
    facet_wrap_paginate(~age,  nrow= 5, ncol= 5, page = pg) +
    labs(x= 'Time (in years)', 
         y= 'Incidence rate', 
         title= paste0('Incidence rate over time: ', unique(dt$site_name), ', ', description),
         color= 'Scenario', 
         fill= 'Scenario') +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  +
    plotting_theme
  
  return(p)
}



mortality_diagnostic<- function(dt, pg, intro_yr){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= deaths, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5)  +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    facet_wrap_paginate(~age, scales = 'free', ncol= 5, nrow= 5, page = pg) +
    labs(x= 'Time (in years)', 
         y= 'Deaths', 
         title= paste0('Deaths over time: ', unique(dt$site_name), ', ', description),
         color= 'Scenario', 
         fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
  
}
mortality_rate_diagnostic<- function(dt, pg, intro_yr){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= mortality, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5) +
    geom_vline(xintercept= 2023, linetype= "dotted") +
    facet_wrap_paginate(~age, ncol= 5, nrow= 5, page= pg) +
    labs(x= 'Time (in years)',
         y= 'Mortality rate', 
         title= paste0('Mortality rate over time: ', unique(dt$site_name)),
         color= 'Scenario', 
         fill= 'Scenario') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
  
}


daly_diagnostic<- function(dt, pg, intro_yr){
  
  p<- ggplot(data= dt, mapping = aes(x= year, y= dalys, color= scenario, fill= scenario))+
    geom_point(alpha= 0.5)  +
    facet_wrap_paginate(~age,scales = 'free', ncol= 5, nrow= 5, page = pg) +
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(x= 'Time (in years)', y= 'DALYs', title= paste0('DALYs over time: ', unique(dt$site_name), ', ', description),
         color= 'Scenario', fill= 'Scenario') +
    theme_minimal()+
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
}

#' Plot model PFPR against prevalence 
#'
#' @param   dt               raw model output
#' @returns plot of PFPR2-10 against prevalence data for site
plot_model_against_prevalence<- function(dt, intro_yr){
  
  require(data.table)
  
  site_data<- foresite::get_site(unique(dt$country))
  
  site_name<- unique(dt$site_name)
  ur<- unique(dt$urban_rural)
  
  prevalence<- as.data.table(site_data$prevalence)
  
  prevalence<- prevalence[name_1== site_name & urban_rural== ur]
  
  dt$prevalence <- dt$n_detect_730_3649 / dt$n_730_3649
  
  
  # Set the time
  dt$year <- (dt$timestep / 365) + 2000
  
  p<- ggplot()+
    geom_line(data= dt[year < 2021], mapping= aes(x= year, y= prevalence), linewidth= 0.25)+
    geom_point(data= prevalence, mapping= aes(x= year, y= pfpr), size= 2, pch= 19)+
    geom_vline(xintercept= intro_yr, linetype= "dotted") +
    labs(title= 'Model output against parasite prevalence',
         y= 'PfPR',
         x= 'Year') +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2)) +
    plotting_theme
  
  return(p)
}


plot_cases_averted<- function(dt){
  
  
  site_name<- unique(dt$site_name)
  ur<- unique(dt$urban_rural)
  
  dt<- dt |> subset(year >= 2035)
  intvn<- dt|> subset(scenario == 'intervention')
  intvn<- intvn |> 
    rename(cases_intvn = cases,
           dalys_intvn = dalys,
           deaths_intvn = deaths)
  
  bl<- dt|> subset(scenario== 'baseline')
  
  merged<- merge(intvn, bl, by= c('year', 'age'))
  
  merged<- merged |>
    mutate(cases_averted = cases - cases_intvn,
           deaths_averted = deaths - deaths_intvn,
           dalys_averted = dalys - dalys_intvn)
  
  merged<- data.table(merged)
  merged[, cases_averted := sum(cases_averted), by= 'age']
  merged<- unique(merged, by = 'age')
  
  ggplot(data= merged, mapping= aes(x= age, y= cases_averted))+
    geom_col() +
    plotting_theme +
    labs(title= paste0('Cases averted by age for site: ', site_name, ' ', ur),
         x= 'Age (years',
         y= 'Cases averted, 2035-2050')
}



plot_pfpr_over_time<- function(model_output, intro_yr){
  
  site_name<- unique(model_output$site_name)
  ur<- unique(model_output$urban_rural)
  model_output<- model_output|> subset(scenario!= TRUE)
  model_output$pfpr<- model_output$n_detect_730_3649/ model_output$n_730_3649
  
  ggplot(data= model_output, mapping = aes(y= pfpr, x= timestep/365, color= scenario))+
    geom_line()+
    geom_vline(xintercept= intro_yr-2000, linetype= "dotted") +
    labs(x= 'Time (years)', y= 'PFPR (2-10), proportion',
         title= paste0('PFPR over time: ', site_name, ' ', ur))+
    plotting_theme +
    scale_color_manual(values= wes_palette('Royal2', n= 2)) +
    scale_fill_manual(values= wes_palette('Royal2', n= 2))  
  
}



# Plot dose timing
plot_doses_delivered <- function(output){
  # Set colour palette:
  cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  yr<- 365
  
  #output<- output|> subset(timestep <= 25*365)
  output$year <- ceiling(output$timestep / yr)
  doses <- output[, c(grep("n_pev" , names(output)), grep("year", names(output)))]
  doses <- aggregate(cbind(doses[1:4]),
                     by = list(doses$year), 
                     FUN = sum)
  doses <- as.matrix(t(doses[, -1]))
  
  barplot(doses, xlab = "Year",
          ylab = "Number of doses",
          col = cols[1:6], space = 0, axes = T,
          beside = FALSE, xaxs = "i", yaxs = "i",
          ylim = c(0, max(colSums(doses))*1.1))
  grid(lty = 2, col = "grey80", lwd = 0.5);box()
  axis(side = 1, lty = 1, col = "black", pos = 0)
  legend("topleft", box.lty = 0, legend = c("Dose 1","Dose 2","Dose 3","Booster 1"),
         fill = cols[1:6], bg="transparent", cex = 0.8, y.intersp = 1.5)
}




demography_plot<- function(dem_output){
  
  # Subset the final day of the simulation for each of the two demography runs:
  dem_output <- dem_output[dem_output$timestep == 49 * 365,]
  
  age_min = seq(0, 99, by= 1) * year
  age_max = seq(1, 100, by= 1) * year -1
  # Extract the age variables and convert the dataframe to long format:
  convert_to_long <- function(age_min, age_max, output) {
    output <- lapply(
      seq_along(age_min),
      function(i) {
        data.frame(
          age_lower = age_min[[i]],
          age_upper = age_max[[i]],
          n = output[,paste0('n_age_', age_min[[i]], '_',age_max[[i]])],
          age_plot = age_min[[i]]/365,
          timestep = output$timestep)
      }
    )
    output <- do.call("rbind", output)
  }
  
  # Convert the output for plotting:
  dem_output <- convert_to_long(age_min, age_max, dem_output)
  
  # Define the plotting window
  par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
  
  # a) Default/Exponentially-distributed demography
  plot.new(); grid(lty = 2, col = "grey80", lwd = 0.5, ny = 5, nx = 6); par(new = TRUE)
  barplot(height = dem_output[dem_output$run == "exponential", c("n", "age_plot")]$n,
          names = c(paste0(seq(0,75, by = 5),"-",seq(0,75, by = 5)+4), "80+"),
          axes = TRUE, space = 0, ylim = c(0, 250), xaxs = "i", yaxs = "i",
          main = "Default", xlab = "Age Group", ylab = "Individuals",
          cex.axis = 0.8, cex.names = 0.8, cex.lab = 1, cex.main = 1, las = 2,
          col = cols[2]); box()
  
  # b) Custom demography
  plot.new()
  grid(lty = 2, col = "grey80", lwd = 0.5, ny = 5, nx = 6)
  par(new = TRUE)
  barplot(height = dem_output[dem_output$run == "custom", c("n", "age_plot")]$n,
          names = c(paste0(seq(0,75, by = 5),"-",seq(0,75, by = 5)+4), "80+"),
          axes = TRUE, space = 0, ylim = c(0, 250), xaxs = "i", yaxs = "i",
          main = "Custom", xlab = "Age Group",
          cex.axis = 0.8, cex.names = 0.8, cex.lab = 1, cex.main = 1, las = 2,
          col = cols[1]); box()
}




# site file diagnostics --------------------------------------------------------

library(scales)
library(ggpubr)
library(wesanderson)
library(tidyr)





# plot cases + deaths

# cases and deaths  ------------------------------------------------------------
cases_deaths_diagnostic<- function(site){
  
  dt<- data.table(site$cases_deaths)
  
  site_name<- site$sites$name_1
  ur<- site$sites$urban_rural
  
  lower <-
    melt(dt,
         measure.vars = c('wmr_cases_l', 'wmr_deaths_l', 'incidence_l', 'mortality_l'))  |>
    select(year, variable, value) |>
    rename(lower = value)
  
  upper <-
    melt(dt,
         measure.vars = c('wmr_cases_u', 'wmr_deaths_u', 'incidence_u', 'mortality_u')) |>
    select(year, variable, value) |>
    rename(upper = value)
  
  median <-
    melt(dt,
         measure.vars = c('wmr_cases', 'wmr_deaths', 'incidence', 'mortality')) |>
    select(year, variable, value)
  
  lower[variable %like% 'cases', variable := 'cases']
  lower[variable %like% 'deaths', variable := 'deaths']
  lower[variable %like% 'mortality', variable := 'mortality']
  lower[variable %like% 'incidence', variable := 'incidence']
  
  upper[variable %like% 'cases', variable := 'cases']
  upper[variable %like% 'deaths', variable := 'deaths']
  upper[variable %like% 'mortality', variable := 'mortality']
  upper[variable %like% 'incidence', variable := 'incidence']
  
  median[variable %like% 'cases', variable := 'cases']
  median[variable %like% 'deaths', variable := 'deaths']
  median[variable %like% 'mortality', variable := 'mortality']
  median[variable %like% 'incidence', variable := 'incidence']
  
  plot <- Reduce(merge, list(lower, median, upper))
  
  p<- ggplot(data = plot) +
    geom_line(mapping = aes(x = year, y = value, color = variable)) +
    geom_ribbon(
      mapping = aes(
        x = year,
        ymin = lower,
        ymax = upper,
        fill = variable
      ),
      alpha = 0.2
    ) +
    facet_wrap( ~ variable, ncol = 4) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = wes_palette('Royal2', n = 4)) +
    scale_fill_manual(values = wes_palette('Royal2', n = 4))  +
    plotting_theme +
    labs(
      y = 'Value',
      x = 'Year',
      title = 'Cases, deaths, mortality, and incidence over time',
      subtitle = paste('Site: ', site_name, ' ', ur)
    )
  
  return(p)
}


# prevalence -------------------------------------------------------------------
prevalence_diagnostic<- function(site){
  
  dt<- melt(data.table(site$prevalence), measure.vars= c('pfpr', 'pvpr'))
  
  site_name<- site$sites$name_1
  ur<- site$sites$urban_rural
  
  p<- ggplot(dt, mapping = aes(x= year, y= value, color= variable))+
    geom_line()+
    plotting_theme+
    labs(x= 'Year', y= 'Prevalence value', color= 'Prevalence type',
         title= 'Parasite prevalence over time',
         subtitle = paste0('Site: ', site_name, ' ', ur))
  
  print(p)
}




# interventions ----------------------------------------------------------------
# plot_interventions_combined(
#   interventions = site$interventions,
#   population = site$population,
#   group_var = c("country", "name_1"),
#   include = c("itn_use", "itn_input_dist", "tx_cov", "smc_cov", "pmc_cov"),
#   labels = c("ITN usage", "ITN model input", "Treatment","SMC", "PMC")
# )



# population  ------------------------------------------------------------------

population_diagnostic<- function(site){
  
  dt<- melt(site$population, measure.vars = c('par_pf', 'par_pv'))
  
  p1<- ggplot(data=dt)+
    geom_col(position = 'stack', mapping= aes(x= year, y= value, fill= variable)) +
    scale_fill_manual(values = wes_palette('Royal2', n = 2))  +
    geom_line(mapping= aes(x= year, y=par)) +
    plotting_theme+
    labs(title= 'Population at risk over time', 
         subtitle= paste0('Site: ', site_name, ' ', ur),
         y= 'Population', x= 'Year',
         fill= 'Population at risk') +
    scale_y_continuous(labels= comma)
  
  
  dt<- melt(site$population, measure.vars = c('pop', 'par'))
  
  p2<- ggplot(data=dt)+
    geom_line(mapping= aes(x= year, y= value, color= variable)) +
    plotting_theme+
    labs(title= 'Population over time', 
         subtitle= paste0('Site: ', site_name, ' ', ur),
         y= 'Population', x= 'Year') +
    scale_y_continuous(labels= comma)
  
  plots<- ggarrange(p1, p2, ncol= 2)
  
  return(plots)         
}

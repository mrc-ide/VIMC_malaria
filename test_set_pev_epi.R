# set_pev_epi_testing
library(malariasimulation)

# plotting functions  ----------------------------------------------------------
# Set colour palette:
cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_doses <- function(){
  output$month <- ceiling(output$timestep / month)
  doses <- output[, c(grep("n_pev" , names(output)), grep("month", names(output)))]
  doses <- aggregate(cbind(doses[1:4]),
                     by = list(doses$month), 
                     FUN = sum)
  doses <- as.matrix(t(doses[, -1]))
  
  barplot(doses, xlab = "Month",
          ylab = "Number of doses",
          col = cols[1:6], space = 0, axes = T,
          beside = FALSE, xaxs = "i", yaxs = "i",
          ylim = c(0, 230))
  grid(lty = 2, col = "grey80", lwd = 0.5);box()
  axis(side = 1, lty = 1, col = "black", pos = 0)
  legend("topleft", box.lty = 0, legend = c("Dose 1","Dose 2","Dose 3","Booster 1"),
         fill = cols[1:6], bg="transparent", cex = 0.8, y.intersp = 1.5)
}


plot_doses_multiple_boosters <- function(){
  output$month <- ceiling(output$timestep / month)
  doses <- output[, c(grep("n_pev" , names(output)), grep("month", names(output)))]
  doses <- aggregate(cbind(doses[1:5]),
                     by = list(doses$month), 
                     FUN = sum)
  doses <- as.matrix(t(doses[, -1]))
  
  barplot(doses, xlab = "Month",
          ylab = "Number of doses",
          col = cols[1:6], space = 0, axes = T,
          beside = FALSE, xaxs = "i", yaxs = "i",
          ylim = c(0, 230))
  grid(lty = 2, col = "grey80", lwd = 0.5);box()
  axis(side = 1, lty = 1, col = "black", pos = 0)
  legend("topleft", box.lty = 0, legend = c("Dose 1","Dose 2","Dose 3","Booster 1", "Booster 2"),
         fill = cols[1:6], bg="transparent", cex = 0.8, y.intersp = 1.5)
}

# initial vaccine scenario  ----------------------------------------------------
year <- 365
month <- 30
sim_length <- 3 * year
human_population <- 10000
starting_EIR <- 20

simparams <- get_parameters(list(
  human_population = human_population,
  clinical_incidence_rendering_min_ages = 0,
  clinical_incidence_rendering_max_ages = 5 * year,
  individual_mosquitoes = FALSE
)
)

simparams <- set_equilibrium(parameters = simparams, init_EIR = starting_EIR)

static_booster_coverage <- set_pev_epi(
  simparams,
  profile = rtss_profile, # We will model implementation of the RTSS vaccine.
  timesteps = 1 * year, # Vaccination will begin at 1 year into the simulation.
  coverages = 1, # Vaccine coverage is 100%.
  min_wait = 0, # There is no minimum wait since the last vaccination.
  age = 5 * month, # Individuals will be vaccinated once they reach 5 months of age.
  booster_timestep = 12 * month, # The booster is administered 12 months following the third dose. 
  booster_coverage = 0.95, # 95% of those vaccinated with the primary series will be boosted.
  booster_profile = list(rtss_booster_profile) # We will model implementation of the RTSS booster.
)

output <- run_simulation(timesteps = sim_length * 2, parameters = static_booster_coverage)
plot_doses()

# try changing booster coverage at a specified timestep ------------------------
changed_coverage <- set_pev_epi(
  static_booster_coverage,
  profile = rtss_profile, 
  timesteps = 1 * year,  
  coverages = 1,         
  min_wait = 0,          
  age = 5 * month,        
  booster_timestep =  48 * month, #change booster coverage to 20% at 4 years
  booster_coverage = 0.20, 
  booster_profile = list(rtss_booster_profile)
)


output <- run_simulation(timesteps = sim_length * 2, parameters = changed_coverage)
plot_doses()


# multiple boosters  -----------------------------------------------------------
rtssepiparams2 <- set_pev_epi(
  simparams,
  profile = rtss_profile, 
  timesteps = 1 * year, 
  coverages = 1, 
  age = 5 * month, 
  min_wait = 0, 
  booster_timestep = c(12 * month, 24 * month), # Here, we are testing a strategy with 2 boosters, one at 1 year after the 3rd dose and the second 2 years after the 3rd dose.
  booster_coverage = c(1, 1), # For each of the two boosters, coverage is 100%.
  booster_profile = list(rtss_booster_profile, rtss_booster_profile) 
)

output <- run_simulation(timesteps = sim_length * 2, parameters = rtssepiparams2)
plot_doses_multiple_boosters()

# add changing coverage
rtssepiparams2 <- set_pev_epi(
  simparams,
  profile = rtss_profile, 
  timesteps = 1 * year, 
  coverages = 1, 
  age = 5 * month, 
  min_wait = 0, 
  booster_timestep = c(12 * month), # Here, we are testing a strategy with 2 boosters, one at 1 year after the 3rd dose and the second 2 years after the 3rd dose.
  booster_coverage = c(1), # For each of the two boosters, coverage is 100%.
  booster_timed_coverage = c(1, 1, 0.10, 0.10),
  booster_timed_coverage_timestep = c(1, 2, 3, 4) * year,
  booster_profile = list(rtss_booster_profile) 
)

output <- run_simulation(timesteps = sim_length * 2, parameters = rtssepiparams2)
plot_doses()


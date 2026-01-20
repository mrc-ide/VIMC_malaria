
# demography test
# test a vaccine run with no change in demography to see the impact
# pull inputs for a moderate to high transmission admin unit 

library(vimcmalaria)
library(dplyr)
library(ggplot2)
library(site)
library(tidyr)

# pull site file
iso3c<- 'COD'
site<- fetch_site('COD')


# pull proxy coverage data for COD
completed_new<- completed_reports('process_country') |>
  filter(description == 'number_plug_paper') |>
 filter(scenario == 'no-vaccination') |>
  filter(parameter_draw == 0,
  iso3c== 'COD') |>
  filter(date_time == max(date_time))


vimc_input<- readRDS(paste0('J:/VIMC/VIMC_malaria/archive/process_country/', completed_new$directory_name, '/vimc_input.rds'))

coverage_data<- vimc_input$coverage_input |> filter(iso3c == 'COD')

# what sites are available in COD?
site$sites

# they're all high transmission so just pick the first one 
site <- site::subset_site(
  site = site,
  site_filter = data.frame(
    country= 'Democratic Republic of the Congo',
    iso3c = 'COD',
    name_1 = 'Bas-Uele'
  ))

# look at the demography for this site
site$demography

  run_params<- pull_age_groups_time_horizon(quick_run= FALSE)

  site<- expand_intervention_coverage(site,
                                      terminal_year = 2025)
  site<- update_coverage_values(site,
                    iso3c = iso3c,
                    coverage_data,
                     scenario_name = 'proxy')

  # check the site has a non-zero EIR
  check_eir(site)

  # pull parameters for this site ------------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors$vector_species,
    seasonality = site$seasonality$seasonality_parameters,
    eir = site$eir$eir[1],
    burnin = 10,
    overrides = list(human_population = 25000)
  )


  # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages
  params$progress_bar <- TRUE

  set.seed(56)

  model <- retry::retry(
    malariasimulation::run_simulation(timesteps = params$timesteps,
                                      parameters = params),
    max_tries = 5,
    when = 'error reading from connection|embedded nul|unknown type',
    interval = 3
  )

# run the exact same model with fixed demography
params_fixed<- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors$vector_species,
    seasonality = site$seasonality$seasonality_parameters,
    eir = site$eir$eir[1],
    burnin = 10,
    overrides = list(human_population = 25000)
  )
  
# set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages
  params$progress_bar <- TRUE

  set.seed(56)



fix_demography<- function(site, params){

  demo <- site$demography[site$demography$year == '2000',] #fixing demography to what occurred in 2000
  ages <- round(unique(demo$age_upper) * 365)
  timesteps <- 365 * (unique(demo$year) - 2000)
  deathrates <- demo$adjusted_mortality_rates / 365
  deathrates_matrix <- matrix(deathrates, nrow = length(timesteps), byrow = TRUE)

  # Add parameters
  params_fixed <- malariasimulation::set_demography(
    parameters = params,
    agegroups = ages,
    timesteps = timesteps,
    deathrates = deathrates_matrix
  )
  
  return(params_fixed)
}



  model_fixed <- retry::retry(
    malariasimulation::run_simulation(timesteps = params_fixed$timesteps,
                                      parameters = params_fixed),
    max_tries = 5,
    when = 'error reading from connection|embedded nul|unknown type',
    interval = 3
  )



  raw_output<- postie::drop_burnin(model_fixed, burnin= 10* 365)

  output <- postie::get_rates(
    raw_output,
    time_divisor = 365,
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )

# --- 4. Compare first and last timesteps ---
age_comp <- output |>
  filter(t %in% c(2000, 2025)) %>%
  mutate(period = ifelse(t == 2000, "Start", "End"))

# --- 5. Plot population age distribution over time ---
ggplot(age_comp, aes(x = age_lower + (age_upper - age_lower)/2,
                     y = n / sum(n),
                     fill = period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  labs(title = "Age distribution stability (Bas-Uélé, fixed demography)",
       x = "Age (years)", y = "Proportion of population") +
  scale_fill_manual(values = c("Start" = "firebrick", "End" = "steelblue")) +
  theme_minimal()



  raw_output_orig<- postie::drop_burnin(model, burnin= 10* 365)

  output_orig <- postie::get_rates(
    raw_output_orig,
    time_divisor = 365,
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )

# --- 4. Compare first and last timesteps ---
age_comp <- output_orig |>
  filter(t %in% c(2000, 2025)) %>%
  mutate(period = ifelse(t == 2000, "Start", "End"))

# --- 5. Plot population age distribution over time ---
ggplot(age_comp, aes(x = age_lower + (age_upper - age_lower)/2,
                     y = n / sum(n),
                     fill = period)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  labs(title = "Age distribution stability (Bas-Uélé, fixed demography)",
       x = "Age (years)", y = "Proportion of population") +
  scale_fill_manual(values = c("Start" = "firebrick", "End" = "steelblue")) +
  theme_minimal()




# ran test models on fixed demography for congo
# pull the raw model outputs

# pull proxy coverage data for COD
completed_new<- completed_reports('p') |>
  filter(description == 'fixed_demography') |>
  filter(parameter_draw == 0,
  iso3c== 'COD')

original_run<- completed_reports('postprocessing') |>
  filter(description == 'number_plug_paper') |>
  filter(iso3c== 'COD')


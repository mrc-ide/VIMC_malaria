set_demog<- function(params){
  
  mort <- read.csv('vimc_inputs/demography/202310gavi-1_dds-202208_mort_rate_both.csv') |>
    filter(country_code == iso3c) |>
    filter(year >= 2000) |>
    data.table()

  # align age groups
  
  mort[age_to == 0, age_to := 1]
  mort[age_to > 1, age_to := age_to + 1]
  mort[age_to == 121, age_to := 200]
  mort[, age_to := age_to * 365]
  
  # subset to years after 2000 because the model only runs for this time period

  # reorder
  mort <- mort |>
    select(country_code, age_to, year, value)
  
  ages <- round(unique(mort$age_to))
  
  # make empty matrix of deathrates for each timestep
  mat<- matrix(nrow= length(ages))
  
  #  add a baseline column for earliest year
  yr<- min(mort$year)
  input<- mort[year == yr]
  
  # Set deathrates for each age group (divide annual values by 365):
  baseline <- (input$value) / 365
  mat<- matrix(baseline, nrow= 1)
  
  for (i in unique(mort$year)){
    
    message(paste0('prepping year ', i))
    input<- mort[year == i]
    
    # Set deathrates for each age group (divide annual values by 365:
    deathrates <- (input$value) / 365
    mat<- rbind(mat, deathrates) # each column is a different age group, each row a different year
    
  }
  
  
  # Set the population demography using these updated deathrates
  dem_params <- set_demography(
    params,
    agegroups = ages,
    timesteps = c(0, (unique(mort$year)/365)),
    deathrates = mat
  )
  
  return(dem_params)
}


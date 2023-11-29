pull_doses_output <- function(raw_output, processed_output) {
  scenario <- raw_output$scenario[1]
  raw_output$year <- floor(raw_output$timestep / 365) + 2000
  
  ## Pull out doses before 2040 and over all time
  # N fully vaccinated children are the number receiving the last dose.
  
  if(grepl("rts4", scenario) | grepl("r4", scenario)) {  ## for the booster scenarios
    
    doses_per_year <-raw_output |>
      dplyr::group_by(year) |>
      dplyr::summarise(n_model=mean(n_365_729),    ## average number of people in the eligible age grp (?best way to do this)
                       doses_model=sum(n_pev_epi_booster_1)) |>
      mutate(rate_dosing = doses_model/n_model)
    
    ### Merge in VIMC pop in eligible age gp.
    vimc_cohort <- processed_output |>
      dplyr::filter(age==1) |>
      dplyr::select(year, cohort_size)
    
    doses_per_year <- left_join(doses_per_year, vimc_cohort, by="year") |>
      mutate(doses = rate_dosing * cohort_size)
    
  } else {   ## for the dose 3 without booster scenarios
    
    doses_per_year <-raw_output |>
      dplyr::group_by(year) |>
      dplyr::summarise(n_model=mean(n_0_364),    ## average number of people in the eligible age grp (?best way to do this)
                       doses_model=sum(n_pev_epi_dose_3)) |>
      mutate(rate_dosing = doses_model/n_model)
    
    ### Merge in VIMC pop in eligible age gp.
    vimc_cohort <- processed_output |>
      dplyr::filter(age==0) |>
      dplyr::select(year, cohort_size)
    
    doses_per_year <- left_join(doses_per_year, vimc_cohort, by="year") |>
      mutate(doses = rate_dosing * cohort_size)  %>%
      select(-c(n_model, doses_model)) %>%
      filter(year<=2100)
    
  }
  return(doses_per_year)
}


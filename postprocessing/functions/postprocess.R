#' postprocess model output at the site level
#' @param model   model output
#' @param site_name site name
#' @param ur
#' @param scenario vaccine scenario
#' @returns list of parameters for all model runs
#' @export
process_output<- function(model, vimc_input, site_data, site_name, ur, iso3c, scenario, quick_run, description){

  message('postprocessing')
  # calculate rates
  raw_output<- drop_burnin(model, burnin= unique(model$burnin)* 365)

  output <- postie::get_rates(
    raw_output,
    time_divisor = 365,
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )

  dt<- vimc_postprocess(output,
                        le= vimc_input$le_input,
                        site_name= site_name,
                        ur= ur,
                        iso3c= iso3c,
                        site_data,
                        vimc_pop= vimc_input$population_input_all_age,
                        quick_run = quick_run,
                        pop_single_yr= vimc_input$population_input_single_yr)


  # final formatting  ------------------------------------------------------------
  output<- format_outputs(dt, iso3c = iso3c, site_name = site_name, ur= ur, scenario = scenario, description = description)

  if(scenario!="no-vaccination") {

    doses_per_year <- pull_doses_output(raw_output, output)

  } else{
    doses_per_year<- data.table::data.table()
  }
  ### pull out prevalence
  prev <- postie::get_prevalence(raw_output,
                                 time_divisor = 365,
                                 baseline_t = 1999,
                                 age_divisor = 365)
  prev$n_2_10 <- raw_output |>
    mutate(year = floor(timestep/365)) |>
    group_by(year) |>
    summarise(n_2_10 = mean(n_730_3649)) |>
    filter(year != max(year)) |>
    pull(n_2_10)


  return(list('processed_output' = output, 'doses' = doses_per_year, 'prevalence' = prev))
}

#' expand VIMC life expectancy data frame to single yea
#' @param le   VIMC life expectacy input
#' @export
expand_life_expectancy<- function(le, iso3c){

  le<- le |>
    filter(country_code == iso3c,
           year >= 2000) |>
    dplyr::group_by(year) |>
    tidyr::complete(age_from = c(1:100)) |>
    dplyr::ungroup() |>
    tidyr::fill(dplyr::all_of(names(le)), .direction = "down")

  # fill years out (five year age groups)
  le<- le |>
    dplyr::group_by(age_from) |>
    tidyr::complete(year = c(2000:2100))|>
    dplyr::ungroup() |>
    tidyr::fill(dplyr::all_of(names(le)), .direction = "down") |>
    rename(age_lower = age_from,
           remaining_yrs = value) |>
    select(year, age_lower, remaining_yrs)

  return(le)
}

#' VIMC postprocessing
#' @param output  model output
#' @param le VIMC life expectancy input
#' @param iso3c countrycode
#' @param site_data site file
#' @param site_name name of site
#' @param quick_run quick_run
#' @param ur urbanicity
#' @param vimc_population VIMC population input
#' @param pop_single_yr VIMC population input (single year age groups)
#' @export
vimc_postprocess<- function(output, le, iso3c, site_data, site_name, ur, vimc_pop, pop_single_yr, quick_run){

  # fill rates out to single year age groups
  output<- output |>
    dplyr::group_by(.data$t) |>
    tidyr::complete(age_lower = c(1:100)) |>
    select(-.data$age_upper) |>
    dplyr::ungroup() |>
    tidyr::fill(.data$clinical, .data$severe, .data$mortality, .data$yld_pp, .data$yll_pp, .data$dalys_pp, .direction = 'down') |>
    select(-.data$prop_n, -.data$n, -.data$yll_pp, -.data$dalys_pp) |>
    rename(year = t)

    if (quick_run == TRUE){

    # fill rates out flatly
    output<- output |>
      dplyr::group_by(.data$age_lower) |>
      tidyr::complete(year = c(2000:2100)) |>
      dplyr::ungroup() |>
      tidyr::fill(.data$clinical, .data$severe, .data$mortality, .data$yld_pp, .direction = 'down')
  }


  # merge in inputs for expected remaining years of life (to calculate YLLs)  ------
  le<- expand_life_expectancy(le, iso3c)

  # calculate ylls_pp + dalys per person
  dt<- merge(output, le, by = c('year', 'age_lower'), all.x = TRUE)

  # recalculate YLLs and DALYs based on country-specific life expectancy  ------
  dt<- dt |>
    mutate(ylls_pp = .data$mortality * .data$remaining_yrs) |>
    mutate(dalys_pp = .data$ylls_pp + .data$yld_pp) |>
    select(-remaining_yrs)

  # scale site population to be consistent with VIMC population ----------------
  populations<- scale_population(site_data,site_name, ur, iso3c, vimc_pop, pop_single_yr)

  # merge in site population + prop_n
  dt<- merge(dt, populations$site_population, by= 'year')
  dt<- merge(dt, populations$age_proportions, by = c('year', 'age_lower'))

  # calculate counts for entire time period --------------------------------------
  dt<- dt |>
    mutate(
      cases = round(.data$clinical * .data$vimc_site_population * .data$prop_n),
      deaths = round(.data$mortality * .data$vimc_site_population * .data$prop_n),
      ylls = round(.data$ylls_pp * .data$vimc_site_population * .data$prop_n),
      dalys = round(.data$dalys_pp * .data$vimc_site_population * .data$prop_n),
      population = round(.data$vimc_site_population * .data$prop_n)) |>
    select(-prop_n)

  return(dt)

}

#' format outputs for submission
#' @param dt  postprocessed output
#' @param site_name name of site
#' @param ur urbanicity
#' @export
format_outputs<- function(dt, iso3c, site_name, ur, scenario, description){
  dt <- dt |>
    mutate(
      disease = 'Malaria',
      country = iso3c,
      country_name = countrycode::countrycode(
        sourcevar = iso3c,
        origin = 'iso3c',
        destination = 'country.name'),
      site_name = site_name,
      urban_rural = ur,
      scenario = scenario,
      description = description
    ) |>
    rename(age = .data$age_lower,
           cohort_size = .data$population) |>
    select(
      .data$disease,
      .data$year,
      .data$age,
      .data$country,
      .data$country_name,
      .data$site_name,
      .data$urban_rural,
      .data$scenario,
       description,
      .data$cohort_size,
      .data$cases,
      .data$dalys,
      .data$ylls,
      .data$deaths,
      .data$clinical,
      .data$mortality,
      .data$dalys_pp
    ) |>
    mutate(
      cases = if_else(is.na(.data$cases), 0, cases),
      deaths = if_else(is.na(.data$deaths), 0, deaths),
      dalys = if_else(is.na(dalys), 0, dalys),
      mortality = if_else(is.na(mortality), 0, mortality),
      clinical = if_else(is.na(clinical), 0, clinical),
      dalys = if_else(is.na(dalys), 0, dalys)
    )

  return(dt)
}


#' scale site file population to be consistent with country population input from VIMC
#' @param site_data  site file
#' @param site_name name of site
#' @param ur urbanicity
#' @param vimc_pop VIMC population input
#' @param pop_single_yr single-year age group VIMC population input
#' @export
scale_population<- function(site_data, site_name, ur, iso3c, vimc_pop, pop_single_yr){
  # merge in population from site files (as we only have VIMC inputs for the national level)
  # first, separately sum cases by year
  total_pop<- site_data$population |>
    group_by(.data$year) |>
    summarise(summed_pop = sum(.data$pop))

  # pull the population for the site of interest
  pop <- site_data$population |>
    filter(name_1 == site_name & urban_rural == ur) |>
    select(year, pop) |>
    rename(site_file_population = .data$pop)

  # merge these two tables together
  pops<- merge(pop, total_pop, by= 'year')

  # merge in national population from VIMC (available for entire time period)
  vimc_pop<- vimc_pop |>
    filter(country_code == iso3c,
           year >= 2000)|>
    rename(national_pop = .data$value)|>
    select(.data$year, .data$national_pop)

  # merge in vimc population
  pops<- merge(vimc_pop, pops, all.x = T)

  # first rescale site file population based on the ratio of (sum of site file pops in country)/ (VIMC country level population)
  # should be more or less the same, but should be done for consistency sake
  pops<- pops |>
    mutate(vimc_site_population = (.data$site_file_population * .data$national_pop)/.data$summed_pop)

  # calculate population ratio as vimc(site)/ vimc(country)
  pops<- pops |>
    mutate(pop_ratio = .data$vimc_site_population/ .data$national_pop) |>
    tidyr::fill(.data$pop_ratio, .direction = 'down')

  # then calculate vimc_site_population by multiplying this ratio to the national population for the final 50 years
  pops<- pops |>
    mutate(vimc_site_population = ifelse(.data$year<= 2050, .data$vimc_site_population, .data$pop_ratio* .data$national_pop))

  # subset out site file population for 2000-2100
  site_pop<- pops |>
    select(.data$year, .data$vimc_site_population)

  # pull in single year population to calculate proportion_n by age group
  national_pop<- pops |>
    select(.data$year, .data$national_pop)

  age_proportions<- merge(pop_single_yr, national_pop, by = c('year'))
  age_proportions <- age_proportions |>
    mutate(prop_n = .data$value/ .data$national_pop) |>
    select(.data$year, .data$age_from, .data$age_to, .data$prop_n) |>
    rename(age_lower = .data$age_from)

  return(list('site_population' = site_pop, 'age_proportions' = age_proportions))

}

#' pull dosage output
#' @param raw_output  model output
#' @param processed_output name of site
#' @export
pull_doses_output <- function(raw_output, processed_output) {
  scenario <- raw_output$scenario[1]
  raw_output$year <- floor(raw_output$timestep / 365) + 2000

  ## Pull out doses before 2040 and over all time
  # N fully vaccinated children are the number receiving the last dose.

  if(grepl("rts4", scenario) | grepl("r4", scenario)) {  ## for the booster scenarios

    doses_per_year <-raw_output |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(n_model=mean(n_365_729),    ## average number of people in the eligible age grp (?best way to do this)
                       doses_model=sum(n_pev_epi_booster_1)) |>
      mutate(rate_dosing = .data$doses_model/.data$n_model)

    ### Merge in VIMC pop in eligible age gp.
    vimc_cohort <- processed_output |>
      dplyr::filter(age==1) |>
      dplyr::select(year, .data$cohort_size)

    doses_per_year <- left_join(doses_per_year, vimc_cohort, by="year") |>
      mutate(doses = rate_dosing * cohort_size)

  } else {   ## for the dose 3 without booster scenarios

    doses_per_year <-raw_output |>
      dplyr::group_by(year) |>
      dplyr::summarise(n_model=mean(n_0_364),    ## average number of people in the eligible age grp
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


pull_low_transmission_sites<- function(iso3c, site_data, processed_sites){
  # pull site output for no-vaccination for the low transmission settings

  site_data$prevalence<- site_data$prevalence |>
    filter(year == 2019) |>
    mutate(run_model = ifelse(pfpr > 0.10, TRUE, FALSE))

  # make exceptions for Madagascar, Ethiopia, and Sudan
  # hardcode for time's sake but operationalize later
  if(unique(site_data$prevalence$country) == 'Madagascar'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model))
  }

  if(unique(site_data$prevalence$country) == 'Ethiopia'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model))


  }


  if(unique(site_data$prevalence$country) == 'Sudan'){


    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |>
      mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))

  }
  if(length(unique(site_data$prevalence$run_model)) ==  1 & site_data$prevalence$run_model[1] == TRUE){

    return(data.table())

  } else{

    prevalence<- site_data$prevalence |>
      select(name_1, urban_rural, iso3c, run_model) |>
      rename(site_name = name_1,
             ur= urban_rural)


    site_info<- prevalence |>
      filter(iso3c == {{iso3c}},
             run_model == FALSE)

    append<- data.table()
    message(paste0('adding ', nrow(site_info), ' sites'))
    for (i in 1:nrow(site_info)){

      site<- site_info[ i,]

      add<- processed_sites |>
        filter(site_name == site$site_name & urban_rural == site$ur)

      append<- rbind(append, add, fill = T)
    }

    return(append)


  }
}




# calculate the proportion of severe cases + deaths to recalculate severe cases + deaths after scaling
add_proportions<- function(dt){
  dt<- dt |>
    mutate(prop_severe = severe/cases,
           prop_deaths = deaths/ cases) |>
    mutate(prop_severe = if_else(is.na(.data$prop_severe), 0, prop_severe),
           prop_deaths = if_else(is.na(.data$prop_deaths), 0, prop_deaths))



  return(dt)
}



plotting_theme<- theme_bw(base_size = 12) +
  theme( legend.position = 'bottom',
         strip.text.x = element_text(size = rel(0.8)),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
         #axis.text.y = element_blank(),
         text = element_text(family= 'Arial Narrow'),
         axis.ticks.y= element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank())

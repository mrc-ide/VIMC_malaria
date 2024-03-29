# parameter functions

#' Make list of parameters that will apply to all model runs
#' @param   quick_run     quick run setting
#' @returns list of parameters for all model runs
#' @export
pull_age_groups_time_horizon<- function(quick_run){

  year<- 365
  burnin<- 15

  if(quick_run == TRUE){

    term_yr<- 2030
    pop_val<- 5000

    min_ages = c(0:5, 6,15,20) * year
    max_ages = c(1:6, 15,20,200) * year -1

  }

  else{

    pop_val<- 50000
    term_yr<- 2100

    min_ages = c(seq(0, 19, by= 1), seq(20, 90, by= 10)) * year
    max_ages = c(seq(1, 20, by= 1), seq(30, 100, by= 10)) * year -1

  }

  return(list('term_yr' = term_yr,
              'pop_val' = pop_val,
              'min_ages'= min_ages,
              'max_ages' = max_ages,
              'burnin' = burnin))
}

#' parameterize site + urbanicty of interest
#' @param   site_name        name of site
#' @param   ur               urbanicity, urban or rural
#' @param   site_data        site file
#' @param   coverage_data    VIMC vaccine forecast for site of interest
#' @param   scenario         vaccine forecast scenario
#' @param   parameter_draw   parameter draw value
#' @param   quick_run        quick_run setting (boolean)
#' @returns site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
#' @export
pull_input_params<- function(site_name,
                             ur,
                             iso3c,
                             site_data,
                             coverage_data,
                             scenario,
                             parameter_draw,
                             quick_run){

  message('parameterizing')
  # site data
  site <- extract_site(site_file = site_data,
                       site_name = site_name,
                       ur = ur)


  run_params<- pull_age_groups_time_horizon(quick_run)

  # specify vaccine coverage based on forecast  ----------------------------------
  site<- expand_intervention_coverage(site,
                                      terminal_year = run_params$term_yr)
  site<- update_coverage_values(site,
                                iso3c = iso3c,
                                coverage_data,
                                scenario_name = scenario)

  # check the site has a non-zero EIR
  check_eir(site)

  # pull parameters for this site ------------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    burnin = run_params$burnin,
    overrides = list(human_population = run_params$pop_val)
  )

  # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages

  # if this is a stochastic run, set parameter draw ------------------------------
  params<- parameterize_stochastic_run(params, parameter_draw)


  inputs <- list(
    'param_list' = params,
    'site_name' = site_name,
    'ur' = ur,
    'iso' = iso3c,
    'scenario' = scenario,
    'parameter_draw' = parameter_draw,
    'pop_val' = run_params$pop_val,
    'burnin' =  run_params$burnin
  )

  return(inputs)

}



#' Change parameter set based off of stochastic draws
#' @param   params           model input parameters
#' @param   parameter_draw   parameter draw number (0-50)
#' @returns parameters for stochastic run
#' @export
parameterize_stochastic_run<- function(params, parameter_draw){

  if (parameter_draw > 0){

    params<- params |>
      set_parameter_draw(parameter_draw) |>
      set_equilibrium(init_EIR= params$init_EIR)

  }

  return(params)
}

#' update vaccine coverage based on VIMC inputs from Montagu
#' @param   site             site data file
#' @param   coverage_data    VIMC vaccine forecast for site of interest
#' @param scenario_name scenario for vaccine forecast
#' @returns site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
#' @export
update_coverage_values<- function(site, iso3c, coverage_data, scenario_name){

  if(scenario_name == 'no-vaccination'){

    coverage_data<- coverage_data |>           # pull another projection for data table structure and fill with zeroes
      filter(country_code == iso3c) |>
      filter(scenario == 'malaria-r3-r4-default') |>
      mutate(coverage = 0)  |>
      mutate(scenario = 'no-vaccination')

  }else{

    coverage_data<- coverage_data |>
      filter(country_code == iso3c) |>
      filter(scenario == scenario_name)
  }

  dt<- coverage_data |>
    rename(vaccine_name = vaccine) |>
    data.table()

  # add identifying type column for vaccine
  dt[{{vaccine_name}} %like% 'RTS', vaccine := 'RTS,S']
  dt[is.na(vaccine), vaccine := 'R21']

  vaccine_val<- unique(dt$vaccine)

  if (length(vaccine_val) > 1){ stop('Can only implement one type of vaccine at a time. Check vaccine inputs.') }

  dt<- dcast(data.table(dt),
             year + vaccine ~ vaccine_name,
             value.var= 'coverage')

  # if columns for other vaccines or doses are empty, fill them ----------------
  columns_to_check <- c("R3", "R4", "RTS3", "RTS4")
  missing_columns <- setdiff(columns_to_check, names(dt))

  dt <- dt |>
    add_column(!!!setNames(rep(0, length(missing_columns)),
                           missing_columns))

  dt <- dt |>
    rename(rtss_coverage = RTS3,
           rtss_booster_coverage = RTS4,
           r21_coverage = R3,
           r21_booster_coverage = R4)

  # transform booster coverage into value per person according to coverage in the preceding year

  if(scenario_name == 'malaria-rts3-rts4-bluesky'){
    dt[rtss_booster_coverage== 0.9, rtss_booster_coverage:= 1]
  }

else{
  for (yr in unique(dt$year)){

    dt[year== yr & rtss_coverage!= 0 & rtss_booster_coverage!= 0,
       rtss_booster_coverage := rtss_booster_coverage / dt[year == yr- 1, rtss_coverage]]

    dt[year== yr & r21_coverage!= 0 & r21_booster_coverage!= 0,
       r21_booster_coverage := r21_booster_coverage / dt[year == yr- 1, r21_coverage]]
  }
}
  intvns<- data.table(merge(site$interventions, dt, by = 'year', all.x= T))

  intvns[is.na(rtss_coverage), "rtss_coverage" := 0]
  intvns[is.na(rtss_booster_coverage), "rtss_booster_coverage" := 0]
  intvns[is.na(r21_coverage), "r21_coverage" := 0]
  intvns[is.na(r21_booster_coverage), "r21_booster_coverage" := 0]
  intvns[is.na(vaccine), vaccine := vaccine_val]

  site$interventions<- intvns

  return(site)
}


#' expand intervention years out to terminal year of forecast using scene package
#' @param   site             site file
#' @param   terminal_year    terminal year of forecast
#' @returns site file with extrapolated coverage values out to terminal year
#' @export
expand_intervention_coverage<- function(site, terminal_year){

  # first set terminal year to terminal year of forecast
  group_var <- names(site$sites)

  first_yr<- max(site$interventions$year) + 1            # first year in site file
  itn_yr<- first_yr- 3                                   # last year to carry over for ITN usage and model input (3 year cycle)

  site$interventions <- site$interventions |>
    scene::expand_interventions(max_year = terminal_year,
                                group_var = group_var)


  for (yr in c(first_yr:terminal_year)){

    comparator<- site$interventions |>
      filter(year == yr - 3)

    intvns <-   data.table(site$interventions)
    intvns[year == yr, `:=` (itn_use = comparator$itn_use,
                             itn_input_dist = comparator$itn_input_dist)]
    site$interventions <- intvns

  }


  site$interventions <- site$interventions |>
    scene::fill_extrapolate(group_var = group_var)

  return(site)
}


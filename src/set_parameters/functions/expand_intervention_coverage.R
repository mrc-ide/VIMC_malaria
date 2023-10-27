#' expand intervention years out to terminal year of forecast using scene package
#'
#' @param   site             site data file
#' @param   terminal_year    terminal year of forecast
#' @returns site file with extrapolated coverage values out to terminal year
expand_intervention_coverage<- function(site, terminal_year){
  
  # first set terminal year to terminal year of forecast
  group_var <- names(site$sites)
  
  first_yr<- max(site$interventions$year)  +1              # first year in site file
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


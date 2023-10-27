
#' R21 vax and booster profiles from R21 paper
#' pull values from R21 parameter draws
#'
#' @param   i    observation in draw dataset
#' @returns list of R21 profile + R21 booster profile 
pull_r21_draws<- function(i){
  r21_params <- readRDS("r21_draws.rds")[i,]
  
  r21_profile <- rtss_profile 
  
  r21_profile$vmax = r21_params$v_max
  r21_profile$alpha = r21_params$alpha
  r21_profile$beta = r21_params$beta
  r21_profile$cs = c(r21_params$mu_r21_cs, r21_params$sd_r21_cs)
  r21_profile$rho = c(r21_params$mu_r21_rho, r21_params$sd_r21_rho)
  r21_profile$ds = c(r21_params$mu_r21_ds, r21_params$sd_r21_ds)
  r21_profile$dl = c(r21_params$mu_r21_dl, r21_params$sd_r21_dl)
  
  r21_booster_profile <- r21_profile
  r21_booster_profile$cs = c(r21_params$mu_r21_cs_boost, r21_params$sd_r21_cs_boost)
  r21_booster_profile$rho = c(r21_params$mu_r21_rho_boost, r21_params$sd_r21_rho_boost)
  
  r21_booster_profile2 <- r21_booster_profile
  r21_booster_profile2$cs = c(r21_params$mu_r21_cs_boost2, r21_params$sd_r21_cs_boost2)
  
  return(list('r21_profile' = r21_profile, 'r21_booster_profile' = r21_booster_profile, 'r21_booster_profile2' = r21_booster_profile2))
}

#' Median R21 vax and booster profiles from R21 paper (see Table 1 of manuscript)
#' @returns list of R21 profile + R21 booster profile 
format_median_r21_profile<- function(){
  
  r21_params<- data.table(read.csv('r21_malariasimulation_parameters.csv'))
  
  r21_profile <- rtss_profile 
  r21_profile$cs<- c(r21_params[par== 'r21_cs']$mu, r21_params[par== 'r21_cs']$sd)
  r21_profile$rho<- c(r21_params[par== 'r21_rho']$mu, r21_params[par== 'r21_rho']$sd)
  r21_profile$ds<- c(r21_params[par== 'r21_ds']$mu, r21_params[par== 'r21_ds']$sd)
  r21_profile$dl<- c(r21_params[par== 'r21_dl']$mu, r21_params[par== 'r21_dl']$sd)
  r21_profile$beta<- 471
  r21_profile$alpha<- 0.91
  r21_profile$vmax<- 0.87
  
  
  r21_booster_profile <- r21_profile
  r21_booster_profile$cs<- c(r21_params[par== 'r21_cs_boost']$mu, r21_params[par== 'r21_cs_boost']$sd)
  r21_booster_profile$rho<- c(r21_params[par== 'r21_rho_boost']$mu, r21_params[par== 'r21_rho_boost']$sd)
  
  return(list('r21_profile' = r21_profile, 'r21_booster_profile' = r21_booster_profile))
}


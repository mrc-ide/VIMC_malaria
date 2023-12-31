#' Extract a single site-input from a country site file
#'
#' @param site_file Country site file
#' @param index Index row from site_file$sites
#'
#' @return Single site
#' @export
extract_site <- function(site_file, site_name, ur){
  
  sites<- data.table(site_file$sites)
  Encoding(sites$name_1) <- "UTF-8"
  
  sites$name_1<- iconv(sites$name_1, from="UTF-8", to="ASCII//TRANSLIT")
  
  index_site <- sites[name_1== site_name & urban_rural== ur]
  
  to_mod <- c("sites", "interventions", "pyrethroid_resistance", "population",
              "vectors", "seasonality", "prevalence", "eir")
  
  site <- site_file
  
  for(level in to_mod){
    mod<- site[[level]]
    Encoding(mod$name_1) <- "UTF-8"

    mod$name_1<- iconv(mod$name_1, from="UTF-8", to="ASCII//TRANSLIT")
    
    mc <- intersect(colnames(index_site), colnames(mod))
    site[[level]] <- dplyr::left_join(index_site, mod, by = mc)
  }
  return(site)
}

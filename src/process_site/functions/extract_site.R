#' Extract a single site-input from a country site file
#'
#' @param site_file Country site file
#' @param index Index row from site_file$sites
#'
#' @return Single site
#' @export
extract_site <- function(site_file, site_name, ur){
  
  sites<- data.table(site_file$sites)
  index_site <- sites[name_1== site_name & urban_rural== ur]
  
  to_mod <- c("sites", "interventions", "pyrethroid_resistance", "population",
              "vectors", "seasonality", "prevalence", "eir")
  
  site <- site_file
  for(level in to_mod){
    mc <- intersect(colnames(index_site), colnames(site[[level]]))
    site[[level]] <- dplyr::left_join(index_site, site[[level]], by = mc)
  }
  return(site)
}

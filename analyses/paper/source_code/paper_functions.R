##########################################################
## title paper_functions
## author Lydia Haile
## purpose functions used to fill out paper
###########################################################################

# packages
library(vimcmalaria)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(ggpubr)
library(data.table)
library(purrr)
library(sf)

# objects for plotting
cols<- c('#CE5137', '#1B4F72', '#45B39D', '#BA4A00')


plotting_theme<- theme_bw(base_size = 14) +
    theme( legend.position = 'bottom',
           strip.text.x = element_text(size = rel(0.8)),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
           text = element_text(family= 'TT Arial'),
           axis.ticks.y= element_blank(),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# helper functions
get_values <- function(col) {
  mean <- mean(col, na.rm = TRUE)
  lower <- quantile(col, 0.025, na.rm = TRUE)
  upper <- quantile(col, 0.975, na.rm = TRUE)

  return(paste0(mean, "  (", lower, " - ", upper, ")"))
}

# make admin-1 boolean variable
read_pop<- function(iso3c){

  site_data<- site::fetch_site(iso3c, urban_rural= TRUE)


  prevalence<- prev |>
    select(name_1, urban_rural, iso3c, run_model)

  #now pull annual population and merge this on
  pop<- site_data$population$population_total


  pop<- merge(pop, prevalence, by = c('name_1', 'urban_rural', 'iso3c'))

  return(pop)
}

extract_site_data <- function(directory) {
  site_file <- readRDS(paste0("archive/process_inputs/", directory, "/site_file.rds"))

  # extract prevalence data
  prev <- site_file$prevalence |>
    filter(year %in% c(2000, 2019)) |>
    mutate(id = paste0(name_1, "_", urban_rural)) |>
    data.table()

  prevs <- dcast(prev, id ~ year, value.var = "pfpr") |>
    rename(
      pfpr_2000 = `2000`,
      pfpr_2019 = `2019`
    )


  # extract intervention coverage data for last available year
  intvns <- site_file$interventions |>
    filter(year == 2022) |>
    mutate(id = paste0(name_1, "_", urban_rural)) |>
    select(id, itn_use, tx_cov, smc_cov, pmc_cov, irs_cov)

  site_info <- merge(prevs, intvns, by = "id")

  return(site_info)
}




# generate maps
Africa <- c("DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM",
            "COD","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB",
            "CIV","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MYT",
            "MAR","MOZ","NAM","NER","NGA","COG","REU","RWA","SHN","STP","SEN",
            "SYC","SLE","SOM","ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA",
            "ESH","ZMB","ZWE")

# save copies of gadm SpatVector as .rds files in data folder
import_gadm <- function(ISO, level){
  geodata::gadm(country = ISO, level = level, path = "J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/analyses/gadm", version = "4.0")
}

map2(Africa, 0, import_gadm)    # admin0
map2(Africa, 1, import_gadm)    # admin1

# create a list of all countries
countries_africa <- lapply(c(Africa),
                           function(x){
                             list.files(path = "J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/analyses/gadm",
                                        pattern = paste0("*", x , "_0_pk.rds"),
                                        full.names = TRUE)
                           }) |>
  unlist()


admin1s_africa <- lapply(c(Africa),
                         function(x){
                           list.files(path = "J:/VIMC_archive/VIMC_runs_sept_2024/VIMC_malaria/analyses/gadm",
                                      pattern = paste0("*", x , "_1_pk.rds"),
                                      full.names = TRUE)
                         }) |>
  unlist()
# unpack gadms
unpack_gadm <- function(file){

  object <- readRDS(file) # read in object
  object <- terra::vect(object) # unpack SpatVector
  st_as_sf(object) # transform to sf object

}

countries_africa <- map_dfr(countries_africa, unpack_gadm) # loop over each country
admin1s_africa <- map_dfr(admin1s_africa, unpack_gadm) # loop over each admin1
st_crs(countries_africa); st_crs(countries_africa) # view CRS
# select out African countries with malaria (i.e. countries in foresite package)
countries <- countries_africa[!(countries_africa$ID_0 %in% c("DZA", "CPV", "EGY", "LSO", "LBY", "MUS", "MYT", "MAR", "REU", "SHN", "STP", "SYC", "TUN", "ESH")), ]
admin1s <- admin1s_africa[!(admin1s_africa$ID_0 %in% c("DZA", "CPV", "EGY", "LSO", "LBY", "MUS", "MYT", "MAR", "REU", "SHN", "STP", "SYC", "TUN", "ESH")), ]




admin1s<- admin1s |>
  mutate(id= paste0(NAME_1, '_', ID_0))


#' site file is split by admin1 and urbanicity, for the sake of mapping need to reduce down to admin1
#' @param admin_data
#'
#' @returns admin1 data reduced down to urbanicity
#' @export
#'
#' @examples
remove_urbanicity<- function(admin_data){

  reduced<- data.frame()
  for(nm in unique(admin_data$name_1)){

    message(nm)
    subset<- admin_data |> filter(name_1 == nm)

    if(nrow(subset) > 1 & length(unique(subset$run_model))== 1){

      subset<- subset[1]
    }
    if(nrow(subset) > 1 & length(unique(subset$run_model))== 2){

      subset<- subset[run_model == 'Introduced']
    }
    subset<- subset |> select(-urban_rural)
    reduced<- rbind(subset, reduced)
  }

  return(reduced)
}



# Country borders --------------------------------------------------------------
# define borders between country polygons
borders <- st_boundary(countries) # will find all outside and inside borders
border_admins <- admin1s[unique(unlist(st_touches(borders, admin1s))),]

borders_inner <- countries |> # will find inner borders
  rmapshaper::ms_innerlines() |>
  # as_tibble() |>
  st_as_sf()

admin1s_intersect <- st_intersects(admin1s, borders_inner)

index <- tibble(index = as.numeric(admin1s_intersect %>% lengths > 0))
border_admins <- bind_cols(admin1s, index) |>
  mutate(nrow = row_number()) |>
  # remove admins not touching the inner borders
  filter(index == 1)
  # remove admin1s missed by the function (Madagascar, South Africa, Nigeria)
  #filter(!(nrow %in% c(482, 490, 305, 307, 424)))



#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_site_outputs<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  iso3c<- metadata$iso3c
  scenario<- metadata$scenario

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))                  # get output file
  sites<- data.table::rbindlist(lapply(output$site_output, function(x) return(x$processed_output))) #pull out processed site_level output


  saveRDS(sites, paste0('J:/VIMC/VIMC_malaria/analyses/paper/inputs/site_output/', iso3c, '_site_output', scenario, '.rds'))

  return(sites)
}

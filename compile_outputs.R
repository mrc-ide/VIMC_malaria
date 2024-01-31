# compile reports-- if results are missing, impute zeroes for now

path <-"J:/reclone/VIMC_malaria/archive/scale_and_plot/"

filenames <-list.files(path)
library(purrr)
date = as.numeric(substring(filenames,1,8))

list.files(paste0(path,"/",filenames[1]))

# some functions
get_country <- function(filename) {
  site_data <-readRDS(paste0(path,"/",filename,"/site_file.rds"))

  return(data.frame(iso = site_data$sites$iso3c[1], country=site_data$sites$country[1]))
}


get_scenario <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
  return(data.frame(scenario = country_output$scenario[1],
                    parameter_draw= country_output$parameter_draw[1]))
}

get_output <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/processed_output.rds"))
  return(country_output)

}
#### compile country data
cres <-data.frame(filenames = filenames, date = date,
                  date_time = as.numeric(paste0(substring(filenames,1,8), substring(filenames, 10,13)))
)
cres<- data.table(cres)
cres<- cres[date_time >= 202401291731]

cres <- cbind(cres, bind_rows(map(cres$filenames, get_country)))
cres <- cbind(cres, bind_rows(map(cres$filenames, get_scenario)))

dim(cres)
cres <- cres |>
  arrange(desc(date_time)) |>
  dplyr::distinct(iso, country,parameter_draw, scenario, .keep_all = TRUE) |>
  arrange(country, scenario, parameter_draw)




# bind outputs together
outputs<- rbindlist(lapply(cres$filenames, get_output))

# final formatting to template  ------------------------------------------------

final<- outputs |>
  select(-pre_scaled_cases,
         -clinical,
         -mortality,
         -dalys_pp) |>
  select(disease,
         year,
         age,
         country,
         country_name,
         cohort_size,
         cases,
         dalys,
         deaths,
         ylls,
         scenario,
         parameter_draw)


Encoding(final$country_name) <- "UTF-8"
final$country_name<- iconv(final$country_name, from="UTF-8", to="ASCII//TRANSLIT")

for (scen in unique(final$scenario)){

  print(scen)

  saving<- final |>
    filter(scenario == scen) |>
    rename(run_id = parameter_draw)
  print(length(unique(saving$country)))
  saving<- saving |>
    select(-scenario)

  write.csv(saving, paste0('montagu/', 'stochastic-burden-est-', scen, '.csv'))
}





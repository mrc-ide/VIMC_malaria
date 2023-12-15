# process for montagu-- first run

path <-"N:/Lydia/VIMC_malaria/archive/process_country"

filenames <-list.files(path)
library(purrr)
date = as.numeric(substring(filenames,1,8))
filenames <- filenames[which(date>20231130)]

list.files(paste0(path,"/",filenames[1]))

# some functions
get_country <- function(filename) {
  site_data <-readRDS(paste0(path,"/",filename,"/site_file.rds"))
  return(data.frame(iso = site_data$sites$iso3c[1], country=site_data$sites$country[1]))
}


get_scenario <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/country_output.rds"))
  return(data.frame(scenario = country_output$scenario[1], 
                    description = country_output$description[1]))
}

#### compile country data
cres <-data.frame(filenames = filenames, date = date[which(date>20231130)],
                  date_time = as.numeric(paste0(substring(filenames,1,8), substring(filenames, 10,13)))
)
cres <- cbind(cres, bind_rows(map(filenames, get_country)))
cres <- cbind(cres, bind_rows(map(filenames, get_scenario)))

dim(cres)
cres <- cres |>
  arrange(desc(date_time)) |>
  dplyr::distinct(iso, country, scenario, description, .keep_all = TRUE) |>
  arrange(country, scenario)


get_output <- function(filename) {
  country_output <- readRDS(paste0(path,"/",filename,"/country_output.rds"))
  return(country_output)
  
}

# bind outputs together
outputs<- rbindlist(lapply(cres$filenames, get_output))

le<- read.csv('src/process_inputs/vimc_inputs/demography/202310gavi-1_dds-202208_life_ex_both.csv')

unique(outputs$scenario)
names(outputs)

# recalculate ylls
test<- le |>
  filter(year >= 2000) |>
  dplyr::group_by(year, country, country_code) |>
  tidyr::complete(age_from = c(1:100)) |>
  dplyr::ungroup() |>
  tidyr::fill(dplyr::all_of(names(le)), .direction = "down")


test<- test |>
  dplyr::group_by(age_from,country, country_code) |>
  tidyr::complete(year = c(2000:2100))|>
  dplyr::ungroup() |>
  tidyr::fill(dplyr::all_of(names(le)), .direction = "down") |>
  rename(age_lower = age_from,
         remaining_yrs = value) |>
  select(year, country_code, age_lower, remaining_yrs) 

test<- test |>
  rename(age = age_lower) |>
  rename(country = country_code)

# calculate ylls_pp + dalys per person
output<- merge(outputs, test, by = c('year', 'age', 'country'), all.x = TRUE)

#calculate yll
output <- output |>
  mutate(yll = round(deaths * remaining_yrs),
         cases = round(cases),
         cohort_size = round(cohort_size))

# final formatting to template  ------------------------------------------------

final<- output |> 
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
         yll,
         scenario)


Encoding(final$country_name) <- "UTF-8"
final$country_name<- iconv(final$country_name, from="UTF-8", to="ASCII//TRANSLIT")

for (scen in unique(final$scenario)){
  
  print(scen)
  
  saving<- final |>
    filter(scenario == scen)
  
  saving<- saving |>
    select(-scenario)
  
  write.csv(saving, paste0('montagu/', scen, '_final_output.csv'))
}





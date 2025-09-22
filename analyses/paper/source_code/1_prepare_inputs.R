#########################################################
## title   1_prepare_inputs
## author  Lydia Haile
## purpose pull final outputs in different formats and save to folder 
##         for number plugging, tables, and figures
###########################################################################

  source('J:/VIMC/VIMC_malaria/analyses/paper/source_code/paper_functions.R')

iso3cs<-read.csv('src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv') |> pull(country_code) |> unique()

# pull country level output -------------- --------------------------------------
completed <- completed_reports("process_country") |>
  dplyr::filter(
    description == "number_plug_paper",
    quick_run == FALSE
  ) |>
  dplyr::arrange(dplyr::desc(date_time)) |>
  dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, .keep_all = TRUE) |>
  dplyr::arrange(iso3c, scenario, parameter_draw)


default<- compile_final_outputs('number_plug_paper') |>
  data.table() |>
  mutate(scenario = ifelse(scenario == 'proxy', 'Routine', scenario))

total<- copy(default) # VIMC output unsummarised
total<- unique(total)

# calculate cases averted (all ages)-------------------------------------------------------------------------------------
  novax<- total |> filter(scenario == 'no-vaccination') |>
    rename(cases_novax = cases,
           deaths_novax = deaths,
           dalys_novax = dalys) |>
    select(year, age, run_id, country, cases_novax, deaths_novax, dalys_novax)

  vax<- total |> filter(scenario == 'Routine')
  vax<- merge(vax, novax, by = c('country', 'age', 'year', 'run_id'))

  vax<- vax |>
    filter(run_id < 51) |>
    mutate(cases_averted = cases_novax- cases,
           deaths_averted = deaths_novax- deaths,
           dalys_averted = dalys_novax - dalys)

vax<- unique(vax)
saveRDS(vax, 'analyses/paper/inputs/vax.rds')

# check squareness of dataset 

check_squareness<- vax |>
  group_by(country_name, run_id, year, scenario) |>
    summarise(cases_averted = sum(cases_averted, na.rm = TRUE),
              deaths_averted = sum(deaths_averted, na.rm = TRUE),
              dalys_averted = sum(dalys_averted, na.rm = TRUE),
              cases_novax = sum(cases_novax, na.rm = TRUE),
              deaths_novax = sum(deaths_novax, na.rm = TRUE),
              dalys_novax = sum(dalys_novax, na.rm = TRUE),
              .groups = 'keep')

# visualise this so I can be sure there are 50 outputs for each country
ggplot(data= check_squareness, mapping = aes(x= year, y= cases_averted, group= run_id)) +
  geom_line() +
  facet_wrap_paginate(~country_name, scales= 'free', nrow= 4, ncol= 4, page= 2)


# summary across africa ------------------------------------------------------------------------------------------------------------------
  africa<- vax |>
   # filter(run_id == 0) |>
    group_by(scenario, run_id) |>
    #filter(year <= 2050) |>
    summarise(cases_averted = sum(cases_averted, na.rm = TRUE),
              deaths_averted = sum(deaths_averted, na.rm = TRUE),
              dalys_averted = sum(dalys_averted, na.rm = TRUE),
              cases_novax = sum(cases_novax, na.rm = TRUE),
              deaths_novax = sum(deaths_novax, na.rm = TRUE),
              dalys_novax = sum(dalys_novax, na.rm = TRUE),
              .groups = 'keep') |>
    ungroup() |>
    group_by(scenario) |>
    filter(scenario!= 'Control') |>
    mutate(percent_cases_averted = cases_averted/cases_novax,
           percent_deaths_averted = deaths_averted/ deaths_novax,
           percent_dalys_averted = dalys_averted/ dalys_novax) |>
    data.table()
  print(africa)

saveRDS(africa, 'analyses/paper/inputs/africa.rds')

# results by country -------------------------------------------------------------------------------
by_country <- vax |>
  group_by(scenario, country, country_name, run_id) |>
  summarise(
    cases_averted = sum(cases_averted),
    deaths_averted = sum(deaths_averted),
    dalys_averted = sum(dalys_averted),
    .groups = "keep"
  ) |>
  data.table() |>
  ungroup() |>
  group_by(scenario, country) |>
  mutate(
    cases_mean = mean(cases_averted, na.rm = TRUE),
    cases_lower = quantile(cases_averted, 0.025, na.rm = TRUE),
    cases_upper = quantile(cases_averted, 0.975, na.rm = TRUE),
    deaths_mean = mean(deaths_averted, na.rm = TRUE),
    deaths_lower = quantile(deaths_averted, 0.025, na.rm = TRUE),
    deaths_upper = quantile(deaths_averted, 0.975, na.rm = TRUE)
  ) |>
  arrange(-cases_averted) |>
  filter(!scenario == "Control")

saveRDS(by_country, 'analyses/paper/inputs/by_country.rds')


# results per fully vaccinated person ---------------------------------------------------------------------------------------------------
total_doses<- compile_dose_outputs('number_plug_paper', iso = 'all') |> data.table() |> filter(scenario %in% c('proxy', 'no-vaccination'))
total_doses[scenario %like% 'no-vaccination', scenario := 'Control']
total_doses<- unique(total_doses)
total_doses<- total_doses |>
  #filter(year <= 2038)|>
  group_by(iso3c, scenario, run_id) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            fvp = sum(fvp),
            .groups = 'keep') |>
  mutate(cases_averted_per_fvp= cases_averted*100000/ fvp,
         deaths_averted_per_fvp = deaths_averted*100000/ fvp) |>
  select(iso3c, scenario, run_id, cases_averted_per_fvp, deaths_averted_per_fvp) |>
  rename(country = iso3c) |>
  ungroup() |>
  group_by(scenario, country) |>
  summarise(
    cases_mean = mean(cases_averted_per_fvp, na.rm = TRUE),
    cases_lower = quantile(cases_averted_per_fvp, 0.025, na.rm = TRUE),
    cases_upper = quantile(cases_averted_per_fvp, 0.975, na.rm = TRUE),
    deaths_mean = mean(deaths_averted_per_fvp, na.rm = TRUE),
    deaths_lower = quantile(deaths_averted_per_fvp, 0.025, na.rm = TRUE),
    deaths_upper = quantile(deaths_averted_per_fvp, 0.975, na.rm = TRUE),
    .groups= 'keep'
  ) |>
  unique()


saveRDS(total_doses, 'analyses/paper/inputs/total_doses.rds')

# admin units modelled -----------------------------------------------------------------------------------------------
pops<- rbindlist(lapply(iso3cs, read_pop))
pops<- pops |>
  unique(by= c('name_1', 'urban_rural'))|>
  data.table()
pops<- pops[, run_model := as.character(run_model)]
pops<- pops[run_model == TRUE, run_model := 'Introduced']
pops<- pops[run_model == FALSE, run_model := 'Not introduced']
pop_reduced<- remove_urbanicity(pops) |>
  mutate(id = paste0(name_1, '_', iso3c))
# output this as a dataset to add to the appendix
pop_format<- pop_reduced |>
  select(name_1, country, run_model) |>
  rename(admin_unit = name_1,
         vaccine_introduced= run_model)

saveRDS(pop_reduced, 'analyses/paper/inputs/pop_reduced.rds')
saveRDS(pop_format, 'analyses/paper/inputs/pop_format.rds')

# results grouped by age ---------------------------------------------------------------------------------------------------
vax_age<- copy(vax)
vax_age$age<- cut(vax_age$age, breaks = c(0, 5, 15, 101), include.lowest = TRUE)
table_2<- vax_age |>
  filter(scenario== 'Routine') |>
  group_by(scenario, age, year, run_id) |>
  summarise(cases_averted = sum(cases_averted,na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            dalys_averted = sum(dalys_averted, na.rm= TRUE),
            cohort_size = sum(cohort_size, na.rm= TRUE),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, age, year) |>
    summarise(
      cases_mean = mean(cases_averted, na.rm = TRUE),
      cases_lower = quantile(cases_averted, 0.025, na.rm = TRUE),
      cases_upper = quantile(cases_averted, 0.975, na.rm = TRUE),
      deaths_mean = mean(deaths_averted, na.rm = TRUE),
      deaths_lower = quantile(deaths_averted, 0.025, na.rm = TRUE),
      deaths_upper = quantile(deaths_averted, 0.975, na.rm = TRUE),
      cohort_size = mean(cohort_size, na.rm = TRUE),
      .groups= 'keep'
    ) |>
    unique() |>
  as.data.table()


  table_2[age== '[0,5]', age:= 'under 5']
  table_2[age== '(5,15]', age:= '5 to 15']
  table_2[age== '(15,101]', age:= 'over 15']

saveRDS(table_2, 'analyses/paper/inputs/table_2.rds')

central<- completed |> filter(parameter_draw == 0)

# results by admin1 unit---------------------------------------------------------------------------------------------------------------
lapply(c(1:nrow(central)), get_site_outputs, map= central, output_filepath= 'J:/VIMC/VIMC_malaria/archive/process_country/')
files<- list.files('analyses/paper/inputs/site_output/', full.names = TRUE)
outputs<- rbindlist(lapply(files, readRDS))
outputs<- outputs |>
  mutate(id = paste0(site_name, '_', urban_rural))

test<- unique(outputs)
# incidence rate before vaccine is introduced versus 15 years after
inc_22<- outputs |>
  group_by(scenario, year, id) |>
  filter(year == 2022) |>
  mutate(cases_2022 = cases,
         cohort_22 = cohort_size)

inc_37<- outputs |>
  group_by(scenario, year, id) |>
  filter(year == 2038) |>
  rename(cases_2037 = cases,
         cohort_37 = cohort_size) |>
  ungroup() |>
  select(cases_2037, cohort_37, scenario, id)

inc<- merge(inc_22, inc_37, by = c('id', 'scenario'))

inc<- inc |>
  group_by(scenario, id) |>
  summarise(cases_2022 = sum(cases_2022),
            cases_2037 = sum(cases_2037),
            cohort_22 = sum(cohort_22),
            cohort_37 = sum(cohort_37),
            .groups = 'keep')

inc<- data.table(inc)
inc<- inc[ scenario == 'proxy', scenario:= 'Routine']
inc<- inc[scenario == 'no-vaccination', scenario:= 'Control']

saveRDS(inc, 'inc.rds')


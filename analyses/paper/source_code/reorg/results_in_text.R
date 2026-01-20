#########################################################
## title  results_in_text
## author  Lydia Haile
## purpose number plug vaccine impact paper
#########################################################

# read inputs
source('J:/VIMC/VIMC_malaria/analyses/paper/source_code/paper_functions.R')
vax<- readRDS('analyses/paper/inputs/vax.rds') #results saved


# abstract: results section-----------------------------------------------------------------------
africa<- vax |>
    group_by(scenario, run_id) |>
    filter(year <= 2050) |>
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

# cases and deaths averted 
## With a routine vaccination scenario introduced in 2023, we estimate that vaccines will avert: --------------
get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# these numbers present X% of cases and deaths
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))


## through 2100, this represents X% of cases and X% of deaths: ------------------------------
africa<- vax |>
    group_by(scenario, run_id) |>
    filter(year <= 2050) |>
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

get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))

## The impact was greatest in children under 5: ----------------------------------------
africa<- vax |>
    group_by(scenario, run_id) |>
  filter(age <= 5) |>
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

get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))




# Results section: -------------------------------------------------------------------------------------
## Paragraph 1: under a baseline vaccine scenario:------------------------------------------------------
  bl<- vax |>
    filter(year %in%  c(2023:2050)) |>
    group_by(scenario,run_id) |>
    summarise(cases_baseline = sum(cases_novax, na.rm = TRUE), # pull ivory coast site file
              deaths_baseline = sum(deaths_novax, na.rm= TRUE),
              .groups = 'keep')

get_values(bl |> filter(scenario == 'Routine') |> pull(cases_baseline))
get_values(bl |> filter(scenario == 'Routine') |> pull(deaths_baseline))

# Under a routine vaccine scenario, X cases and X deaths will be averted through 2050
africa<- vax |>
  filter(year <= 2050) |>
    group_by(scenario, run_id) |>
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

get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# these numbers present X% of cases and deaths through 2050
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))

# through 2100, this represents an X% decrease in cases and deaths
africa<- vax |>
  filter(year <= 2050) |>
    group_by(scenario, run_id) |>
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

get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))


## Results by age (paragraph 2) -----------------------------------------------------------------------
#counts through 2100
table_2<- vax_age |>
  filter(scenario== 'Routine') |>
  group_by(scenario, age, run_id) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            dalys_averted = sum(dalys_averted),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, age) |>
    summarise(
      cases_mean = mean(cases_averted, na.rm = TRUE),
      cases_lower = quantile(cases_averted, 0.025, na.rm = TRUE),
      cases_upper = quantile(cases_averted, 0.975, na.rm = TRUE),
      deaths_mean = mean(deaths_averted, na.rm = TRUE),
      deaths_lower = quantile(deaths_averted, 0.025, na.rm = TRUE),
      deaths_upper = quantile(deaths_averted, 0.975, na.rm = TRUE),
      .groups= 'keep'
    ) |>
    unique()

# percentages by age through 2100
percents<- vax_age |>
  filter(scenario== 'Routine') |>
  filter(year >= 2023) |>
  group_by(scenario, age, run_id) |>
  summarise(cases_averted = sum(cases_averted,na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            dalys_averted = sum(dalys_averted, na.rm= TRUE),
            cohort_size = sum(cohort_size, na.rm= TRUE),
            cases_novax = sum(cases_novax, na.rm = TRUE),
            deaths_novax = sum(deaths_novax, na.rm = TRUE),
            dalys_novax = sum(dalys_novax, na.rm = TRUE),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, age) |>
    mutate(percent_cases_averted = cases_averted/cases_novax,
           percent_deaths_averted = deaths_averted/ deaths_novax,
           percent_dalys_averted = dalys_averted/ dalys_novax) |>
    unique() |>
  as.data.table()

# through 2100, X cases will be averted in children under 5:
print(table_2)

# This translates into X% in each age group:
get_values(percents |> filter(scenario == 'Routine'& age== '[0,5]') |> pull(percent_cases_averted))
get_values(percents |> filter(scenario == 'Routine'& age== '(5,15]') |> pull(percent_cases_averted))
get_values(percents |> filter(scenario == 'Routine'& age== '(15,101]') |> pull(percent_cases_averted))


#deaths through 2100:
print(table_2)

# This translates into X% in each age group:
get_values(percents |> filter(scenario == 'Routine'& age== '[0,5]') |> pull(percent_deaths_averted))
get_values(percents |> filter(scenario == 'Routine'& age== '(5,15]') |> pull(percent_deaths_averted))
get_values(percents |> filter(scenario == 'Routine'& age== '(15,101]') |> pull(percent_deaths_averted))


## fourth paragraph: cases and deaths averted per FVP ------------------------------------
total_doses<- compile_dose_outputs('number_plug_paper', iso = 'all') |> data.table() |> filter(scenario %in% c('proxy', 'no-vaccination'))
total_doses[scenario %like% 'no-vaccination', scenario := 'Control']
total_doses<- unique(total_doses)
total_doses<- total_doses |>
 filter(year <= 2038)|>
  group_by(iso3c, scenario, run_id) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            fvp = sum(fvp),
            .groups = 'keep') |>
  mutate(cases_averted_per_fvp= cases_averted*100000/ fvp,
         deaths_averted_per_fvp = deaths_averted*1000/ fvp) |>
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

# in the first 15 years, estimate X and X cases and deaths averted per FVP 
proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(cases_mean))
proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(deaths_mean))

# in 2100, X and X cases and deaths averted:
total_doses<- compile_dose_outputs('number_plug_paper', iso = 'all') |> data.table() |> filter(scenario %in% c('proxy', 'no-vaccination'))
total_doses[scenario %like% 'no-vaccination', scenario := 'Control']
total_doses<- unique(total_doses)
total_doses<- total_doses |>
  group_by(iso3c, scenario, run_id) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            fvp = sum(fvp),
            .groups = 'keep') |>
  mutate(cases_averted_per_fvp= cases_averted*100000/ fvp,
         deaths_averted_per_fvp = deaths_averted*1000/ fvp) |>
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

proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(cases_mean))
proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(deaths_mean))




### by country --------------------------------------
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

#greatest number of cases in these countries:
get_values(by_country |> filter(scenario == 'Routine', country_name == 'Nigeria') |> pull(cases_averted))
get_values(by_country |> filter(scenario == 'Routine', country_name %like% 'Kinshasa') |> pull(cases_averted))

# greatest number of deaths in these countries:
get_values(by_country |> filter(scenario == 'Routine', country_name == 'Nigeria') |> pull(deaths_averted))
get_values(by_country |> filter(scenario == 'Routine', country_name %like% 'Kinshasa') |> pull(deaths_averted))



# discussion section:--------------------------------------------------------
##first paragraph:------------------------------------------
africa<- vax |>
  filter(year <= 2050) |>
    group_by(scenario, run_id) |>
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

# our results demonstrate a large potential impact:
get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# Despite the large magnitude:
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))


# vaccine impact results are similar to previous work in the first 15 years following vaccine intro:
total_doses<- compile_dose_outputs('number_plug_paper', iso = 'all') |> data.table() |> filter(scenario %in% c('proxy', 'no-vaccination'))
total_doses[scenario %like% 'no-vaccination', scenario := 'Control']
total_doses<- unique(total_doses)
total_doses<- total_doses |>
  filter(year<= 2038) |>
  group_by(iso3c, scenario, run_id) |>
  summarise(cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted),
            fvp = sum(fvp),
            .groups = 'keep') |>
  mutate(cases_averted_per_fvp= cases_averted*100000/ fvp,
         deaths_averted_per_fvp = deaths_averted*1000/ fvp) |>
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

proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(cases_mean))
proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(deaths_mean))

## fourth paragraph: estimated X% reduction in a 50 year period ----------------------------------
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))



# mvip comparison----------------------------------------------------------------------------------
mvip<- copy(vax)
mvip<- unique(mvip)

mvip<- mvip |>
  filter(scenario== 'Routine') |>
  filter(age %in% c(1:4)) |>
  group_by(scenario, run_id, year) |>
  summarise(cases_averted = sum(cases_averted,na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            dalys_averted = sum(dalys_averted, na.rm= TRUE),
            cohort_size = sum(cohort_size, na.rm= TRUE),
            cases= sum(cases, na.rm = TRUE),
            cases_novax = sum(cases_novax, na.rm = TRUE),
            deaths_novax = sum(deaths_novax, na.rm = TRUE),
            dalys_novax = sum(dalys_novax, na.rm = TRUE),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, run_id, year)|>
    mutate(percent_cases_averted = cases_averted/cases_novax,
           percent_deaths_averted = deaths_averted/ deaths_novax,
           percent_dalys_averted = dalys_averted/ dalys_novax) |>
    unique() |>
  as.data.table()



get_values(mvip |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(mvip |> filter(scenario == 'Routine') |> pull(deaths_averted))

get_values(mvip |> filter(scenario == 'Routine') |> pull(cases_novax))
get_values(mvip |> filter(scenario == 'Routine') |> pull(deaths_novax))

get_values(mvip |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(mvip |> filter(scenario == 'Routine') |> pull(percent_cases_averted))



ggplot(mvip[run_id == 0]) +
  geom_line(mapping = aes(x= year, y= cases_averted), color = 'blue', fill = 'blue') 

ggplot(mvip[run_id == 0]) +
  geom_line(mapping = aes(x= year, y= cases_novax), color = 'blue') +
  geom_line(mapping = aes(x= year, y= cases), color = 'red') +
  geom_line(mapping = aes(x= year, y= cases_averted), color = 'green')+
  labs(title= 'Cases modelled, 31 countries, children 1-4 years',
      subtitle= 'Red= cases in vaccine scenario, blue= cases in nonvaccine scenario, green= cases averted')


p1<- ggplot(mvip) +
  geom_line(mapping = aes(x= year, y= percent_mean))+
  geom_ribbon(mapping = aes(x= year, ymin = percent_lower, ymax= percent_upper), alpha = 0.2)+
  labs(main= 'Percent of cases averted annually, children 1-4 years', 
y= 'Percent of cases averted', x= 'Year')

p2<- ggplot(mvip) +
  geom_line(mapping = aes(x= year, y= percent_deaths_mean))+
  geom_ribbon(mapping = aes(x= year, ymin = percent_deaths_lower, ymax= percent_deaths_upper), alpha = 0.2)+
  labs(main= 'Percent of cases averted annually, children 1-4 years', 
y= 'Percent of deaths averted', x= 'Year')


ggarrange(p1, p2, ncol= 2)
mvip<- mvip |>
  filter(scenario== 'Routine') |>
  filter(age %in% c(1:4)) |>
  group_by(scenario, run_id, year) |>
  summarise(cases_averted = sum(cases_averted,na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            dalys_averted = sum(dalys_averted, na.rm= TRUE),
            cohort_size = sum(cohort_size, na.rm= TRUE),
            cases= sum(cases, na.rm = TRUE),
            cases_novax = sum(cases_novax, na.rm = TRUE),
            deaths_novax = sum(deaths_novax, na.rm = TRUE),
            dalys_novax = sum(dalys_novax, na.rm = TRUE),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, run_id, year)|>
    mutate(percent_cases_averted = cases_averted/cases_novax,
           percent_deaths_averted = deaths_averted/ deaths_novax,
           percent_dalys_averted = dalys_averted/ dalys_novax) |>
    unique() |>
  ungroup() |>
  group_by(scenario, year) |>
summarise(percent_mean= mean(percent_cases_averted),
          percent_lower= quantile(percent_cases_averted, 0.0275),
        percent_upper = quantile(percent_cases_averted, 0.975),
      percent_deaths_mean= mean(percent_deaths_averted),
    percent_deaths_lower = quantile(percent_deaths_averted, 0.0275),
  percent_deaths_upper= quantile(percent_deaths_averted, 0.975),
.groups= 'keep') |>
  as.data.table()



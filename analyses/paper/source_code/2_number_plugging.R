#########################################################
## title   2_abstract
## author  Lydia Haile
## purpose number plug abstract
#########################################################

# read inputs
source('J:/VIMC/VIMC_malaria/analyses/paper/source_code/paper_functions.R')

africa<- readRDS('analyses/paper/inputs/africa.rds')
by_country<- readRDS('analyses/paper/inputs/pop_format.rds')
table_2<- readRDS('analyses/paper/inputs/table_2.rds')
total_doses<- readRDS('analyses/paper/inputs/total_doses.rds')
pop_format<- readRDS('analyses/paper/inputs/pop_format.rds')
vax<- readRDS('analyses/paper/inputs/vax.rds')
pop_reduced<- readRDS('analyses/paper/inputs/pop_reduced.rds')

# abstract
print(africa)

# cases and deaths averted 
get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# these numbers present X% of cases and deaths
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))



# percentages by age through 2100
table_2<- vax_age |>
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

get_values(table_2 |> filter(scenario == 'Routine'& age== '[0,5]') |> pull(percent_cases_averted))
get_values(table_2 |> filter(scenario == 'Routine'& age== '(5,15]') |> pull(percent_cases_averted))
get_values(table_2 |> filter(scenario == 'Routine'& age== '(15,101]') |> pull(percent_cases_averted))


get_values(table_2 |> filter(scenario == 'Routine'& age== '[0,5]') |> pull(percent_deaths_averted))
get_values(table_2 |> filter(scenario == 'Routine'& age== '(5,15]') |> pull(percent_deaths_averted))
get_values(table_2 |> filter(scenario == 'Routine'& age== '(15,101]') |> pull(percent_deaths_averted))



# there were still X deaths on average
# duplication in rows
vax<- unique(vax)

total_deaths<- vax |>
  group_by(run_id, year, scenario) |>
  summarise(deaths = sum(deaths), .groups= 'keep')

get_values(total_deaths |> filter(scenario == 'Routine', year == 2100) |> pull(deaths))

total_deaths<- vax |>
  group_by(run_id, year, scenario) |>
  summarise(deaths = sum(deaths_novax), .groups= 'keep')


# results: overall impact
  bl<- vax |>
    filter(year %in%  c(2023:2050)) |>
    group_by(scenario,run_id) |>
    summarise(cases_baseline = sum(cases_novax, na.rm = TRUE), # pull ivory coast site file
              deaths_baseline = sum(deaths_novax, na.rm= TRUE),
              .groups = 'keep')


get_values(bl |> filter(scenario == 'Routine') |> pull(cases_baseline))
get_values(bl |> filter(scenario == 'Routine') |> pull(deaths_baseline))


#results section 2

#results section 3

#discussion
  bl<- vax |>
    filter(year %in% c(2023:2050)) |>
    group_by(scenario,run_id) |>
    summarise(cases_baseline = sum(cases_novax, na.rm = TRUE), # pull ivory coast site file
              deaths_baseline = sum(deaths_novax, na.rm= TRUE),
              .groups = 'keep')

get_values(bl |> filter(scenario == 'Routine') |> pull(cases_baseline))
get_values(bl |> filter(scenario == 'Routine') |> pull(deaths_baseline))


# cases and deaths averted 
# make sure to check year and age range of the input dataset
get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# these numbers present X% of cases and deaths
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))


total_deaths<- vax |>
  group_by(run_id, year, scenario) |>
  summarise(deaths = sum(deaths), .groups= 'keep')

get_values(total_deaths |> filter(scenario == 'Routine', year == 2100) |> pull(deaths))



# by country
get_values(by_country |> filter(scenario == 'Routine', country_name == 'Nigeria') |> pull(cases_averted))
get_values(by_country |> filter(scenario == 'Routine', country_name %like% 'Kinshasa') |> pull(cases_averted))
get_values(by_country |> filter(scenario == 'Routine', country_name == 'Nigeria') |> pull(deaths_averted))
get_values(by_country |> filter(scenario == 'Routine', country_name %like% 'Kinshasa') |> pull(deaths_averted))


# results section results per dose (average and IQR)
proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(cases_mean))

proxy<- get_values(total_doses |> filter(scenario == 'proxy') |> pull(deaths_mean))

# switch over the total doses dataset depending on time period

# by age
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


  get_values(table_2 |> filter(age== '[0, 5]') |> pull(cases_averted))

table_2 |> filter(age== '(5,15]')

table_2 |> filter(age== '(15,101]')



# mvip comparison
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



# OK this explains it-- the much smaller percentage of cases and deaths averted in 1-4 year olds is due to 
# deflated coverage-- vaccine coverage scales up to full coverage in like 2060, and by this point the vaccine is averting upwards of 20-35% of cases in this target age group
# this should be made clear in the  text, but should probably check this with Lucy before presenting this exactly as it

# value is 5 percent through 2027 which feels too low? 
# 6 percent through 2030 which is seems too low?
# but not a bug in the inputs
#surprised that cases averted are not higher in this age group and instead approximate that of general population
# maybe pull a few more figures but otherwise wait until Lucy is back




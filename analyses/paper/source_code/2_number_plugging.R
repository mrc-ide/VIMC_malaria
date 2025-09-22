#########################################################
## title   2_abstract
## author  Lydia Haile
## purpose number plug abstract
#########################################################

# read inputs

africa<- readRDS('analyses/paper/inputs/africa.rds')
by_country<- readRDS('analyses/paper/inputs/pop_format.rds')
table_2<- readRDS('analyses/paper/inputs/table_2.rds')
total_doses<- readRDS('analyses/paper/inputs/total_doses.rds')
pop_format<- readRDS('analyses/paper/inputs/pop_format.rds')
vax<- readRDS('analyses/paper/inputs/vax.rds')
inc<- readRDS('analyses/paper/inputs/')
# abstract
print(africa)

# cases and deaths averted 
get_values(africa |> filter(scenario == 'Routine') |> pull(cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(deaths_averted))

# these numbers present X% of cases and deaths
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_cases_averted))
get_values(africa |> filter(scenario == 'Routine') |> pull(percent_deaths_averted))

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
    filter(year <= 2050) |>
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
    group_by(scenario,run_id) |>
    summarise(cases_baseline = sum(cases_novax, na.rm = TRUE), # pull ivory coast site file
              deaths_baseline = sum(deaths_novax, na.rm= TRUE),
              .groups = 'keep')

get_values(bl |> filter(scenario == 'routine') |> pull(cases_baseline))
get_values(bl |> filter(scenario == 'routine') |> pull(deaths_baseline))


# cases and deaths averted 
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

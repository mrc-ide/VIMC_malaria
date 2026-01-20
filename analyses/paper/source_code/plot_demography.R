# plot changes in demography
# plot how varyign demography may impact model results in a high transmission setting (Congo)
library(tidyr)
library(data.table)
library(vimcmalaria)
library(ggpubr)
library(dplyr)

# objects for plotting
cols<- c('#CE5137', '#1B4F72', '#45B39D', '#BA4A00')

plotting_theme<- theme_bw(base_size = 14) +
    theme( legend.position = 'bottom',
           strip.text.x = element_text(size = rel(0.8)),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
           #text = element_text(family= 'TT Arial'),
           axis.ticks.y= element_blank(),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# read the unscaled outputs
fixed<- rbindlist(readRDS('analyses/paper/source_code/reorg/COD_fixed_demography.rds')) |> 
  mutate(description = 'fixed') |>
  rename(run_id = parameter_draw) |>
  mutate(scenario = ifelse(scenario == 'proxy', 'Routine', scenario)) |>
  data.table()

dynamic<- rbindlist(readRDS('analyses/paper/source_code/reorg/COD_dynamic_output_dynamic_demography.rds')) |>
  mutate(description = 'dynamic') |> 
  rename(run_id = parameter_draw) |>
  mutate(scenario = ifelse(scenario == 'proxy', 'Routine', scenario)) |>
  data.table()

default<- rbind(fixed, dynamic, fill = TRUE)
total<- default |> filter(description == 'dynamic')


# fix the population from 2025 onwards for the fixed outputs
total_fixed<- default |>
  filter(description == 'fixed') |>
  mutate(clinical = cases/cohort_size,
  mort= deaths/cohort_size)

# pull age/sex/country specific cohort size to merge onto original dataset and recalculate rates with
initial_demo<- default |> 
  filter(year== 2025, run_id == 0) |>
  select(age, country, country_name, cohort_size) |>
  unique() |>
  rename(cohort_size_2025 = cohort_size) # duplicates based on parameter_id 

tot<- merge(total_fixed, 
  initial_demo, 
  by = c('age', 'country', 'country_name'), 
  allow.cartesian = T)

tot<- tot |>
  mutate(cohort_size = if_else(year > 2025, cohort_size_2025, cohort_size)) |>
  mutate(cases = clinical* cohort_size,
  deaths= mort * cohort_size) |>
  select(-clinical, -mort) 

tot<- rbind(tot, total, fill =TRUE)

# rerun the same plots
  novax<- tot |> filter(scenario == 'no-vaccination') |>
    rename(cases_novax = cases,
           deaths_novax = deaths,
           dalys_novax = dalys) |>
    select(year, age, run_id, description, country, cases_novax, deaths_novax, dalys_novax)

  vax<- tot |> filter(scenario == 'Routine')
  vax<- merge(vax, novax, by = c('country', 'age', 'year', 'run_id', 'description'))

  vax<- vax |>
    mutate(cases_averted = cases_novax- cases,
           deaths_averted = deaths_novax- deaths,
           dalys_averted = dalys_novax - dalys)

fig_1<- vax |>
  group_by(scenario,year, run_id, country, description) |>
  summarise(cases_averted = sum(cases_averted, na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            .groups = 'keep') |>
  ungroup() |>
  group_by(scenario, run_id) |>
  mutate(cumulative_cases = cumsum(cases_averted),
         cumulative_deaths = cumsum(deaths_averted)) |>
  data.table() |>
  ungroup() |>
  group_by(scenario, year, country, description) |>
  summarise(cases_mean = mean(cases_averted, na.rm= TRUE),
        cases_lower = quantile(cases_averted, 0.025, na.rm= TRUE),
      cases_upper =  quantile(cases_averted, 0.975, na.rm= TRUE),
      cases_cumulative_mean = mean(cumulative_cases, na.rm= TRUE),
      cases_cumulative_lower = quantile(cumulative_cases, 0.025, na.rm= TRUE),
      cases_cumulative_upper =  quantile(cumulative_cases, 0.975, na.rm= TRUE),
      deaths_mean = mean(deaths_averted, na.rm= TRUE),
      deaths_lower = quantile(deaths_averted, 0.025, na.rm= TRUE),
    deaths_upper =  quantile(deaths_averted, 0.975, na.rm= TRUE),
    deaths_cumulative_mean = mean(cumulative_deaths, na.rm= TRUE),
    deaths_cumulative_lower = quantile(cumulative_deaths, 0.025, na.rm= TRUE),
    deaths_cumulative_upper =  quantile(cumulative_deaths, 0.975, na.rm= TRUE), .groups = 'keep')


p1<- ggplot(fig_1) +
  geom_smooth(aes(x= year, y= cases_mean/1000000, color= scenario), method= 'loess', span= 0.08, se= FALSE) +
  geom_ribbon(mapping = aes(x= year, ymin= cases_lower/1000000, ymax= cases_upper/1000000, fill= scenario), alpha = 0.25) +
  facet_wrap(~ description ) +
  scale_color_manual(values = '#CE5137') +
  scale_fill_manual(values = '#CE5137') +
  plotting_theme +
    theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = '',
    y = "Cases averted (millions)",
    x = ""
  )

p2<- ggplot(fig_1) +
  geom_smooth(aes(x= year, y= deaths_mean/1000, color= scenario), method= 'loess', span= 0.08, se= FALSE) +
  geom_ribbon(mapping = aes(x= year, ymin= deaths_lower/1000, ymax= deaths_upper/1000, fill= scenario), alpha = 0.25) +
  facet_wrap(~description) +
  scale_color_manual(values = '#1B4F72') +
  scale_fill_manual(values = '#1B4F72') +
  plotting_theme +
    theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(title = '',
    y = "Deaths averted (thousands)",
    x = "Year"
  )

ggarrange(p1, p2, ncol= 2)


vax_age<- copy(vax)
vax_age$age<- cut(vax_age$age, breaks = c(0, 5, 15, 101), include.lowest = TRUE)
table_2<- vax_age |>
  filter(scenario== 'Routine') |>
  group_by(scenario, age, year, run_id, country, description) |>
  summarise(cases_averted = sum(cases_averted,na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            cohort_size = sum(cohort_size, na.rm= TRUE),
            .groups = 'keep') |>
  data.table()  |>
    ungroup() |>
    group_by(scenario, age, year, country, description) |>
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



p3<- ggplot(table_2) +
  geom_smooth(mapping = aes(x= year, y= cases_mean, color = age), method= 'loess', span = 0.1, se= FALSE) +
  geom_ribbon(aes(x= year, ymin = cases_lower, ymax = cases_upper, fill= age) , alpha = 0.1) +
  facet_wrap(~description ) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  xlim(2020, 2100) +
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
    theme(legend.position = 'none') +
  labs(title = '',
       x= '',
       y= 'Cases averted')

p4<- ggplot(table_2) +
  geom_smooth(mapping = aes(x= year, y= deaths_mean, color = age), method= 'loess', span= 0.1, se= FALSE) +
  geom_ribbon(aes(x= year, ymin = deaths_lower, ymax = deaths_upper, fill= age) , alpha = 0.1) +
    facet_wrap(~description) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  xlim(2020, 2100)+
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
  labs(title = '',
       x= 'Year',
       y= 'Deaths averted')


# # these plots look very similar? the fixed demography plots look slightoly flatter but not by much
# # probably because I've not removed the changing age structure over the century (all-age)
# # maybe run the same plot, but instead of using the UN pop values, fix the population from 2000 onwards ------------------------
total_fixed<- total |>
  filter(description == 'fixed') |>
  mutate(clinical = cases/cohort_size,
  mort= deaths/cohort_size) |>
  select(-cohort_size)

# pull age/sex/country specific cohort size to merge onto original dataset and recalculate rates with
initial_demo<- total |> 
  filter(year== 2000, run_id == 0) |>
  select(age, country, country_name, cohort_size) |>
  unique()

tot<- merge(total_fixed, initial_demo, by = c('age', 'country', 'country_name'), allow.cartesian = T)
tot<- tot |>
  mutate(cases = clinical* cohort_size,
  deaths= mort * cohort_size) |>
  select(-clinical, -mort)


# rerun the same plots
  novax<- tot |> filter(scenario == 'no-vaccination') |>
    rename(cases_novax = cases,
           deaths_novax = deaths,
           dalys_novax = dalys) |>
    select(year, age, run_id, description, country, cases_novax, deaths_novax, dalys_novax)

  vax<- tot |> filter(scenario == 'Routine')
  vax<- merge(vax, novax, by = c('country', 'age', 'year', 'run_id', 'description'))

  vax<- vax |>
    mutate(cases_averted = cases_novax- cases,
           deaths_averted = deaths_novax- deaths,
           dalys_averted = dalys_novax - dalys)

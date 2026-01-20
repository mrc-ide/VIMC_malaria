##########################################################
## title  figures_SI
## author  Lydia Haile
## purpose figures in SI in order
#########################################################

vax<- readRDS('analyses/paper/inputs/vax.rds')
cols<- c('#CE5137', '#1B4F72', '#45B39D', '#BA4A00')

plotting_theme<- theme_bw(base_size = 14) +
    theme( legend.position = 'bottom',
           strip.text.x = element_text(size = rel(0.8)),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.1),
           #text = element_text(family= 'TT Arial'),
           axis.ticks.y= element_blank(),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# figure S2 --------------------------------------------------------------

# plot vaccine coverage across all countries
country_names<- vax |>
  rename(iso3c = country) |>
  select(iso3c, country_name) |>
  unique()

proxy_scenario<- readRDS('src/process_inputs/proxy_scenario.rds')
proxy_scenario<- proxy_scenario |>
  mutate(scenario= 'proxy') |>
  select(year, coverage, booster, iso3c, scenario) |>
  inner_join(country_names)

# merge on country name
cov<- data.table(proxy_scenario)
cov<- melt(cov, id.vars= c('year', 'country_name'), measure.vars = c('coverage', 'booster'), value.name = 'cov') |>
  rename(series = variable) |>
  mutate(series = ifelse(series == 'coverage', 'primary', 'booster')) |>
  rename(coverage = cov) |>
  filter(series == 'primary')

  figure_s2<- ggplot(cov, mapping = aes(x= year, y= coverage, color= series)) +
    geom_line()+
    facet_wrap(~country_name)+
   # geom_vline(mapping= aes(xintercept= 2022), linetype= 'dotted')+
    geom_vline(mapping= aes(xintercept= 2030), linetype= 'dotted')+
    scale_color_manual(values = cols) +
    plotting_theme +
   theme(legend.position = 'none') +
       labs(y= 'Vaccine coverage (first three doses)', x= 'Year')

# figure S3 --------------------------------------------------------------
pops<- rbindlist(lapply(iso3cs, read_pop))
pops<- pops |>
  unique(by= c('name_1', 'urban_rural'))|>
  data.table()
pops<- pops[, run_model := as.character(run_model)]
pops<- pops[run_model == TRUE, run_model := 'Introduced']
pops<- pops[run_model == FALSE, run_model := 'Not introduced']
pop_reduced<- remove_urbanicity(pops) |>
  mutate(id = paste0(name_1, '_', iso3c))

pop_reduced[id %like% 'Southern Nations', id:= 'Southern Nations, Nationalities and Peoples_ETH']
pop_reduced[id %like% 'Nasarawa_NGA', id:= 'Nassarawa_NGA']

mapdat_admin<- merge(admin1s |> select(id, geometry), pop_reduced, by = 'id', all.x= TRUE)
mapdat_admin<- st_as_sf(mapdat_admin)

# do some cleaning of trhe admin1 units
mapdat_admin <- mapdat_admin |>
  mutate(run_model = ifelse(id %like% 'GHA', 'Introduced', run_model)) |>
  mutate(run_model = ifelse(id %like% 'NGA', 'Introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'CMR' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'TGO' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'TZA' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'ZMB' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'SDN' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'SLE' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(is.na(run_model), 'Not introduced', run_model))

figure_s3 <- ggplot() +
  geom_sf(data = countries_africa) +
  geom_sf(data = countries, color = 'black', size= 1) +
  geom_sf(data = border_admins) +
  geom_sf(data = borders_inner, color = "black", size = 1, lwd= 1) +
  geom_sf(data= mapdat_admin, aes(fill = run_model), alpha = 0.8, show.legend = TRUE, linewidth = 0.005) +
  scale_fill_manual(values = cols) +
  labs(fill = "Vaccine introduced") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())


# Figure S4: plot of cases + deaths averted by country -------------------------------------------------------------------
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

p1 <- ggplot(data = by_country, aes(x = reorder(country, -cases_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = cases_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = cases_lower, ymax = cases_upper), position = position_dodge(0.8)) +
  plotting_theme +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  labs(
    title = "",
    x = "Country",
    y = "Cases averted"
  )


p2 <- ggplot(data = by_country, aes(x = reorder(country, -deaths_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = deaths_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = deaths_lower, ymax = deaths_upper), position = position_dodge(0.8)) +
  plotting_theme +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  labs(
    title = "",
    x = "Country",
    y = "Deaths averted"
  )


 ggarrange(p1, p2, ncol = 2)


# plot cases averted by country per FVP
p3<- ggplot(data = total_doses, aes(x = reorder(country, -cases_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = cases_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = cases_lower, ymax = cases_upper), position = position_dodge(0.8)) +
  plotting_theme +
  scale_fill_manual(values = cols)+
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))+
  labs(title = '',
       x= 'Country',
       y= 'Cases averted per 100,000 \n fully vaccinated people')


p4<- ggplot(data = total_doses, aes(x = reorder(country, -deaths_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = deaths_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = deaths_lower, ymax = deaths_upper), position = position_dodge(0.8)) +
  plotting_theme +
  scale_fill_manual(values = cols)+
    theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  labs(title = '',
       x= 'Country',
       y= 'Deaths averted per 100,000 \n fully vaccinated people')

figure_s4<- ggarrange(p1, p2, p3, p4, ncol =2, nrow= 2)


# figure S5 population ---------------------------------------------------
# Figure S5- population size over time 
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

p5<- ggplot(table_2) +
  geom_line(mapping = aes(x= year, y= cohort_size, color = age)) +
  #geom_ribbon(aes(x= year, ymin = deaths_lower, ymax = deaths_upper, fill= age) , alpha = 0.1) +
  #geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
    theme(legend.position = 'none')+
  labs(title = '',
       x= 'Year',
       color= 'Age',
       y= 'Cohort size')

props<- table_2 |>
  group_by(year) |>
  summarise(total_pop = sum(cohort_size))
table_2<- merge(table_2, props, by = 'year')
table_2<- table_2 |>
  mutate(prop_population = cohort_size/total_pop)

p6<- ggplot(table_2) +
  geom_line(mapping = aes(x= year, y= prop_population, color = age)) +
  #geom_ribbon(aes(x= year, ymin = deaths_lower, ymax = deaths_upper, fill= age) , alpha = 0.1) +
  #geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
  labs(title = '',
       x= 'Year',
       color = 'Age',
       y= 'Proportion of total population')

figure_s5<- ggarrange(p5, p6, ncol = 2)



# demography analysis plots: figures S6-S9
# objects for plotting

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


# Figure S6 --------------------------------------------------------------
#Figure S6: cases averted by year in fixed vs demography model runs
figure_s6<- ggplot(fig_1) +
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



# Figure S7 --------------------------------------------------------------
#Figure S7: deaths averted per year in fixed vs dynamic demography runs
figure_s7<- ggplot(fig_1) +
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



# Figure S8 --------------------------------------------------------------
# figure S8: cases averted by age group, fixed vs. dynamic demography
figure_s8<- ggplot(table_2) +
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



# Figure S9 --------------------------------------------------------------
#Figure S9: deaths averted by age group, fixed vs dynamic demography
figure_s9<- ggplot(table_2) +
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




# Figure S10 -------------------------------------------------------------
# Figure S10: age pattern for Nigeria and Ethiopia (high and low transmission)
  country_examples <- vax |>
  group_by(scenario, country, country_name, age, run_id) |>
  summarise(
    cases_averted = sum(cases_averted),
    deaths_averted = sum(deaths_averted),
    dalys_averted = sum(dalys_averted),
    .groups = "keep"
  ) |>
  data.table() |>
  ungroup() |>
  group_by(scenario, country, age) |>
  mutate(
    cases_mean = mean(cases_averted, na.rm = TRUE),
    cases_lower = quantile(cases_averted, 0.025, na.rm = TRUE),
    cases_upper = quantile(cases_averted, 0.975, na.rm = TRUE),
    deaths_mean = mean(deaths_averted, na.rm = TRUE),
    deaths_lower = quantile(deaths_averted, 0.025, na.rm = TRUE),
    deaths_upper = quantile(deaths_averted, 0.975, na.rm = TRUE)
  ) |>
  arrange(-cases_averted) |>
  filter(!scenario == "Control") |>
  data.table()



p1<- ggplot(data = country_examples[country_name == 'Nigeria'] ) +
    geom_line(aes(x = age, y = cases_mean, color = scenario))+
  geom_ribbon(aes(x= age, ymin = cases_lower, ymax = cases_upper, fill= scenario), alpha = 0.1) +
    scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
      scale_color_manual(values = cols)+
labs(y = 'Cases', x= 'Age', color = '', fill= '') +
plotting_theme +
    theme( legend.position = 'none')


p2<- ggplot(data = country_examples[country_name == 'Ethiopia']) +
    geom_line(aes(x = age, y = cases_mean, color = scenario))+
  geom_hline(yintercept= 0, linetype= 'dashed') +
  geom_ribbon(aes(x= age, ymin = cases_lower, ymax = cases_upper, fill= scenario), alpha = 0.1) +
    scale_y_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = cols)+
labs(y = 'Cases', x= 'Age', color = '', fill= '') +
plotting_theme +
  theme( legend.position = 'none')

figure_s10<- ggarrange(p1, p2, ncol = 2)



# Figure S11 -------------------------------------------------------------
##supplemental figures10: age pattern pre- and post vaccination
age_pattern <- total |>
  group_by(scenario, age, run_id) |>
  summarise(
    cases = sum(cases, na.rm= TRUE),
    deaths = sum(deaths, na.rm= TRUE),
    .groups = "keep"
  ) |>
  ungroup() |>
  group_by(scenario, age) |>
  summarise(cases_mean = mean(cases),
           cases_min = quantile(cases, 0.025, na.rm= TRUE),
           cases_max= quantile(cases, 0.975, na.rm= TRUE),
           deaths_mean = mean(deaths),
           deaths_min = quantile(deaths, 0.025, na.rm = TRUE),
           deaths_max= quantile(deaths, 0.975, na.rm= TRUE),
          .groups= 'keep' ) |>
  mutate(scenario = ifelse(scenario== 'no-vaccination', 'No vaccination', scenario))


p1<- ggplot(data = age_pattern ) +
    geom_line(aes(x = age, y = cases_mean, color = scenario))+
  geom_ribbon(aes(x= age, ymin = cases_min, ymax = cases_max, fill= scenario), alpha = 0.1) +
    scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = cols)+
labs(y = 'Cases', x= 'Age', color= 'Scenario', fill= 'Scenario') +
plotting_theme

p2<- ggplot(data = age_pattern) +
    geom_line(aes(x = age, y = deaths_mean, color = scenario))+
  geom_ribbon(aes(x= age, ymin = deaths_min, ymax = deaths_max, fill= scenario), alpha = 0.1) +
    scale_y_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = cols)+
labs(y = 'Deaths', x= 'Age') +
plotting_theme +
  theme( legend.position = 'none')


figure_s11<- ggarrange(p1, p2, ncol = 2)



# Print all plots --------------------------------------------------------
# print plots collected to collate in SI
# remaining figures not devised hree are pulled from previous work and cited appropriately
print(figure_s2)
print(figure_s3)
print(figure_s4)
print(figure_s5)
print(figure_s6)
print(figure_s7)
print(figure_s8)
print(figure_s9)
print(figure_s10)
print(figure_s11)



# Plot cases averted per FVP against PFPR in 2024 by admin unit
# keep source code but will not include in paper


# calculate cases averted per FVP at the admin-1 level + merge it upon PFPR in 2024
#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_dose_outputs<- function(descrip, iso){

  completed<- completed_reports('postprocessing')

  completed<- completed[description == 'number_plug_paper'] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)

  pull_dose_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- rbindlist(readRDS(paste0('archive/postprocessing/', directory_name, '/dose_output.rds')))

    return(output)
  }
  
  outputs<- rbindlist(lapply(c(1:nrow(completed)), pull_dose_output, map = completed))
}

output_modify<- copy(outputs)


output_modify <- output_modify |>
  filter(year %in% c(2023:2038)) |>
group_by(scenario, site_ur) |>
summarise(cases_averted= sum(cases_averted),
  deaths_averted = sum(deaths_averted),
fvp= sum(fvp),
.groups = 'keep')


#pull in prevalence data from 2024 and merge this on

prev_data<- rbindlist(lapply(iso3cs, extract_site_data))
prev_data<- prev_data |>
  rename(site_ur= id)

output_modify<- merge(output_modify, prev_data, by= c('site_ur'))
output_modify <- output_modify |>
mutate(cases_averted_per_fvp = cases_averted/ fvp,
deaths_averted_per_fvp = deaths_averted/fvp)


ggplot(output_modify, mapping = aes(x= pfpr_2024, y= cases_averted_per_fvp))+
  geom_point()

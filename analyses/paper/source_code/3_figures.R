#########################################################
## title   3_figures
## author  Lydia Haile
## purpose generate figures
###########################################################################


# figure 1: line plot of cases + deaths averted over time---------------------------------------------------------
fig_1<- vax |>
  filter(run_id == 0) |>
  group_by(scenario,year, run_id) |>
  summarise(cases_averted = sum(cases_averted, na.rm= TRUE),
            deaths_averted = sum(deaths_averted, na.rm = TRUE),
            .groups = 'keep') |>
  ungroup() |>
  group_by(scenario, run_id) |>
  mutate(cumulative_cases = cumsum(cases_averted),
         cumulative_deaths = cumsum(deaths_averted)) |>
  data.table() |>
  ungroup() |>
  group_by(scenario, year) |>
  mutate(cases_mean = mean(cases_averted, na.rm= TRUE),
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
    deaths_cumulative_upper =  quantile(cumulative_deaths, 0.975, na.rm= TRUE))


p1<- ggplot(fig_1) +
  geom_line(aes(x = year, y = cases_mean/1000000, color = scenario)) +
  geom_ribbon(aes(x= year, ymin= cases_lower/1000000, ymax= cases_upper/1000000, fill= scenario), alpha = 0.25) +
  scale_color_manual(values = '#CE5137') +
  scale_fill_manual(values = '#CE5137') +
  plotting_theme +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "",
    y = "Cases averted (millions)",
    x = ""
  )

p2<- ggplot(fig_1) +
  geom_line(aes(x = year, y = deaths_mean/1000, color = scenario)) +
  geom_ribbon(aes(x= year, ymin= deaths_lower/1000, ymax= deaths_upper/1000, fill= scenario), alpha = 0.25)+
  scale_color_manual(values = '#1B4F72') +
  scale_fill_manual(values = '#1B4F72') +
  plotting_theme +
    theme(legend.position = 'none') +
  +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = "Deaths averted (thousands)",
    x = "Year"
  )

ggarrange(p1, p2, nrow= 2)


p3<- ggplot(fig_1) +
  geom_line(aes(x = year, y = cases_cumulative_mean/1000000, color = scenario)) +
  geom_ribbon(aes(x= year, ymin= cases_cumulative_lower/1000000, ymax= cases_cumulative_upper/1000000, fill= scenario), alpha = 0.25) +
  scale_color_manual(values = '#CE5137') +
  scale_fill_manual(values = '#CE5137') +
  plotting_theme +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "",
    y = "Cumulative cases averted (millions)",
    x = ""
  )

p4<- ggplot(fig_1) +
  geom_line(aes(x = year, y = deaths_cumulative_mean/1000, color = scenario)) +
  geom_ribbon(aes(x= year, ymin= deaths_cumulative_lower/1000, ymax= deaths_cumulative_upper/1000, fill= scenario), alpha = 0.25)+
  scale_color_manual(values = '#1B4F72') +
  scale_fill_manual(values = '#1B4F72') +
  plotting_theme +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = "Cumulative deaths averted (thousands)",
    x = "Year")


ggarrange(p3, p4, nrow= 2)
ggarrange(p1, p3, p2, p4, nrow= 2, ncol= 2)



# supplemental figure: plot of cases + deaths averted by country -------------------------------------------------------------------
p1 <- ggplot(data = by_country, aes(x = reorder(country, -cases_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = cases_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = cases_lower, ymax = cases_upper), position = position_dodge(0.8)) +
  plotting_theme +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cases averted",
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
  labs(
    title = "Deaths averted",
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
  labs(title = 'Cases averted per 100,000 fully vaccinated people',
       x= 'Country',
       y= 'Cases averted per 100,000 fully vaccinated people')


p4<- ggplot(data = total_doses, aes(x = reorder(country, -deaths_mean), group = scenario)) +
  geom_col(position = "dodge", width = 0.8, mapping = aes(y = deaths_mean, color = scenario, fill = scenario)) +
  geom_errorbar(aes(ymin = deaths_lower, ymax = deaths_upper), position = position_dodge(0.8)) +
  plotting_theme +
  scale_fill_manual(values = cols)+
    theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Deaths averted per 100,000 fully vaccinated people',
       x= 'Country',
       y= 'Deaths averted per 100,000 fully vaccinated people')

ggarrange(p1, p2, p3, p4, ncol =2, nrow= 2)


# maps
mapdat <- total_doses |>
  left_join(countries_africa |> dplyr::select(ID_0, geometry),
            by = c("country" = "ID_0")) |>
  st_as_sf()

pop_reduced[id %like% 'Southern Nations', id:= 'Southern Nations, Nationalities and Peoples_ETH']
pop_reduced[id %like% 'Nasarawa_NGA', id:= 'Nassarawa_NGA']

mapdat_admin<- merge(admin1s |> select(id, geometry), pop_reduced, by = 'id', all.x= TRUE)
mapdat_admin<- st_as_sf(mapdat_admin)

#mapdat_admin |> filter(is.na(run_model))

#do some cleaning of trhe admin1 units
map_dt<- mapdat_admin |> filter(id %like% 'NGA')
model_dt<- pop_reduced |> filter(id %like% 'NGA') # not matching but introduced everywhere so can hardcode


setdiff(map_dt$id, model_dt$id)
setdiff(model_dt$id, map_dt$id)


mapdat_admin <- mapdat_admin |>
  mutate(run_model = ifelse(id %like% 'GHA', 'Introduced', run_model)) |>
  mutate(run_model = ifelse(id %like% 'NGA', 'Introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'CMR' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'TGO' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'TZA' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'ZMB' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'SDN' & is.na(run_model), 'Not introduced', run_model)) |>
  mutate(run_model= ifelse(id %like% 'SLE' & is.na(run_model), 'Not introduced', run_model))


#  make a plot of all of the admin-1 units modelled for study (methods) -----
p <- ggplot() +
  geom_sf(data = countries_africa) +
  geom_sf(data = countries) +
  geom_sf(data = border_admins) +
  geom_sf(data = borders_inner, color = "black", size = 1) +
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



# Figure 2: cases and deaths averted 
p1 <- ggplot() +
geom_sf(data = countries, fill = "lightgrey", color = "lightgrey") +
geom_sf(data= mapdat, aes(fill = cases_mean), color = "lightgrey", show.legend = TRUE, linewidth = 0.005) +
scale_fill_distiller(palette = "Oranges", direction = 1, labels= scales::comma) + # "RdYlBu"
#scale_fill_continuous(labels = scales::comma) +
# scale_fill_gradient(low = "#ffcccc", high = "#990000") +
labs(fill = "Cases averted per 100,000 FVP") +
theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.ticks = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank(),
panel.border = element_blank(),
plot.background = element_blank(),
panel.background = element_blank())


p2 <- ggplot() +
geom_sf(data = countries, fill = "lightgrey", color = "lightgrey") +
geom_sf(data= mapdat, aes(fill = deaths_mean), color = "lightgrey", show.legend = TRUE, linewidth = 0.005) +
scale_fill_distiller(palette = "Oranges", direction = 1, labels= scales::comma) + # "RdYlBu"
#scale_fill_continuous(labels = scales::comma) +
# scale_fill_gradient(low = "#ffcccc", high = "#990000") +
labs(fill = "Deaths averted per 100,000 FVP") +
theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.ticks = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank(),
panel.border = element_blank(),
plot.background = element_blank(),
panel.background = element_blank())

# Figure 3: by age--------------------------
p3<- ggplot(table_2) +
  geom_line(mapping = aes(x= year, y= cases_mean, color = age)) +
  geom_ribbon(aes(x= year, ymin = cases_lower, ymax = cases_upper, fill= age) , alpha = 0.1) +
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
  geom_line(mapping = aes(x= year, y= deaths_mean, color = age)) +
  geom_ribbon(aes(x= year, ymin = deaths_lower, ymax = deaths_upper, fill= age) , alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  xlim(2020, 2100)+
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
  labs(title = '',
       x= 'Year',
       y= 'Deaths averted')

ggarrange(p3, p4, nrow = 2, heights= c(1, 1.25))

# try same plot but as a stacked bar chart
# should calculate mean + cis after all calculations but update this later
stacked<- table_2 |>
  mutate(year_cut = cut(year,
                        breaks = c(1999, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2101), ordered_result = TRUE))
stacked<- stacked |>
  mutate(year_cut = as.character(year_cut)) |>
  mutate(year_cut = str_replace(year_cut, ',', '-')) |>
 # mutate(year_cut = str_replace(year_cut, "(", ""))
  #mutate(year_cut = str_replace(year_cut, ']', ''))
group_by(year_cut,age, scenario) |>
  summarise(cases_mean = sum(cases_mean),
            cases_lower = sum(cases_lower),
            cases_upper = sum(cases_upper),
            deaths_mean = sum(deaths_mean),
            deaths_lower = sum(deaths_lower),
            deaths_upper = sum(deaths_upper),
            .groups = 'keep')

p3<- ggplot() +
  geom_col(stacked, position = 'stack', mapping = aes(x= year_cut, y= cases_mean, fill = age)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  #xlim((1999,2005], (2095,2101]) +
  plotting_theme+
  theme(legend.position = 'none') +
  labs(title = '',
       x= '',
       y= 'Cases averted')

p4<- ggplot() +
  geom_col(stacked, position = 'stack', mapping = aes(x= year_cut, y= deaths_mean, fill = age)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
  theme(legend.position = 'none') +
  labs(title = '',
       x= '',
       y= 'Deaths averted')

ggarrange(p3, p4, ncol = 2)


# supplemental figure- population size over time 
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

ggarrange(p5, p6, ncol = 2)


##supplemental figure: age pattern pre- and post vaccination
age_pattern <- total |>
  group_by(scenario, age, run_id) |>
  summarise(
    cases = sum(cases, na.rm= TRUE),
    cases_min= quantile()
    deaths = sum(deaths, na.rm= TRUE),
    .groups = "keep"
  )


p1<- ggplot(data = age_pattern, aes(x = age, y = cases, color = scenario)) +
  geom_line()+
    scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = cols)+
#labs(title = 'Incident cases by age and vaccine scenario') +
plotting_theme

p2<- ggplot(data = age_pattern, aes(x = age, y = deaths, color = scenario)) +
  geom_line()+
    scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = cols)+
#labs(title = 'Deaths by age and vaccine scenario') +
plotting_theme +
  theme( legend.position = 'none')



ggarrange(p1, p2, ncol = 2)



#by admin unit---------------------------------------
# incidence pre- and post -vaccinatiopn
ggplot(data = inc, mapping = aes(x= cases_2022/cohort_22, y= cases_2037/cohort_37, color= scenario)) +
  geom_point()+
  #facet_wrap(~scenario) +
  geom_abline(slope = 1, intercept = 0) +
  plotting_theme +
  xlim(0, 1.2) +
  ylim(0, 1.2)+
  theme(legend.position = 'bottom') +
  scale_color_manual(values = cols)+
    labs(color= 'Scenario',
         x= 'Incidence rate before vaccine introduction',
         y= 'Incidence rate 15 years after vaccine introduction')



# incidence rate before vaccine is introduced versus 15 years after
inc_22<- vax |>
  group_by(scenario, year, country) |>
  filter(year == 2022) |>
  mutate(cases_2022 = cases,
         cohort_22 = cohort_size)

inc_37<- vax |>
  group_by(scenario, year, country) |>
  filter(year == 2037) |>
  rename(cases_2037 = cases,
         cohort_37 = cohort_size) |>
  ungroup() |>
  select(cases_2037, cohort_37, scenario, country)



inc<- merge(inc_22, inc_37, by = c('country', 'scenario'))

inc<- inc |>
  group_by(scenario, country) |>
  summarise(cases_2022 = sum(cases_2022),
            cases_2037 = sum(cases_2037),
            cohort_22 = sum(cohort_22),
            cohort_37 = sum(cohort_37),

            .groups = 'keep')

#inc<- inc |> filter(scenario != 'Control')
ggplot(data = inc, mapping = aes(x= cases_2022/cohort_22, y= cases_2037/cohort_37, color= scenario)) +
  geom_point()+
  #facet_wrap(~scenario) +
  geom_abline(slope = 1, intercept = 0) +
  plotting_theme +
  theme(legend.position = 'none') +
  scale_color_manual(values = cols)+
    labs(
         x= 'Incidence rate before vaccine introduction',
         y= 'Incidence rate 15 years after vaccine introduction')


#########################################################
## title   3_figures
## author  Lydia Haile
## purpose generate figures
###########################################################################
source('J:/VIMC/VIMC_malaria/analyses/paper/source_code/0_paper_functions.R')
vax<- readRDS('analyses/paper/inputs/vax.rds')



# figure 1: line plot of cases + deaths averted over time---------------------------------------------------------
fig_1<- vax |>
#  filter(run_id == 0) |>
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
  geom_smooth(aes(x= year, y= cases_mean/1000000, color= scenario), method= 'loess', span= 0.08, se= FALSE) +
  geom_ribbon(mapping = aes(x= year, ymin= cases_lower/1000000, ymax= cases_upper/1000000, fill= scenario), alpha = 0.25) +
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
  geom_smooth(aes(x = year, y = deaths_mean/1000, color = scenario), method= 'loess', span= 0.08, se= FALSE) +
  geom_ribbon(aes(x= year, ymin= deaths_lower/1000, ymax= deaths_upper/1000, fill= scenario), alpha = 0.25)+
  scale_color_manual(values = '#1B4F72') +
  scale_fill_manual(values = '#1B4F72') +
  plotting_theme +
    theme(legend.position = 'none') +
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
    y = "Cumulative cases averted \n(millions)",
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
    y = "Cumulative deaths averted \n(thousands)",
    x = "Year")


ggarrange(p3, p4, nrow= 2)

pdf('Figure_1.pdf')
p<- ggarrange(p1, p3, p2, p4, nrow= 2, ncol= 2)
print(p)
dev.off()

# Figure 2: cases and deaths averted by country -------------------------------------------------------------- 
total_doses<- compile_dose_outputs('number_plug_paper', iso = 'all') |> data.table() |> filter(scenario %in% c('proxy', 'no-vaccination'))
total_doses[scenario %like% 'no-vaccination', scenario := 'Control']
total_doses<- unique(total_doses)
total_doses<- total_doses |>
 filter(year %in% c(2023:2038))|>
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


mapdat <- total_doses |>
  left_join(countries_africa |> dplyr::select(ID_0, geometry),
            by = c("country" = "ID_0")) |>
  st_as_sf()
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

pdf('Figure_2.pdf')

fig_2<- ggarrange(p1, p2, nrow=2)

print(fig_2)

dev.off()


# Figure 3: by age-----------------------------------------------------------------------------------------------------
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

p3<- ggplot(table_2) +
  geom_smooth(mapping = aes(x= year, y= cases_mean, color = age), method= 'loess', span = 0.1, se= FALSE) +
  geom_ribbon(aes(x= year, ymin = cases_lower, ymax = cases_upper, fill= age) , alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
    geom_vline(xintercept = 2030) +
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
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_vline(xintercept = 2030) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  xlim(2020, 2100)+
  scale_y_continuous(labels = scales::comma) +
  plotting_theme+
  labs(title = '',
       x= 'Year',
       y= 'Deaths averted')

fig_3<- ggarrange(p3, p4, nrow = 2, heights= c(1, 1.25))

pdf('Figure_3.pdf')
print(fig_3)
dev.off()


# print plots all together -------------------------
print(fig_1)
print(fig_2)
print(fig_3)
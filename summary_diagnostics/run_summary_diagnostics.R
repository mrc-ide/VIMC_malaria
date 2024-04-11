# summary diagnostic pdfs
extrafont::loadfonts()

source('src/diagnostics/diagnostic_report_functions.R')
pdf('summary_diagnostics/averted_outputs_over_time.pdf', width = 12)

# cases deaths and dalys averted over time
for(iso in iso3cs){

  message(iso)


  output<- readRDS(paste0('J:/VIMC_malaria/outputs/stochastic_estimates/by_country/', iso, '.rds'))
  output<- output |>
    rename(parameter_draw = run_id)

  averted<- prepare_averted_outputs(output)
  burden<- prepare_burden_outputs(output)

  p<- ggplot(data = averted$by_year, mapping = aes(x= year, y= cases_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='Cases averted over time',
         subtitle = unique(output$country_name)) +
    plotting_theme

  print(p)

  p<- ggplot(data = averted$by_year, mapping = aes(x= year, y= deaths_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='Deaths averted over time',
         subtitle = unique(output$country_name)) +
    plotting_theme

  print(p)

  p<- ggplot(data = averted$by_year, mapping = aes(x= year, y= dalys_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='DALYs averted over time',
         subtitle = unique(output$country_name)) +
    plotting_theme

  print(p)

}

dev.off()


pdf('summary_diagnostics/averted_outputs_over_age2.pdf')

for(iso in isos){

  message(iso)


  output<- readRDS(paste0('J:/VIMC_malaria/outputs/stochastic_estimates/by_country/', iso, '.rds'))
  output<- output |>
    rename(parameter_draw = run_id)

  averted<- prepare_averted_outputs(output)
  burden<- prepare_burden_outputs(output)

 p<-  ggplot(data = averted$by_age, mapping = aes(x= age, y= cases_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='Cases averted over age',
         subtitle = unique(output$country_name)) +
    plotting_theme

print(p)

 p<-  ggplot(data = averted$by_age, mapping = aes(x= age, y= deaths_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='Deaths averted over age',
         subtitle = unique(output$country_name)) +
    plotting_theme

 print(p)

  p<- ggplot(data = averted$by_age, mapping = aes(x= age, y= dalys_averted), alpha = 0.05) +
    geom_line(aes( color = scenario, group= parameter_draw)) +
    stat_summary(fun.y = mean, geom = "line", color = "black") +
    geom_abline(slope = 0 , intercept = 0, linetype = 'dashed') +
    facet_wrap(~scenario)+
    labs(title ='DALYs averted over age',
         subtitle = unique(output$country_name)) +
    plotting_theme

  print(p)
}

dev.off()

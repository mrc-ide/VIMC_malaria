# run diagnostic report by country
parms<- orderly2::orderly_parameters(iso3c = 'NGA',
                             description = 'gavi_reruns_2025',
                             quick_run= FALSE)

source('diagnostic_report_functions.R')
library(data.table)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(scene)
library(scales)
library(wesanderson)
library(ggpubr)
library(vimcmalaria)
library(plotly)

orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(vimc_input.rds = "vimc_input.rds"))
orderly2::orderly_dependency("process_inputs", "latest(parameter:iso3c == this:iso3c)", c(site_file.rds = "site_file.rds"))



# pull in final outputs from outputs folder
orderly2::orderly_dependency("postprocessing", "latest(parameter:iso3c == this:iso3c
                                                && parameter:description == this:description
                                                && parameter:quick_run == this:quick_run)", 
                             c(final_output.rds = "final_output.rds"))

orderly2::orderly_dependency("postprocessing", "latest(parameter:iso3c == this:iso3c
                                                && parameter:description == this:description
                                                && parameter:quick_run == this:quick_run)", 
                             c(dose_output.rds = "dose_output.rds"))

vetting<- rbindlist(readRDS('final_output.rds'))
dose_output<- rbindlist(readRDS('dose_output.rds'))

# load site file --------------------------------------------------------------
site_data<- readRDS('site_file.rds')
vimc_input<- readRDS('vimc_input.rds')

coverage_data<- vimc_input$coverage_input
population<- vimc_input$population_input_all_age
population_by_age<- vimc_input$population_input_single_yr

vetting<- vetting |>
  mutate(parameter_draw = run_id) 

country_name<- unique(vetting$country_name)
averted_output<- prepare_averted_outputs(vetting)
burden_output<- prepare_burden_outputs(vetting)
doses<- prepare_dose_output(dose_output, site_data, coverage_data)

diagnostic_inputs<- list('vetting' = vetting,
                         'averted_output' = averted_output,
                         'burden_output' = burden_output,
                         'dose_output' = doses,
                         'coverage_data' = coverage_data,
                         'population' = population,
                         'site_data' = site_data,
                         'country_name' = country_name,
                         'population' = population,
                         'population_by_age' = population_by_age)

saveRDS(diagnostic_inputs, 'diagnostic_inputs.rds')


# # render report --------------------------------------------------------------

message('rendering')
  rmarkdown::render(input= 'diagnostic_report.Rmd',
                  output_file = paste0('diagnostic_report_', iso3c,'.html'),
                  output_format = 'html_document',
                  params= list('vetting' = vetting,
                               'averted_output' = averted_output,
                               'burden_output' = burden_output,
                               'dose_output' = doses,
                               'coverage_data' = coverage_data,
                               'site_data' = site_data,
                               'country_name' = country_name,
                               'population' = population,
                               'population_by_age' = population_by_age))




doses_short<- prepare_dose_output(dose_output, site_data, coverage_data, interval= 15)
doses_short<- doses_short$site_doses |> filter(scenario== 'malaria-r3-r4-default')

doses_long<- prepare_dose_output(dose_output, site_data, coverage_data, interval= 80)
doses_long<- doses_long$site_doses|> filter(scenario== 'malaria-r3-r4-default')



  p<- ggplot(data = doses_short, mapping = aes(x= pfpr, y = deaths_averted/fvp *100000, color = scenario)) +
    geom_point()+
    plotting_theme +
    theme(legend.position = 'none')+
    labs(title = 'Unscaled deaths averted per 100,000 FVP, 15-years following\nvaccine introduction, NGA',
         subtitle= 'by admin-1 unit and urbanicity, R21 routine scenario',
         x= 'Malaria Atlas Project PFPR(2-10) in 2024',
         y = 'Deaths averted per 100k FVPs')
  
  
  ggplotly(p)


    p2<- ggplot(data = doses_long, mapping = aes(x= pfpr, y = deaths_averted/fvp *100000, color = scenario)) +
    geom_point()+
    plotting_theme +
    theme(legend.position = 'none')+
    labs(title = 'Unscaled deaths averted per 100,000 FVP, 2000-2100, NGA',
           subtitle= 'by admin-1 unit and urbanicity, R21 routine scenario',
         x= 'Malaria Atlas Project PFPR(2-10) in 2024',
         y = 'Deaths averted per 100k FVPs')
  

  plots<- ggarrange(p, p2, ncol= 2)
pdf('NGA_plots.pdf')
print(plots)
dev.off()

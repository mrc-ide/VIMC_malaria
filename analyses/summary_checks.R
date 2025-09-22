# summary checks
# author: Lydia Haile
# purpose: write up quick checks of model outputs based on key issues + plot before submission
################################################################################


library(data.table)
library(vimcmalaria)
library(dplyr)
library(ggpubr)
library(ggforce)
library(scales)

coverage <- read.csv("src/process_inputs/vimc_inputs/vaccine_coverage/coverage_202409malaria-1_malaria-r3-r4-default.csv")
iso3cs <- unique(coverage$country_code)
source("src/diagnostics/diagnostic_report_functions.R")

descrip<- 'number_plug_paper' #set of runs to run diagnostics for
filepath<- paste0('diagnostics/summary_diagnostics/', descrip, '/') # filepath to save diagnostics to
dir.create(filepath)

# pull outputs to plot
final <-compile_final_outputs('number_plug_paper')
final<- unique(final)
saveRDS(final, paste0(filepath, 'final_outputs.rds'))

pull_wmr <- function(iso3c) {
  site <- site::fetch_site(iso3c)
  wmr <- site$cases_deaths

  return(wmr)
}

wmr <- rbindlist(lapply(iso3cs, pull_wmr))



# plotting functions
plot_incidence <- function(country_nm) {
  message(country_nm)
  test <- final |>
    filter(country_name == country_nm) |>
    group_by(scenario, year, run_id) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      dalys = sum(dalys),
      cohort_size = sum(cohort_size),
      .groups = "keep"
    ) |>
    data.table()


  p1 <- ggplot() +
    geom_line(data = test[run_id == 0], mapping = aes(x = year, y = cases / cohort_size, color = scenario)) +
    geom_line(data = test[run_id!= 0], mapping = aes(x= year, y= cases/cohort_size, color = scenario, group = run_id), alpha = 0.1)  +
    #stat_summary(data = test[run_id!= 0], mapping = aes(x= year, y= cases/cohort_size, color = scenario), fun.y = median, geom = "line") +
    plotting_theme +
    labs(
      title = "Incidence over time",
      subtitle = country_nm
    )

  test <- final |>
    filter(country_name == country_nm) |>
    group_by(scenario, age, run_id) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      dalys = sum(dalys),
      cohort_size = sum(cohort_size),
      .groups = "keep"
    ) |>
    data.table()


  p2 <- ggplot() +
    geom_line(data = test[run_id == 0], mapping = aes(x = age, y = cases / cohort_size, color = scenario)) +
    geom_line(data = test[run_id!= 0], mapping = aes(x= age, y= cases/cohort_size, color = scenario, group = run_id), alpha = 0.1)  +
    #stat_summary(data = test[run_id!= 0], mapping = aes(x= age, y= cases/cohort_size, color = scenario), fun.y = median, geom = "line") +
    plotting_theme +
    labs(
      title = "Incidence over age",
      subtitle = country_nm
    )


  plots <- ggarrange(p1, p2, ncol = 2)

  print(plots)
}


plot_deaths <- function(country_nm) {
  message(country_nm)
  test <- final |>
    filter(country_name == country_nm) |>
    group_by(scenario, year, run_id) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      dalys = sum(dalys),
      cohort_size = sum(cohort_size),
      .groups = "keep"
    ) |>
    data.table()


  p1 <- ggplot() +
    geom_line(data = test[run_id == 0], mapping = aes(x = year, y = deaths / cohort_size, color = scenario)) +
    geom_line(data = test[run_id!= 0], mapping = aes(x= year, y= deaths/cohort_size, color = scenario, group = run_id), alpha = 0.1)  +
    #stat_summary(data = test[run_id!= 0], mapping = aes(x= year, y= deaths/cohort_size, color = scenario), fun.y = median, geom = "line") +
    facet_wrap(~scenario) +
    plotting_theme +
    labs(
      title = "Mortality over time",
      subtitle = country_nm
    )

  test <- final |>
    filter(country_name == country_nm) |>
    group_by(scenario, age, run_id) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      dalys = sum(dalys),
      cohort_size = sum(cohort_size),
      .groups = "keep"
    ) |>
    data.table()


  p2 <- ggplot() +
    geom_line(data = test[run_id == 0], mapping = aes(x = age, y = deaths / cohort_size, color = scenario)) +
    geom_line(data = test[run_id!= 0], mapping = aes(x= age, y= deaths/cohort_size, color = scenario, group = run_id), alpha = 0.1)  +
    #stat_summary(data = test[run_id!= 0], mapping = aes(x= age, y= deaths/cohort_size, color = scenario), fun.y = median, geom = "line") +
    plotting_theme +
    labs(
      title = "Mortality over age",
      subtitle = country_nm
    )


  plots <- ggarrange(p1, p2, ncol = 2)

  print(plots)
}


wmr_diagnostic <- function(country_nm) {

  outputs <- final[scenario == "no-vaccination"] |>
    filter(country_name == country_nm) |>
    rename(iso3c = country) |>
    group_by(iso3c, year, run_id) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths), .groups = "keep"
    ) |>
    data.table()

  p1 <- ggplot() +
    geom_line(data = outputs[run_id == 0], mapping = aes(x = year, y = cases)) +
    geom_line(data= outputs[run_id != 0], mapping = aes(x= year, y = cases), color = 'blue', alpha= 0.1) +
    geom_line(data = wmr[country == country_nm], mapping = aes(x = year, y = wmr_cases, color = "red")) +
    geom_vline(xintercept = 2021, linetype = "dotted") +
    geom_vline(xintercept = 2023, linetype = "dotted") +
    scale_y_continuous(labels = comma) +
    plotting_theme +
    theme(legend.position = "none") +
    labs(
      title = "World Malaria Report cases vs. modelled outputs",
      subtitle = country_nm
    )

  p2 <- ggplot() +
    geom_line(data = outputs[run_id == 0], mapping = aes(x = year, y = deaths)) +
    geom_line(data= outputs[run_id != 0], mapping = aes(x= year, y = deaths), color = 'blue', alpha= 0.1) +
    geom_line(data = wmr[country == country_nm], mapping = aes(x = year, y = wmr_deaths, color = "red")) +
    geom_vline(xintercept = 2021, linetype = "dotted") +
    geom_vline(xintercept = 2023, linetype = "dotted") +
    scale_y_continuous(labels = comma) +
    plotting_theme +
    theme(legend.position = "none") +
    labs(
      title = "World Malaria Report deaths vs. modelled outputs",
      subtitle = country_nm
    )


  plots <- ggarrange(p1, p2, nrow = 2)
  print(plots)
}


# plot and save
pdf(paste0(filepath, "world_malaria_report_diagnostics.pdf"), width = 12, height = 12)
lapply(unique(final$country_name), wmr_diagnostic)
dev.off()

pdf(paste0(filepath, "incidence_diagnostic.pdf"), width = 12, height = 12)
lapply(unique(final$country_name), plot_incidence)
dev.off()

pdf(paste0(filepath, "mortality_diagnostic.pdf"), width = 12, height = 12)
lapply(unique(final$country_name), plot_deaths)
dev.off()


# this function is returning duplicates?
descrip<- 'number_plug_paper'
#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_final_outputs<- function(descrip){

  completed<- completed_reports('postprocessing')
  completed<- completed[description == {{descrip}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, quick_run, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description, quick_run)

  pull_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- data.table::rbindlist(readRDS(paste0('archive/postprocessing/', directory_name, '/final_output.rds')))
    return(output)
  }

  outputs<- data.table::rbindlist(lapply(c(1:nrow(completed)), pull_output, map = completed))
}
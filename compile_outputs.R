# compile reports-- if results are missing, impute zeroes for now
path <-"J:/VIMC_malaria/archive/postprocess/"
library(ggpubr)
library(ggforce)
library(wesanderson)
library(extrafont)
library(scene)
library(purrr)
library(data.table)
library(dplyr)



full_output<- compile_stochastic_outputs(filepath = "J:/VIMC_malaria/archive/postprocess/",
                                         description= 'test_round2_changes')


for (scen in unique(full_output$to_submit$scenario)){

  print(scen)

  saving<- full_output$to_submit |>
    filter(scenario == scen) |>
    rename(run_id = parameter_draw)
  print(length(unique(saving$country)))
  saving<- saving |>
    select(-scenario)

  write.csv(saving, paste0('montagu/', 'stochastic-burden-est-', scen, '.csv'))
}


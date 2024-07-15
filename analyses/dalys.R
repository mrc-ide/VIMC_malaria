# investigate DALYs issue



library(vimcmalaria)
library(dplyr)





eth<- compile_final_outputs('fix_rtss_booster') |>
  select(-run_id) |>
  select(-scenario)


View(eth |> filter(cases == 0))



eth<- compile_final_outputs('fix_rtss_booster') 



previous_outputs<- compile_final_outputs('stochastic_fix') 

previous_outputs <- previous_outputs |>
  filter(country != 'ETH',
         scenario %in% c('malaria-r3-r4-bluesky', 'malaria-r3-bluesky', 'malaria-r3-default', 'malaria-r3-r4-default'))


View(previous_outputs |> filter(cases == 0))

# don't see any zeroes

montagu_files<- list.files('montagu/update/', full.names = TRUE)

sent_results<- rbindlist(lapply(montagu_files, read.csv))


View(sent_results |> filter(cases == 0))

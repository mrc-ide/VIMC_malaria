supplement<- supplement |>
  filter(run_id < 51) |>
  group_by(scenario, run_id, year) |>
  summarise(cases = sum(cases, na.rm= TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            .groups = 'keep') |>
  ungroup() |>
  group_by(scenario, year) |>
  summarise(cases = median(cases, na.rm = TRUE),
            deaths= median(deaths, na.rm = TRUE),
            .groups = 'keep') |>
  data.table()
supplement2<- dcast(supplement, year ~ scenario, value.var = c('cases', 'deaths')) |>
  rename(cases_baseline= `cases_no-vaccination`,
          deaths_baseline = `deaths_no-vaccination`,
          ) |>
  mutate(cases_baseline= format(cases_baseline, big.mark= ',', scientific= FALSE),
         cases_routine= format(cases_routine, big.mark= ',', scientific= FALSE),
        deaths_baseline= format(deaths_baseline, big.mark= ',', scientific= FALSE),
        deaths_routine= format(deaths_routine, big.mark= ',', scientific= FALSE)) |>
  rename(Year= year,
         `Cases (no vaccination)`= cases_baseline,
         `Cases (routine vaccination)` = cases_routine,
        ` Deaths (no vaccination)`= deaths_baseline,
         `Deaths (routine vaccination)` = deaths_routine)


write.csv(supplement2, 'analyses/paper/tables/scenario_outputs_supplement.csv')

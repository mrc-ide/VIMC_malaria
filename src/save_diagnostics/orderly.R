# save diagnostics
# save diagnostics for a certain description to a central place
orderly2::orderly_parameters(description = NULL)



metadata<- orderly2::orderly_dependency("process_site",
                                              "latest(parameter:iso3c == this:iso3c 
                                                          && parameter:site_name == this:site_name 
                                                          && parameter:ur == this:ur 
                                                          && parameter:population == this:population
                                                          && parameter:description == this:description
                                                          && parameter:scenario == this:scenario
                                                          && parameter:parameter_draw == this:parameter_draw
                                                          && parameter:quick_run == this:quick_run)",
                                              c(site_diagnostic_report = "site_diagnostic_report.Rmd"))

meta <- orderly2::orderly_metadata_extract(name = 'site_diagnostics', descrip = 'parameters.description is complete_run')

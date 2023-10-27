# VIMC_orderly
Orderly workflow for VIMC malaria runs. Source code is found in the /src folder, with the name of each folder corresponding to the name of each report. The following reports + actions are run in the following order for the 31 VIMC countries:


1) Process external inputs for country of interestm including site file, demographic, and vaccine coverage data ("process_inputs")
2) Parametrize model ("set_parameters")
3) Run malariasimulation model ("launch_models")
4) Process model outputs ("process_results.R")
5) Produce diagnostic report (at the site level)

#  Application
In order to run this workflow, run the "VIMC_workflow.R" script. The following parameters must be changed:
- iso3c: country you would like to run models for
- scenario: scenario you would like to run models for (baseline = standard vaccination schedule, intervention = 80% vaccine coverage in 2023, constant until 2050)
- pop: population size
- description: description of the reason for a certain model run


# VIMC_orderly
Orderly workflow for VIMC malaria runs. Source code is found in the /src folder, with the name of each folder corresponding to the name of each report. The following reports + actions are run in the following order for the 31 VIMC countries:


1) Process external inputs for country of interest, including site file, demographic, and vaccine coverage data [("process_inputs")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_inputs/orderly.R)
2) Parametrize model ["set_parameters"](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/set_parameters/orderly.R)
3) Run malariasimulation model ["launch_models"](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/launch_models/orderly.R)
4) Process model outputs [("process_site")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R)
5) Produce diagnostic report (at the site level) [("site_diagnostics")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R)
6) Aggregate outputs up to country level [("process_country")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_country/orderly.R)

#  Application
In order to run this workflow, run the "VIMC_workflow.R" script. The following parameters must be changed:
- iso3c: country you would like to run models for
- scenario: scenario you would like to run models for. Options:
- 'no-vaccination': No vaccines implemented
- 'r3-default': Initial series of R21
- 'r3-r4-default': Full series of R21 (with booster)
- 'rts3-bluesky': 90% of initial series of RTS,S
- 'rts3-default': Initial series of RTS,S
- 'rts3-rts4-bluesky': 90% of full series of RTS,S (90% coverage for time period)
- draw: draw value for model run. For median parameter values, set to 0.
- population: population size for model run.
- description: description of the reason for a certain model run.
- site_name: name of site to run
- ur: urban/rural split.

#  Other notes
This workflow uses a modified version of the site package [(link here)](https://github.com/mrc-ide/VIMC_malaria/blob/main/site_0.2.2.tar.gz). The modified version of this package is located in this repo and should be installed prior to running this workflow.

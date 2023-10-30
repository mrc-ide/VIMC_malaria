# VIMC Malaria modelling
This repository contains code used to estimate the impact of malaria vaccines on cases, deaths, and DALYs from 2000-2100 for the Vaccine Impact Modelling Consortium (VIMC). More information on the VIMC can be found [here](https://www.vaccineimpact.org/). This workflow is written in [orderly2](https://mrc-ide.github.io/orderly2/), a package designed to facilitate reproducible analysis. Source code is found in the [/src](https://github.com/mrc-ide/VIMC_malaria/tree/main/src) folder, with the name of each folder corresponding to the name of each report. The following reports + actions are run chronologically to produce final estimates, with more detail below. 

##  Quick Start
In order to run this workflow, run the ["VIMC_workflow.R"](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R) script. Carry out the following steps:

### Initialize orderly repository
Install orderly2, if you have not already, with the command `install_github('mrc-ide\orderly2')`.
You may need to initialize this repository by setting your working directory to the repository and running the command `orderly2::orderly_init()`.

###  Install site package
This workflow uses a modified version of the site package [(link here)](https://github.com/mrc-ide/VIMC_malaria/blob/main/site_0.2.2.tar.gz), which is used to translate site files into malariasimulation model paramters. The key change is that this version allows the user to parameterize the R21 vaccine, and also allows the user to implement varied coverage of the booster vaccine dose over time. The modified version of this package should be installed prior to running this workflow using the following command:
`install.packages('VIMC_malaria/site_0.2.2.tar.gz')`
### Save input files

###  Change input parameters
The following parameters must be changed for each run (on the [following lines](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R#L22-L32)):
* **iso3c:** country you would like to run models for
* **scenario:** scenario you would like to run models for. Options:
    * 'no-vaccination': No vaccines implemented
    * 'r3-default': Initial series of R21, based on GAVI forecasts.
    * 'r3-r4-default': Full series of R21 (with booster), based on GAVI forecasts.
    * 'rts3-bluesky': 90% coverage of initial series of RTS,S for entire modelling period
    * 'rts3-default': Initial series of RTS,S, based on GAVI forecasts.
    * 'rts3-rts4-bluesky': 90% of full series of RTS,S (90% coverage for entire modelling time period)
- **draw:** draw value for model run. For median parameter values, set to 0.
- **population:** population size for model run.
- **description:** description of the reason for a certain model run.
- **site_name:** name of site to run
- **ur:** urban/rural split.

### Run process inputs report
The code to run this report is found [here](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R#L35-L44). This only needs to be run one time, if inputs do not change.

### Run set_parameters, launch_models, and process_site for all sites
This reports must be run in chronological order for all of the sites in a country for the vaccine scenario of interest. The code to run these reports can be found [here](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R#L47-L77). To run a specific report, change the `report_type` variable [here](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R#L49).

### Run process_country to aggregate outputs up to country level.
Run this report using the code linked [here](https://github.com/mrc-ide/VIMC_malaria/blob/main/VIMC_workflow.R#L93-L104).

### Produce diagnostics as needed.
Diagnostics can be produced with the "site_diagnostics" report.


# Methods documentation
##  Process inputs
1) Process external inputs for country of interest, including site file, demographic, and vaccine coverage data [("process_inputs")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_inputs/orderly.R)

## Parameterize model
3) Parametrize model [("set_parameters")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/set_parameters/orderly.R)

## Launch model
5) Run malariasimulation model [("launch_models")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/launch_models/orderly.R)


7) Process model outputs [("process_site")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R)
8) Produce diagnostic report (at the site level) [("site_diagnostics")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R)
9) Aggregate outputs up to country level [("process_country")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_country/orderly.R)

#  Application


#  Other notes


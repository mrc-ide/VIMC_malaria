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
VIMC model inputs are saved locally and not tracked on this repository due to large size and privacy issues. VIMC inputs should be saved under `/src/set_parameters/`. Contact Lydia for access to these files.

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

### Produce diagnostics
Diagnostics can be produced with the "site_diagnostics" report.

# Methods documentation
##  Process inputs
Process external inputs for country of interest, including site file, demographic, and vaccine coverage data [("process_inputs")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_inputs/orderly.R). Reads in and saves the following inputs by country:
- Site file (characterizing the level and pattern of transmission in an admin 1 unit, in addition to intervention coverage and vector species)
- Life expectacy by age (from VIMC)
- Mortality rate (from VIMC)
- Population size (from VIMC)

These reports should only be run once, then rerun if inputs change.

## Parameterize model
Parameterize model [("set_parameters")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/set_parameters/orderly.R). Models are run with single year age groups from 0 to 100, from 2000-2100. 
This script pulls in the corresponding site file for an admin 1 unit, which characterizes the pattern on malaria transmission in this area. The interventions component of the site file is modified based on the VIMC scenario of interest, specifying the coverage and booster coverage for each year of the simulation (for either RTS,S or R21). These values are added to the site file via the following columns: **rtss_coverage**, **rtss_booster_coverage**, **r21_coverage**, and **r21_booster_coverage**.

We additionally carry over intervention coverage from the last observed year (typically 2023) out to 2100, assuming constant values for the remainder of the simulation period. Note that ITN usage follows a 3-year cyclical pattern-- this pattern of the last 3 year cycle observed is carried out for the remainder of the simulation period.

We utilize a modified version of the site package to translate site file inputs into malariasimulation parameters. The base version of the site package is found [here](https://github.com/mrc-ide/site). The `add_interventions()` function in the site file was modified to reflect changing booster coverage over time. For source code, contact Lydia. 

Note that this report can not be launched on the HPC cluster, because of package dependency issues.

## Launch model
Run malariasimulation model [("launch_models")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/launch_models/orderly.R). If you would like to obtain an estimate of model run time, test this report locally before launching on the cluster.

## Postprocess outputs 
Process model outputs [("process_site")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R). Outputs are processed in the following steps.

* `get_rates()` from the [postie](https://github.com/mrc-ide/postie) package is used to estimate incidence, mortality, YLD, YLL, and DALY rates from model outputs. Note that these rates are based on the population size used to run the model, not the real-world population size of the site modelled. DALY functionality for postie is currently on the [DALYs](https://github.com/mrc-ide/postie/tree/dalys/R) repository branch; you will need to install this version of the postie package to run this code using the function call `install.packages('mrc-ide/postie@dalys'). DWs for malaria are sourced from the Global Burden of Disease study. For the purposes of VIMC modelling, we ignore malaria GBD disability weights for comorbid conditions such as anemia and motor impairiment. For documentation on the DW values used, see source code [here](https://github.com/mrc-ide/postie/blob/dalys/R/epi.R#L36-L85).
* Because the VIMC utilizes country-specific life-expectancy, YLLs and DALY rates are recalculated based on these inputs.
* Rates are multiplied by site population to estimate cases, deaths, and DALYs from 2000-2050.
* We scale site file populations such that the sum of site file populations is equivalent to the national VIMC population.
* From 2050 onward, site population is estimated as (national population in year) * [(scaled site population in 2050)/ (national population in 2050)]. We assume that the proportional breakdown of population by site is fixed from 2050-2100, in the absence of other data.

## Produce diagnostics
Produce diagnostic report (at the site level) [("site_diagnostics")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_site/orderly.R).

## Process country
Aggregate outputs up to country level [("process_country")](https://github.com/mrc-ide/VIMC_malaria/blob/main/src/process_country/orderly.R).

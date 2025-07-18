## Impact of Vaccine Introduction on Malaria Cases and Deaths in Sub-Saharan Africa: A Modelling Study
This repository contains code used to generate estimates reported in the manuscript draft, "Impact of Vaccine Introduction on Malaria Cases and Deaths in Sub-Saharan Africa: A Modelling Study". This workflow is written in [orderly2](https://mrc-ide.github.io/orderly2/), a package designed at ICL to facilitate reproducible analysis. Source code is found in the [/src](https://github.com/mrc-ide/VIMC_malaria/tree/paper/src) folder, with the name of each folder corresponding to the name of each report and the corresponding script containing code for execution. The following reports + actions are run chronologically to produce final estimates, with more detail below. 

The helper functions developed for this workflow can be found in the [vimcmalaria](https://github.com/mrc-ide/vimcmalaria) package. This package is a holding place for helper functions used in this workflow, and therefore may not be useful when applied to other contexts. More documentation + testing for this package is forthcoming.

## Directory
```
.
├── analyses                             # Analyses to supplement main workflow
|   ├── vaccine_scenario                 # Generate proxy vaccine coverage scenario for manuscript based on DTP3 coverage, vaccine type, and year of introduction
├── src                                  # Source code for VIMC malaria main workflow
|   ├── process_inputs                   # Format input demography, vaccine coverage, and malariasimulation site file inputs for analysis
|   |   ├── process_inputs.R                   
|   ├── process_country                  # Paramaterize, run models, and do initial postprocessing
|   |   ├── process_country.R                   
|   ├── postprocessing                   # Do additional postprocessing, including scaling national outputs to World Malaria Report data and aggregation
|   |   ├── postprocessing.R                   
|   ├── diagnostics                      # Generate an Rmarkdown as a diagnostic report of vaccine impact results
|   |   ├── diagnostics.R                   
├── VIMC_malaria.Rproj                   # R.Studio project file
├── orderly_config.yml                   # Configuration for orderly workflow. Do not modify
├── pkgdepends.txt                       # Package dependencies for cluster installation
├── postie_0.1.2.txt                     # Postie package version for VIMC workflow (because DALYs are recalculated based on VIMC inputs). Might be a separate branch, should be updated in the future
├── workflow.R                           # Script to initialize orderly workflow, set up cluster environment with hipercow package, and run analysis. Start here
└── README.md                            # Project overview

```

###  Quick Start
In order to run this workflow, run the ["workflow.R"](https://github.com/mrc-ide/VIMC_malaria/blob/main/workflow.R) script. 
- `Process_inputs` formats the input files needed for this workflow.
- `Process_country` parameterizes, models, and postprocesses outputs used to estimate vaccine impact at the admin1 level.
- `Postprocessing` conducts basic postprocessing, including scaling and aggregation.
- `Diagnostics` generates a basic report with plots for vetting.

#### Save input files
VIMC model inputs are saved locally and not tracked on this repository due to large size and data privacy contraints. VIMC inputs and site files should be saved under `/src/process_inputs/vimc_inputs` and `/src/process_inputs/site_files`, respectively. Contact Lydia for access to these files.

#### Run process inputs report for each country of interest
This only needs to be run one time for each country, if VIMC inputs and site files do not change. This report pulls in VIMC inputs and site files, reformatting for later use. 

####  Change input parameters
The following parameters must be changed for each run:
- `iso3c`: country/countries you would like to run models for
- `parameter_draw`: parameter draw value for model run. For median parameter values, set to 0.
- `description`: description of the reason for a certain model run. Make sure to change this value for each run, unless you seek to overwrite pre-existing outputs.
- `quick_run`: Boolean, for whether you would like to run a test model or the full simulation. A test model produces outputs with wider age bands and a shorter time horizon (2000-2035), in order to optimize model run time. Preferable to set to `TRUE` when testing models locally, debugging changes, etc.
- `scenario`: vaccine scenario you would like to run models for. Options:
    * `no-vaccination`: No vaccines implemented. 
    * `proxy`: proxy routine coverage scenario based on public GAVI estimates of protected children, routine DTP3 coverage, and vaccine choice by country (R21 vs. RTS,S). 
      
The `make_parameter_map` function will create input parameter data frames (at the site and country level) for all of the sites in the 31 VIMC-modelled countries, as well as each VIMC vaccination scenario. 

#### Run process_country  for all countries, parameter draws, and scenarios of interest
This reports must be run in chronological order for all of the sites in a country for the vaccine scenario of interest. The code to run these reports can be found [here](https://github.com/mrc-ide/VIMC_malaria/blob/main/workflow.R). 
Note that given extended run time, you will likely prefer to launch models on the cluster. Ensure orderly2, malariasimulation, dplyr, and data.table are installed in your cluster environment before launching models or they will fail. 

#### Other notes
This workflow can be quite space-intensive (particularly when running models for all sites in the 31 VIMC input countries). It is worthwhile to monitor the size of this repository and regularly clean out non-final reports. 


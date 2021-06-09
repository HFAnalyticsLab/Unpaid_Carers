# Unpaid carers during the pandemic

#### Project Status: [Planning]

## Project Description

The impact of unpaid caring is closely related to the intensity of the care provision. Evidence suggests that unpaid carers were affected negatively by the COVID-19 pandemic but previous studies have not made a distinction between different caring types. This analysis explores who the unpaid carers are during the second wave of the pandemic, their state of health and how they have been able to manage their own health through accesing healthcare services. It will make a disnticion between different caring types.

Currently  the code in this repo provides descriptive analysis

## Data source

Source: Understanding Society Data set, [Wave 10 (2018/19)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614) and COVID-19 survey Waves [6 (November 2020) and 7 (January 2021)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8644)


## How does it work?

Data can be downloaded from UK Data Service (see data source for link)
The code can be used to clean and analyse the data. 
For the current analysis, we use special weightings provided by Unveristy of Essex, which is not included in the repository. **To run the analysis, the weighting type will need to be changed**.  

### Requirements

Software or packages that needs to be installed and and how to install them.

For example:
These scripts were written in R version 4.0 and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)
* [**survey**](https://cran.r-project.org/web/packages/survey/survey.pdf)
* [**haven**](https://cran.r-project.org/web/packages/haven/index.html)
* [**gtsummary**](https://cran.r-project.org/web/packages/gtsummary/index.html)
* [**THFstyle**](https://github.com/THF-evaluative-analytics/THFstyle) internal package

### Getting started
The 'src' folder contains

* [0_data_loading.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/0_data_loading.R) - Download data - links need to be updated for latest data
* [1_clean_care_tpye.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/1_cleaning_care_type.R) - Clean and save data with care type variables
* [2_demographics.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/2_demographics.R) - Clean and save demographics
* [3_health.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/3_health.R) - Clean and save health variables
* [4_access_to_services.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/4_access_to_services.R) -  Clean and save access to services variables
* [5_covid](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/5_covid.R) - Clean, COVID-19 related variables
* [6_unpaid_carer_analysis](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/6_unpaid_carer_analysis.R) - Descriptives analysis with charts
* [7_sensitivity_analysis](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/7_sensitivity_20analysis.R) - Sensitivity Analysis 

List of variables used can be found in the [variables excel sheet](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/variables.xlsx) 


## Authors

* Anne Alarilla - [Twitter](https://twitter.com/AlarillaAnne) - [GitHub](https://github.com/annealarilla)

## License

This project is licensed under the [MIT License](link to license file).

## Acknowledgments

* Code to download the survey data was adopted from code deposited to [Understanding Society](https://www.understandingsociety.ac.uk/documentation/mainstage/syntax) by Dr. David Bartram
* Univeristy of Essex provided weightings for this analysis

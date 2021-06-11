# Unpaid carers during the pandemic

#### Project Status: [Completed]

## Project Description

The impact of unpaid caring is closely related to the intensity of the care provision. Evidence suggests that unpaid carers were affected negatively by the COVID-19 pandemic but previous studies have not made a distinction between different caring types. This analysis explores who the unpaid carers are during the second wave of the pandemic, their state of health and how they have been able to manage their own health through accesing healthcare services. It will make a disnticion between different caring types.

Currently  the code in this repo provides descriptive analysis

### Defintions of caring

#### During the second wave of pandemic (November 2020/ January 2021) 
Unpaid carers were defined if respondents said yes to the one of the following:

1. Is there anyone living with you who is sick, disabled or elderly whom you look after or give special help to (for example, a sick, disabled or elderly relative, husband, wife or friend etc)?
2. Thinking about the last 4 weeks, did you provide help or support to family, friends or neighbours who do not live in the same house/flat as you?

##### Caring status during the second wave was defined as:

* Not providing care: Not an unpaid carer during the second wave of the pandemic
* <20 hours of care per week: Carers during the second wave of the pandemic providing <20 hours of care per week or not providing help for with personal tasks
* 20+ hours of care per week: Carers during the seond wave of the pandemic providing 20+ hours of care per week or providing help with personal tasks


#### Pre pandemic (2018/2020)
Unpaid carers were defined if respondents said yes to one of the following: 

1. Do you provide regular service or help for any sick, disabled or elderly person not living with you?
2. Is there anyone living with you who is sick, disabled, or elderly, who you give special help to?

##### Caring status pre pandemic was defined as:

* Not providing care: Not an unpaid carer pre pandemic
* <20 hours of care per week: Carers pre pandemic providing <20 hours of care per week 
* 20+ hours of care per week: Carers pre pandemic providing 20+ hours of care per week 

Note: ~10 respondents provided responses during April - May 2020

## Data source

Source: Understanding Society Data set, [Wave 10 (2018/20)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614) and COVID-19 survey Waves [6 (November 2020) and 7 (January 2021)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8644)


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
* Dr Jamie Moore from Univeristy of Essex for providing weightings for this analysis

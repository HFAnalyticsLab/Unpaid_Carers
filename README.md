# Unpaid carers during the pandemic

#### Project Status: [Planning]

## Project Description

The impact of unpaid caring is closely related to the intensity of the care provision. Evidence suggests that unpaid carers were affected negatively by the COVID-19 pandemic but previous studies have not made a distinction between different caring types. This analysis explores who the unpaid carers are during the second half of the pandemic, their state of health and how they have been able to manage their own health through accesing healthcare services. It will make a disnticion between different caring types.

Currently  the code in this repo provides preliminary analysis and findings relevant to the COVID-19 Impact Inquiry. Analysis presented are mainly descriptive. 

## Data source

Source: Understanding Society Data set, [Wave 10 (2018/19)](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000053#!/access-data) and COVID-19 sub study Waves [6 (November 2020) and 7 (January 2021)](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8644)


## How does it work?

The code can be used to analyse the data. 
Data can be donwloaded from UK Data Service (see data source for link)

### Requirements

Software or packages that needs to be installed and and how to install them.

For example:
These scripts were written in R version 4.0 and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)
* [**survey**](https://cran.r-project.org/web/packages/survey/survey.pdf)
* [**haven**](https://cran.r-project.org/web/packages/haven/index.html)
* **gtsummary**

### Getting started
The 'src' folder contains

* [0_data_loading.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/0_data_loading.R) - Download data - links need to be updated for latest data
* [1_clean_care_tpye.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/1_cleaning_care_type.R) - Clean and save data with care type variables
* [2_demographics.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/2_demographics.R) - Clean and save demographics
* [3_health.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/3_health.R) - Clean and save health variables
* [4_access_to_services.R](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/4_access_to_services.R) -  Clean and save access to services variables
* [5_covid](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/5_covid.R) - Clean, COVID-19 related variables
* [6_COVID_19_Impact_Inquiry]( https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/src/6_COVID_19_Impact_Inquiry.R)

List of variables used can be found in the [variables excel sheet](https://github.com/HFAnalyticsLab/Unpaid_Carers/blob/main/variables.xlsx) 

## Useful references


## Authors

* contributor name - [Twitter] - [GitHub]

## License

This project is licensed under the [MIT License](link to license file).

## Acknowledgments

* Credit anyone whose code was used
* Inspiration
* etc

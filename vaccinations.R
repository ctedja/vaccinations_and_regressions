library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)



# This dataset on vaccine rollouts and vaccine hesitancy comes from the CDC.
#   https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw
vac_raw <- read.csv(url("https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv"))




# The following datasets come from various US Gov agencies.
# Took the county-level data from this link:
#   https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

    # Dataset on poverty estimates:
    # Source: U.S. Department of Commerce, Bureau of the Census, Small Area Income and Poverty Estimates (SAIPE) Program.
    # Variable descriptions in the second sheet of the excel file.
    pov_raw <- read_excel("PovertyEstimates.xls", 
               sheet = "Poverty Data 2019",
               skip = 4)
    
    
    # Education Attainment
    # Sources: U.S. Census Bureau, 1970, 1980, 1990, 2000 Censuses of Population, and the 2015-19 American Community Survey 5-yr average county-level estimates.
    edu_raw <- read_excel("Education.xls", 
                          sheet = "Education 1970 to 2019",
                          skip = 4)
    
    
    # Unemployment
    # Sources: Unemployment: U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS); median household income: U.S. Department of Commerce, Bureau of the Census, Small Area Income and Poverty Estimates (SAIPE) Program.
    une_raw <- read_excel("Unemployment.xls", 
                          sheet = "Unemployment Med HH Income",
                          skip = 4)
    
    
    # Population
    pop_raw <- read_excel("PopulationEstimates.xls", 
                          sheet = "Population Estimates 2010-19",
                          skip = 2)


# Next, will filter variables of interest in each of
#   these datasets, and then do a join.

# Then, for the fun stuff!

    # We gotta think of a far better name than this too... ha




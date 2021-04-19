# LOAD ----

library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)

# This dataset on vaccine rollouts and vaccine hesitancy comes from the CDC.
#   https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw

    # Initially, I had the following function but the direct url dl was taking forever)
    #   vac_raw <- read.csv(url("https://data.cdc.gov/resource/q9mh-h2tw.csv"))

vac_raw <- read.csv("Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv")


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


# TIDY ----
# edu_raw has 3,283 FIPS, une_raw has 3,275; pop_raw has 3,273, pov_raw has 3,193
#   and vac_raw has 3,142
    
# I checked with some anti_joins and it seems that all FIPS for datasets
#   are contained within edu_raw, so a left_join could work
#   just note that for the vaccination dataset, FIPS is originally integer
#   while rest 
    
glimpse(vac_raw)
glimpse(edu_raw)
glimpse(une_raw)
glimpse(pop_raw)
glimpse(pov_raw)

length(unique(edu_raw$'FIPS Code'))
length(unique(une_raw$fips_txt))
length(unique(pop_raw$FIPStxt))
length(unique(pov_raw$FIPStxt))
length(unique(vac_raw$FIPS.Code))
    

test <- anti_join(edu_raw, pop_raw, by = c("FIPS Code" = "FIPStxt"))
length(unique(test$'FIPS Code'))
rm(test)

test2 <- anti_join(pop_raw, edu_raw, by = c("FIPStxt" = "FIPS Code"))
length(unique(test2$'FIPStxt'))
rm(test2)


#

vaccin <- vac_raw %>% 
    mutate(FIPS.Code = as.character(FIPS.Code)) %>% 
    mutate(FIPS.Code = case_when(nchar(FIPS.Code) == 4 ~ paste0("0", FIPS.Code), 
                                 nchar(FIPS.Code) == 5 ~ FIPS.Code))

colnames(vac_raw)




# Next, will filter variables of interest in each of
#   these datasets, and then do a join.

# Then, for the fun stuff!"fips_text" = 

    # We gotta think of a far better name than this too... ha




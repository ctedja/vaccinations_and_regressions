
# LOAD ----
library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)
library(broom)
library(egg)
library(maps)
library(sf)
library(tigris)
library(RColorBrewer)


# This dataset on vaccine rollouts and vaccine hesitancy comes from the CDC.
#   https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw

# Initially, I had the following function but the direct url dl was taking forever)
#   vac_raw <- read.csv(url("https://data.cdc.gov/resource/q9mh-h2tw.csv"))

# for Moctar since R is being stupid
#setwd("C:/Users/mocta/Documents/GitHub/vaccinations_and_regressions")

vac_raw <- read.csv("Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv")



#vac_raw <- read.csv("C:/Users/mocta/Documents/GitHub/vaccinations_and_regressions/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv")


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

# I checked with some anti_joins and it seems that all FIPS for vaccination datasets
#   is contained within the other datasets, with the exception of one...
#   Kalawao County, Hawaii.

#   In addition, the education and unemployment datasets include state-level
#   disaggregation + Puerto Rico, but I don't think we'll need those.

length(unique(edu_raw$'FIPS Code'))
length(unique(une_raw$fips_txt))
length(unique(pop_raw$FIPStxt))
length(unique(pov_raw$FIPStxt))
length(unique(vac_raw$FIPS.Code))

# Just note that for the vaccination dataset, FIPS is originally integer
# while the rest of the datasets have it as char, hence:
vaccinations <- vac_raw %>% 
  mutate(FIPS.Code = as.character(FIPS.Code)) %>% 
  mutate(FIPS.Code = case_when(nchar(FIPS.Code) == 4 ~ paste0("0", FIPS.Code), 
                               nchar(FIPS.Code) == 5 ~ FIPS.Code)) %>%
  rename('percent_adults_fully_vaccinated' = 'Percent.adults.fully.vaccinated.against.COVID.19')



# Can delete later, but here if you wanna see some diffs bw datasets
test <- anti_join(edu_raw, vaccinations, by = c("FIPS Code" = "FIPS.Code"))
length(unique(test$'FIPS Code'))
unique(test$'Area name')
rm(test)

test2 <- anti_join(vaccinations, edu_raw, by = c("FIPS.Code" = "FIPS Code"))
length(unique(test2$'FIPS.Code'))
rm(test2)

test3 <- anti_join(une_raw, vaccinations, by = c("fips_txt" = "FIPS.Code"))
length(unique(test3$'FIPS Code'))
unique(test3$'Area name')
rm(test3)

test4 <- anti_join(vaccinations, une_raw, by = c("FIPS.Code" = "fips_txt"))
length(unique(test4$'FIPS.Code'))
rm(test4)


# Now I'm just tidying out the mass of columns we dont need.
#   I've kept here the pop numbers in case we need them later,
#   rather than percentages.

# I've only selected overall poverty population numbers and percentages
#   while emitting child poverty stats and confidence intervals
#     (especially since our vaccination data focuses on adults for now)
poverty <-pov_raw %>%
  select(c("FIPStxt", 
           #"POVALL_2019", 
           "PCTPOVALL_2019"
  )) %>%
  rename('poverty_percentage' = 'PCTPOVALL_2019')

# For education, it's pretty straightforward, just took most recent.
education <- edu_raw %>% select(
  c('FIPS Code',
    #'Less than a high school diploma, 2015-19',
    #'High school diploma only, 2015-19',
    #'Some college or associate\'s degree, 2015-19',
    #'Bachelor\'s degree or higher, 2015-19',
    'Percent of adults with less than a high school diploma, 2015-19',
    'Percent of adults with a high school diploma only, 2015-19',
    'Percent of adults completing some college or associate\'s degree, 2015-19',
    'Percent of adults with a bachelor\'s degree or higher, 2015-19'
  )) %>%
  rename('edu_percent_less_hs' = 'Percent of adults with less than a high school diploma, 2015-19',
         'edu_percent_hs_only' = 'Percent of adults with a high school diploma only, 2015-19',
         'edu_percent_some_college' = 'Percent of adults completing some college or associate\'s degree, 2015-19',
         'edu_percent_bachelors' = 'Percent of adults with a bachelor\'s degree or higher, 2015-19')

unemployment <- une_raw %>% select(
  c('fips_txt',
    #'Employed_2019',
    #'Unemployed_2019,',
    'Unemployment_rate_2019',
    'Median_Household_Income_2019',
    #'Med_HH_Income_Percent_of_State_Total_2019'
  ))

population <- pop_raw %>% select(
  c('FIPStxt',
    'POP_ESTIMATE_2019'
  ))


# Now, to join. As mentioned above, we lose one Kalawao County, Hawaii,
#   though I haven't investigated why that's not in the other datasets.
joined_data <- vaccinations %>% 
  inner_join(unemployment, by = c("FIPS.Code" = "fips_txt")) %>%
  inner_join(population, by = c("FIPS.Code" = "FIPStxt")) %>%
  inner_join(poverty, by = c("FIPS.Code" = "FIPStxt")) %>% 
  inner_join(education, by = c("FIPS.Code" = "FIPS Code"))

glimpse(joined_data)

# I found 315 NAs in the percent_adults_vaccinated... wonder why?
#   they're mostly in Texas, New Mexico, Hawaii, and a few in Virginia
joined_data %>%
  filter(is.na(joined_data$percent_adults_fully_vaccinated))


# EDA Part 1 ----
#   EDA and defining/quantifying the issue: How do we define a low-vaccination county? 
#   How many people are in 'low vaccination' counties?

# A quick dual-plot to explore (boxplot + histogram)
egg::ggarrange(
  # boxplot
  ggplot(joined_data, aes(x = "",
                          y=percent_adults_fully_vaccinated)) +
    stat_boxplot(geom ='errorbar') +
    geom_boxplot(notch = TRUE) +
    coord_flip() +
    theme_minimal() +
    xlab("") +
    ylab(""),
  # hist
  ggplot(joined_data, aes(x = percent_adults_fully_vaccinated)) +
    geom_histogram(bins = 50) +
    theme_classic(), 
  heights = c(1, 3))

# The plot above shows us that there actually be some outliers, I think? 

# Based on the below, the mild threshold for the lower bound (Q1-IQR*1.5) 
#   is at 2.65 percent. vaccination quartiles and lower bounds defined below.
vac_q1 <- quantile(joined_data$percent_adults_fully_vaccinated, na.rm = TRUE)[1]
vac_q2 <- quantile(joined_data$percent_adults_fully_vaccinated, na.rm = TRUE)[2]
vac_q3 <- quantile(joined_data$percent_adults_fully_vaccinated, na.rm = TRUE)[3]
vac_q4 <- quantile(joined_data$percent_adults_fully_vaccinated, na.rm = TRUE)[4]

vac_lower_bound <- vac_q2 - (vac_q4-vac_q2)*1.5

# The mild threshold for the upper bound (Q3+IQR*1.5) is at 35.05 percent
vac_upper_bound <- vac_q4 + (vac_q4-vac_q2)*1.5

# 28 counties are quite a bit behind that lower bound of now, at less than 2.65
#   percent of their population vaccinated, based on the below.
joined_data %>% 
  filter(percent_adults_fully_vaccinated < vac_lower_bound) %>% 
  length()


# As to your question of how we might classify rates of facination,
#   might the following work?

test <- joined_data %>% 
  mutate(vaccination_group = case_when(
    percent_adults_fully_vaccinated < vac_lower_bound ~ "0. Laggers",
    percent_adults_fully_vaccinated > vac_lower_bound &
      percent_adults_fully_vaccinated <= vac_q2 ~ "1. First Quartile",
    percent_adults_fully_vaccinated > vac_q2 &
      percent_adults_fully_vaccinated <= vac_q3 ~ "2. Second Quartile",
    percent_adults_fully_vaccinated > vac_q3 &
      percent_adults_fully_vaccinated <= vac_q4 ~ "3. Third Quartile",
    percent_adults_fully_vaccinated > vac_q4 &
      percent_adults_fully_vaccinated <= vac_upper_bound ~ "4. Fourth Quartile",
    percent_adults_fully_vaccinated > vac_upper_bound ~ "5. Frontrunners"))



# // A Map -----
#   using library(tigris) and library(RColorBrewer) and library(sf)
#   @Moctar you were absolutely right though about how SLOW it is
#   I tried doing the full US but it exploded my computer
#   so as you can see below I'm just testing it on some selected states.
#   Saved the image also in the repo if u wanna see.
#     https://github.com/ctedja/vaccinations_and_regressions/blob/main/Vaccinations_EDA_Map.png
#   for further reference for me, a US geography noob...
#     https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations

county_basemap <- counties(cb = TRUE, 
                           state = c(01, 12, 13, 45, 28, 47, 37)) %>% 
  st_as_sf()

test2 <- test %>% select(FIPS.Code, 
                         vaccination_group, 
                         County.Boundary, 
                         State.Code) %>%
  filter(State.Code %in% c("AL", "FL", "GA", "SC", "MS", "TN", "NC"))

map <- left_join(county_basemap, test2, 
                 by = c("GEOID" = "FIPS.Code")) %>% na.omit()

ggplot(data = map) + 
  geom_sf(data = map, aes(fill = vaccination_group), color = NA) +
  geom_sf(data = county_basemap, aes(fill = NA), color = NA) + 
  theme_void() +
  scale_fill_brewer(palette = "BuPu", direction = -1)


# Excuse this clunky code for now, it's just non-island state codes
#state_code <- c("51", "31", "12", "05", "22", "33", "34", "29", 
#                   "30", "13", "37", "17", "06", "42", "45", "39",
#                   "40", "53", "54", "55", "16", "19", "01", "56", 
#                   "20", "38", "21", "47", "46", "08", "26", "10",
#                   "18", "41", "32", "24", "36", "02", "28", "49", 
#                   "23", "04", "27","44", "11", "09", "25", "50")


# EDA Part 2 -----

  # How related are SES variables to one another?
names(joined_data)
all_ses_data <- joined_data[, c(4, 5, 6, 7, 11, 14, 16, 21:25)]


library(psych)
pairs.panels(all_ses_data)

  # Do any SES variables correlate with vaccination refusal? What about when the US is cut into different regions (ex. Northeast / South / Southwest, West, Midwest, Other?)


# EDA Part 3 --- Tests for spatial autocorrelation in Xs and Ys
















#   Regression modelling: 
#     Model 1 ('cannot') uses socio-economic status indicators, state-level fixed effects to look at vaccination rates. 
#     Model 2 ('will not') looks at 'antivaxxer' indicators related to vaccination rates. 
#     Model 3 ('all together') looks at both of these factors together.

# MODEL 1
# Dependent variable
#   = percent_adults_fully_vaccinated

# Potential independent variables 
#   = Unemployment_rate_2019
#   = poverty_percentage
#   = Median_Household_Income_2019 
#   = Education (there are four different levels, I'm not sure)
#   = Estimated.strongly.hesitant
#   = Estimated.hesitant
#   anything else?


# When trying a single variable linear regression first here, with poverty.
#   we get an r-squared of 0.0254

# There's one outlier, Chattahoochee County, Georgia with like >90% vaccination rate

ggplot(joined_data, 
       aes(poverty_percentage, percent_adults_fully_vaccinated)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

poverty_model <- lm(percent_adults_fully_vaccinated ~ poverty_percentage, 
                    data = joined_data)

glance(poverty_model)


# Now having a look with hesitancy, this is higher than the others,
#   but still not really that big a deal on its own
#   we get an r-squared of 0.167.
hesitancy_model <- lm(percent_adults_fully_vaccinated ~ Estimated.hesitant, 
                      data = joined_data)

glance(hesitancy_model)

# Now having a look with unemployment
#   we get an r-squared of 0.0122
unemployment_model <- lm(percent_adults_fully_vaccinated ~ Unemployment_rate_2019, 
                         data = joined_data)

glance(unemployment_model)




# MODEL 2
# Dependent variable
#   = Hesitancy rates

# Potential independent variables 
#   = Unemployment_rate_2019
#   = poverty_percentage
#   = Median_Household_Income_2019 
#   = Education (there are four different levels, I'm not sure)
#   = Estimated.strongly.hesitant
#   = Estimated.hesitant
#   anything else?

# Interestingly, if you make hesitancy the dependent variable,
#   there does seem to be a very small correlation with education level and hesitancy

ggplot(joined_data, 
       aes(edu_percent_bachelors, Estimated.hesitant)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

hesitancy_education_model <- lm(Estimated.hesitant ~ edu_percent_bachelors, 
                                data = joined_data)

glance(hesitancy_education_model)
glimpse(joined_data)



# We gotta think of a far better name for this project too... ha

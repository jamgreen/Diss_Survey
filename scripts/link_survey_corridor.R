#link to the corridor data with the survey data
#just link manually, going to be annoying

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, tidyr,here, stringr)


surv <- read_csv(here::here("data/survey_results_2018Jan29.csv"))
surv <- surv[-1, ]

surv <- surv %>% filter(Finished == TRUE, StartQuestion != "No, Thank You")

surv <- surv %>% select(-1:-6, -13:-16, -34:-63)

#give new IDs with recode
surv <- surv %>% 
  mutate(city_id = case_when(CityName == "Central Puget Sound region" ~ 1,
                             CityName =="San Francisco" ~ 2,
                             CityName =="City of Jacksonville" ~ 3,
                             CityName == "Baltimore" ~ 4,
                             CityName == "Fort Worth" ~ 5,
                             CityName == "Albuquerque" ~ 6,
                             CityName == "Tucson" ~ 7,
                             CityName == "Raleigh, NC" ~ 8,
                             CityName == "Minneapolis" ~ 9,
                             CityName == "Dallas, Texas" ~ 10,
                             CityName == "Milwaukee" ~ 11,
                             CityName == "Philadelphia, Pennsylvania" ~ 12,
                             CityName == "City of las Vegas" ~ 13,
                             CityName == "City of San Diego" ~ 14,
                             CityName == "Seattle" ~ 15,
                             CityName == "City of Portland" ~ 16,
                             CityName == "Indianapolis, IN" ~ 17,
                             CityName == "Charlotte, NC" ~ 18,
                             CityName == "New York City" ~ 19))

policy <- read_csv(here::here("data/PreservationPolicies - Cities.csv"))

policy <- policy %>% 
  mutate(city_id = case_when(City == "Philadelphia" ~ 12,
                             City == "San Diego" ~ 14,
                             City == "Jacksonville" ~ 3,
                             City == "Indianapolis" ~ 17,
                             City == "San Francisco" ~ 2,
                             City == "Fort Worth" ~ 5,
                             City == "Charlotte" ~ 18,
                             City == "Seattle" ~ 15,
                             City == "Baltimore" ~ 4,
                             City == "Portland" ~ 16,
                             City == "Milwaukee" ~ 11,
                             City == "Las Vegas" ~ 13,
                             City == "Albuquerque" ~ 6,
                             City == "Tucson" ~ 7,
                             City == "Raleigh" ~ 8,
                             City == "Minneapolis" ~ 9,
                             City == "New York" ~ 19,
                             City == "Dallas" ~ 10))

policy_combined <- surv %>% inner_join(policy)

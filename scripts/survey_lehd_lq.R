#working through survey answers for outline

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, tidyr,here, devtools, tigris, sf)
options(tigris_class = "sf")

install_github("jamgreen/lehdr")
library(lehdr)

surv <- read_csv("data/survey_policies_combined.csv")
surv <- surv[-1, ]

surv %>% group_by(`Does your city have a comprehensive plan?`) %>% 
  tally()
surv %>% group_by(`If so, when was the last time the city completed a comprehensive plan revision or update? - Year`) %>% 
  tally()

surv %>% 
  group_by(`In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`, CityName) %>% 
  tally()

#join lehd numbers earlyish like 2005 and 2015 to get change over time

states_lehd <- c("ca", "tx", "fl", "az", "nm", "md", "ny", "pa", "nv", "or", 
                 "wa", "nc", "in", "mn", "wi")

years <- c(2005, 2015)

lehd_states <- grab_lodes(state = states_lehd, year = years, lodes_type = "wac",
                           job_type =  "JT01", segment = "S000", agg_geo = "tract", 
                          download_dir = "lodes_raw")

lehd_states <- lehd_states %>% 
  select(1:4, mfg_emp = CNS05, whole_trade = CNS06, transpo = CNS08)

state_tracts <- map(states_lehd, .f = tracts)
state_places <- map(states_lehd, .f = places)


state_tracts <- rbind_tigris(state_tracts)
state_places <- rbind_tigris(state_places)

surv_city_names <- c("Raleigh", "San Diego", "New York", "Seattle", "Tucson",
                     "San Francisco", "Jacksonville", "Baltimore", "Fort Worth",
                     "Albuquerque", "Minneapolis", "Dallas", "Milwaukee",
                     "Philadelphia", "Las Vegas", "Portland", "Indianapolis",
                     "Charlotte")

surv_city_geoid <- c("0477000", "0667000", "0666000","1235000","2404000",
                     "3240000", "3502000", "3651000", "3712000",
                     "3755000", "4159000", "4260000", "4819000", "4827000",
                     "5363000", "1836003", "3502000", "2743000", "5553000")

state_lehd_places <- state_places %>% 
  filter(GEOID %in% surv_city_geoid)

state_lehd_places <- state_lehd_places %>% 
  select(4:5)

lehd_place_tracts <- st_join(state_tracts, state_lehd_places, left = FALSE,
                             suffix = c("_tract", "_place"))

lehd_place_tracts <- lehd_place_tracts %>% 
  select(1, 4, 13, 14)


#join place_lehd tracts to lehd data then spread for the two years

lehd_place_tracts <- lehd_place_tracts %>% 
  left_join(lehd_states, by = c("GEOID_tract" = "w_tract"))

lehd_place_wide <- lehd_place_tracts %>% 
  as.data.frame() %>% select(-geometry) %>% 
  gather(job_type, employment, 7:10)

lehd_place_wide <- lehd_place_wide %>% 
  unite(job_year, job_type, year,  sep = "_")

lehd_place_wide <- lehd_place_wide %>% 
  spread(job_year, c("employment"))

lehd_place_wide <- lehd_place_wide %>% 
select(-ends_with("_NA"))

#sum up lehd state employment and join to lehd_place_wide

lehd_states <- lehd_states %>% 
  group_by(state, year) %>% 
  summarise_if(is.numeric, sum)

lehd_states <- lehd_states %>% 
  gather(job_type, employment, 3:6)

lehd_states <- lehd_states %>% 
  unite(job_year, job_type, year, sep = "_") %>% 
  spread(job_year, c("employment"))

names(lehd_states) <- paste0("state_", names(lehd_states))

lehd_place_wide <- lehd_place_wide %>% 
  left_join(lehd_states, by = c("state"= "state_state"))

#sum up city employment numbers, then calculate LQs

lehd_place_wide <- lehd_place_wide %>% 
  group_by(NAME_place) %>% 
  mutate(city_C000_2005 = sum(C000_2005, na.rm = TRUE), 
         city_C000_2015 = sum(C000_2015, na.rm = TRUE),
         city_mfg_2005 = sum(mfg_emp_2005, na.rm = TRUE), 
         city_mfg_2015 = sum(mfg_emp_2015, na.rm = TRUE),
         city_transpo_2005 = sum(transpo_2005, na.rm = TRUE), 
         city_transpo_2015 = sum(transpo_2015, na.rm = TRUE), 
         city_whole_trade_2005 = sum(whole_trade_2005, na.rm = TRUE),
         city_whole_trade_2015 = sum(whole_trade_2015, na.rm = TRUE)) %>% 
  ungroup()

lehd_place_wide <- lehd_place_wide %>% 
  mutate(city_mfg_per2005 = city_mfg_2005/city_C000_2005,
         city_transpo_per2005 = city_transpo_2005/city_C000_2005,
         city_trade_per2005 = city_whole_trade_2005/city_C000_2005,
         city_mfg_per2015 = city_mfg_2015/city_C000_2015,
         city_transpo_per2015 = city_transpo_2015/city_C000_2015,
         city_trade_per2015 = city_whole_trade_2015/city_C000_2015,
         state_mfg_per2005 = state_mfg_emp_2005/state_C000_2005,
         state_transpo_per2005 = state_transpo_2005/state_C000_2005,
         state_trade_per2005 = state_whole_trade_2005/state_C000_2005,
         state_mfg_per2015 = state_mfg_emp_2015/state_C000_2015,
         state_transpo_per2015 = state_transpo_2015/state_C000_2015,
         state_trade_per2015 = state_whole_trade_2015/state_C000_2015)

lehd_place_lq <- lehd_place_wide %>% 
  select(NAME_place, state, 14:41) %>% 
  filter(!is.na(state)) %>% 
  distinct()

lehd_place_lq <- lehd_place_lq[-17, ]

lehd_place_lq <- lehd_place_lq %>% 
  mutate(lq_mfg2005 = city_mfg_per2005/state_mfg_per2005,
         lq_mfg2015 = city_mfg_per2015/state_mfg_per2015,
         lq_transpo2005 = city_transpo_per2005/state_transpo_per2005,
         lq_trade2005 = city_trade_per2005/state_trade_per2005,
         lq_trade2015 = city_trade_per2015/state_trade_per2015) %>% 
  select(1:2, 11:18, contains("per"), contains("lq_"))

lehd_place_lq <- lehd_place_lq %>% 
  mutate(city_emp_growth = (city_C000_2015 - city_C000_2005)/city_C000_2005,
         city_mfg_growth = (city_mfg_2015 - city_mfg_2005)/city_mfg_2005,
         city_trade_growth = (city_whole_trade_2015 - city_whole_trade_2005)/city_whole_trade_2005,
         city_transpo_growth = (city_transpo_2015 - city_transpo_2005)/city_transpo_2005)

lehd_place_lq <- lehd_place_lq %>% 
  mutate(city_id = case_when(NAME_place == "Philadelphia" ~ 12,
                           NAME_place == "San Diego" ~ 14,
                           NAME_place == "Jacksonville" ~ 3,
                           NAME_place == "Indianapolis city (balance)" ~ 17,
                           NAME_place == "San Francisco" ~ 2,
                           NAME_place == "Fort Worth" ~ 5,
                           NAME_place == "Charlotte" ~ 18,
                           NAME_place == "Seattle" ~ 15,
                           NAME_place == "Baltimore" ~ 4,
                           NAME_place == "Portland" ~ 16,
                           NAME_place == "Milwaukee" ~ 11,
                           NAME_place == "Las Vegas" ~ 13,
                           NAME_place == "Albuquerque" ~ 6,
                           NAME_place == "Tucson" ~ 7,
                           NAME_place == "Raleigh" ~ 8,
                           NAME_place == "Minneapolis" ~ 9,
                           NAME_place == "New York" ~ 19,
                           NAME_place == "Dallas" ~ 10))

write_csv(lehd_place_lq, "data/survey_city_lehd_lq.csv")

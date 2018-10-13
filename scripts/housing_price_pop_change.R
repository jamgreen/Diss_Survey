#identify strong and weak market cities for exploratory graphing
if(!require(pacman)){install.packages(pacman); library(pacman)}
p_load(tidyverse, tidycensus, sf)

#grab 2000 and 5 year acs 16 city population and house price data...adjust to 2010 prices

census_api_key("c67f1b3134300374d51a55c543649f843fb7d2b3", install = TRUE)

tables_2000 <- tidycensus::load_variables(year = 2000, dataset = "sf3", TRUE)
states_census <- c("ca", "tx", "fl", "az", "nm", "md", "ny", "pa", "nv", "or", 
                 "wa", "nc", "in", "mn", "wi")

surv_city_geoid <- c("0477000", "0667000", "0666000","1235000","2404000",
                     "3240000", "3502000", "3651000", "3712000",
                     "3755000", "4159000", "4260000", "4819000", "4827000",
                     "5363000", "1836003", "3502000", "2743000", "5553000")

#median value of owner occupied housing units and tot pop 2000

house_val_tot_pop2000 <- get_decennial(geography = "place", variables = c("H085001", "P001001"),
                                       year = 2000, state = states_census, geometry = FALSE, output = "wide")

house_val_tot_pop2000 <- house_val_tot_pop2000 %>% 
  filter(GEOID %in% surv_city_geoid)

house_val_tot_pop2000 <- house_val_tot_pop2000 %>% 
  rename(med_house_price2000 = H085001, tot_pop2000 = P001001)

#median value of owner occupied housing units and tot pop acs 2016

tables_2016 <- tidycensus::load_variables(year = 2016, dataset = "acs5", TRUE)

house_val_tot_pop2016 <- get_acs(geography = "place", variables = c("B25077_001", "B01001_001"), year = 2016,
                                 state = states_census, geometry = FALSE, output = "wide")

house_val_tot_pop2016 <- house_val_tot_pop2016 %>% 
  filter(GEOID %in% surv_city_geoid) %>% 
  select(GEOID, NAME, med_house_price2016 = B25077_001E, tot_pop2016 = B01001_001E)

#join together and calculate to 2010 dollars using BLS CPI Calculator to September 2016 prices
#https://www.bls.gov/data/inflation_calculator.htm

house_val_tot_pop <- house_val_tot_pop2000 %>% 
  inner_join(house_val_tot_pop2016, by = "GEOID") %>% 
  rename(NAME = NAME.x) %>% select(-NAME.y)

house_val_tot_pop <- house_val_tot_pop %>% 
  mutate(med_price_2000_cpi = med_house_price2000 * 1.4303,
         med_house_price_change = (med_house_price2016 - med_price_2000_cpi)/med_price_2000_cpi,
         tot_pop_change = (tot_pop2016 - tot_pop2000)/tot_pop2000)

readr::write_csv(house_val_tot_pop, "data/house_val_tot_pop.csv")


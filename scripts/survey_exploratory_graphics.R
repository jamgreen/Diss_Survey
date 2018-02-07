#some initial plots of survey questions both with and w/out likert
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(likert, purrr, ggthemes, tidyverse)

surv1 <- read_csv("data/survey_results_2018Jan29.csv")
surv1 <- surv1[-1,]
surv1 <- surv1 %>% filter(Finished == "TRUE")

#comp plan section dealing with industrial land-----

comp_plan1 <- surv1 %>% 
  group_by(`In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  summarise(N = n())
comp_plan1 <- comp_plan1 %>% 
  rename(CompPlanIndustrial = `In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  filter(!is.na(CompPlanIndustrial))

comp_plot1 <- ggplot(comp_plan1, aes(x = CompPlanIndustrial, y = N)) +
  geom_bar(stat = "identity") + labs(y = "No. of Responses") + theme_bw()

#comp plan orientiation to industrial land----
comp_plan2 <- surv1 %>% 
  rename(IndustrialOrientation = `Which of the following best describes your current comprehensive plan's orientation toward converting industrial land to non-industrial uses?`) %>% 
  group_by(IndustrialOrientation) %>% filter(!is.na(IndustrialOrientation)) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N))

comp_plan2$IndustrialOrientation <- factor(comp_plan2$IndustrialOrientation, levels = 
                                             c("Proactively preserve or expand industrial lands", 
                                               "Concerned but not actively preventing conversion of industrial lands",
                                               "Neutral or mixed orientation toward conversion/preservation",
                                               "Does not limit or prevent conversion of industrial land",
                                               "Not sure"))


comp_plot2 <- ggplot(comp_plan2, aes(x = IndustrialOrientation, y = Share)) +
  geom_col(position = "stack") + coord_flip() +
  labs( x = "Comp. Plan Orientation", y = "% Share") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border = element_blank()) + 
  scale_x_discrete(limits =  c("Not sure",
                               "Does not limit or prevent conversion of industrial land",
                               "Neutral or mixed orientation toward conversion/preservation",
                               "Concerned but not actively preventing conversion of industrial lands",
                               "Proactively preserve or expand industrial lands"))

#would u characterize your city as a port, if so, is the port protection policy----
comp_plan3 <- surv1 %>% 
  group_by(`Would you characterize your city as a trade hub or port (namely, is a significant portion of local employment connected to the movement of goods either through a port, airport, or trucking depots)?`) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N)) %>% 
  rename(Port = `Would you characterize your city as a trade hub or port (namely, is a significant portion of local employment connected to the movement of goods either through a port, airport, or trucking depots)?`)

comp_plan3.5 <- surv1 %>% 
  group_by(`Does your city have an industrial land preservation or conservation policy specific to the port?`) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N)) %>% 
  rename(PortProtection = `Does your city have an industrial land preservation or conservation policy specific to the port?`)

comp_plan3.5 <- comp_plan3.5[-3,]

comp_plot3 <- ggplot(comp_plan3, aes(Port, y = Share)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Do you consider your city to be a trade hub?", y = "% Share")

comp_plot3.5 <- ggplot(comp_plan3.5, aes(PortProtection, y = Share)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Does your city have a land preservation policy\n specific to the port?", y = "% Share")

#industrial land inventory----
comp_plan4 <- surv1 %>% 
  group_by(`Has your city conducted an industrial land inventory? An industrial land inventory is a study that estimates current and future supplies of industrial zoned land including, but not limited to: available acreage, vacancy rates, future demand projections, a`) %>% 
  summarise(N = n()) %>% ungroup() %>% 
  rename(Inventory = `Has your city conducted an industrial land inventory? An industrial land inventory is a study that estimates current and future supplies of industrial zoned land including, but not limited to: available acreage, vacancy rates, future demand projections, a`) %>% 
  ungroup() %>% mutate(Share = N/sum(N))

comp_plot4 <- ggplot(comp_plan4, aes(Inventory, Share)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Has your city conducted an industrial land inventory?", y = "% Share")

comp_plan5 <- surv1 %>% 
  select(`In what year was the latest inventory taken? - Year`)

comp_plan5[9,1] <- "2016"
comp_plan5[5,1] <- "2017"
  
comp_plan5 <- comp_plan5 %>%   
  group_by(`In what year was the latest inventory taken? - Year`) %>% 
  summarise(N = n()) %>%  ungroup() %>% 
  rename(inventory_year = `In what year was the latest inventory taken? - Year`) %>% 
  filter(!is.na(inventory_year))


comp_plot5 <- ggplot(comp_plan5, aes(inventory_year, N)) +
  geom_col(position = "stack") + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "What was the latest year an industrial land inventory was taken?", 
       y = "No. of Responses")
#port land protections----

port_protect <- surv1 %>% 
  group_by(`Does your city have an industrial land preservation or conservation policy specific to the port?`) %>% 
  summarise(N = n()) %>% ungroup() %>% 
  rename(PortProtection = `Does your city have an industrial land preservation or conservation policy specific to the port?`) %>% 
  mutate(Share = N/sum(N)) %>% filter(!is.na(PortProtection))

port_plot <- ggplot(port_protect, aes(PortProtection, Share)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border =  element_blank()) +
  labs(x = "Does your city have an additional land preservation or conservation policy for the port?")

#estimated current land supply----
industry_supply <- surv1 %>% 
  group_by(`How would you characterize the current supply of your industrially zoned land? Approximately, how long would it take for your industrial land to be fully developed?`) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N)) %>% 
  rename(LandSupply = `How would you characterize the current supply of your industrially zoned land? Approximately, how long would it take for your industrial land to be fully developed?`) %>% 
  filter(!is.na(LandSupply))

industry_supply$LandSupply <- factor(industry_supply$LandSupply, levels = 
                                       c("1-5 years", "6-10 years", "11-15 years",
                                         "16-20 years", "21-25 years", "More than 25 years"))

supply_plot <- ggplot(industry_supply, aes(LandSupply, Share)) +
  geom_col(position = "stack") + scale_y_continuous(labels = scales::percent) +
  theme_bw() + theme(panel.border = element_blank()) + 
  labs(x = "How would you characterize the current supply of your industrially zoned land?",
       y = "% Share")

supply_plot2 <- ggplot(industry_supply, aes(LandSupply, N)) +
  geom_col(position = "stack") + theme_bw() + 
  theme(panel.border = element_blank()) + 
  labs(x = "How would you characterize the current supply of your industrially zoned land?",
       y = "No. of Respondents")

#attempting first likert visualizations----

districts <- surv1[, 44:63]

district1 <- districts %>% select(starts_with("District 1"))
district1 <- data.frame(map(district1, factor, levels = c("Strongly disagree", "Disagree", "Somewhat disagree",
                                               "Neither agree nor disagree", "Somewhat agree", "Agree",
                                               "Strongly agree")))

district1 <- district1 %>% 
  rename(`This district is facing redevelopment pressure into non-industrial uses` = 
           District.1.Redevelopment.Pressure,
         `The businesses in this district are economically healthy` = 
           District.1.Businesses.are.healthy,
         `There is a concern about displacement of industrial uses and users in this district` = 
           District.1.concerns.about.displacement,
         `The city currently has a set of policies or strategies to protect industrial users in this district` = 
           District.1.the.city.has.policies.in.place.to.protect.the.district)

district1.likert <- likert(district1)

# district 2
district2 <- districts[, 5:8]

district2 <- data.frame(map(district2, factor, levels = c("Strongly disagree", "Disagree", "Somewhat disagree",
                                                          "Neither agree nor disagree", "Somewhat agree", "Agree",
                                                          "Strongly agree")))

district2 <- district2 %>% 
  rename(`This district is facing redevelopment pressure into non-industrial uses` = 
           District.2.Redevelopment.Pressure,
         `The businesses in this district are economically healthy` = 
          District.2.Businesses.are.healthy,
         `There is a concern about displacement of industrial uses and users in this district` = 
          District.2.concerns.about.displacement,
         `The city currently has a set of policies or strategies to protect industrial users in this district` = 
           District.2.the.city.has.policies.in.place.to.protect.the.district)

district2.likert <- likert(district2)

#city manufacturing policy questions----

city_mfg <- surv1 %>% 
  select( `On a scale from extremely important to not important at all, how would you characterize your city's position on urban manufacturing as part of its overall economic development strategy?`,
        `Does your city currently have an urban manufacturing strategy?`) 

city_mfg$`On a scale from extremely important to not important at all, how would you characterize your city's position on urban manufacturing as part of its overall economic development strategy?` <- 
  factor(city_mfg$`On a scale from extremely important to not important at all, how would you characterize your city's position on urban manufacturing as part of its overall economic development strategy?`, 
         levels = c("Not at all important", "Slightly important", 
                      "Moderately important", "Very important", 
                    "Extremely important"))

city_mfg.likert <- data.frame(city_mfg$`On a scale from extremely important to not important at all, how would you characterize your city's position on urban manufacturing as part of its overall economic development strategy?`)
city_mfg.likert <- likert(city_mfg.likert)

city_mfg1 <- city_mfg %>% 
  group_by(`Does your city currently have an urban manufacturing strategy?`) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N)) %>% 
  filter(!is.na(`Does your city currently have an urban manufacturing strategy?`))

city_mfg_plot <- ggplot(city_mfg1, aes(`Does your city currently have an urban manufacturing strategy?`,
                                      Share)) +
  geom_col() + theme_bw() + scale_y_continuous(labels = scales::percent) +
  theme(panel.border = element_blank())

#title/position -----

title <- surv1 %>% 
  select(Title_Position, 66:67)

title <- title %>% 
  rename(responsiblity = `Is your agency primarily responsible for the implementation of these strategies?`,
          other_agency = `What agency is responsible for the implementation of these strategies?`)

ELSE <- TRUE

title <- title  %>% 
  mutate(cleaned_position = 
        case_when(grepl("Planner|Planning", title$Title_Position, ignore.case = T) ~ "Planning",
                  grepl("Economic", title$Title_Position, ignore.case = T) ~ "Economic Development",
                  grepl("Redevelopment", title$Title_Position, ignore.case = T) ~ "Redevelopment Agency",
                  grepl("PIDC", title$Title_Position, ignore.case = T) ~ "Redevelopment Agency",
        ELSE ~ "Other"))
title <- title %>% 
  mutate(cleaned_responsibility = 
           case_when(grepl("Planner|Planning", title$other_agency, ignore.case = T) ~ "Planning",
                     grepl("Economic|Commerce|EDC", title$other_agency, ignore.case = T) ~ "Economic Development",
                     grepl("Redevelopment", title$other_agency, ignore.case = T) ~ "Redevelopment Agency",
                     grepl("PIDC|Portland|Indy", title$other_agency, ignore.case = T) ~ "Redevelopment Agency",
                     grepl("Corporation", title$other_agency, ignore.case = T) ~ "Redevelopment Agency",
                     ELSE ~ "Other"))

responsibility <- title %>% group_by(responsiblity) %>% summarise(N = n()) %>% 
  ungroup() %>% mutate(Share = N/sum(N))


title_overall <- title %>% group_by(cleaned_position) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N))

title_primary <- title %>% filter(responsiblity == "Yes") %>% group_by(cleaned_position) %>% 
  summarise(N = n()) %>% ungroup() %>% mutate(Share = N/sum(N))

title_other_agency <- title %>% filter(responsiblity == "No") %>% 
  group_by(cleaned_responsibility) %>% summarise(N = n()) %>% 
  ungroup() %>% mutate(Share = N/sum(N))

responsibility_plot <- ggplot(responsibility, aes(responsiblity, N)) +
  geom_col() + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Is your agency primarily responsible for the implementation of these strategies?",
       y = "No. of Respondents") +scale_y_continuous(breaks = c(2,4,6,8,10,12,14))

title_plot <- ggplot(title_overall, aes(cleaned_position, Share)) +
  geom_col() + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Home Agency of Respondents") + coord_flip() +
  scale_y_continuous(labels = scales::percent)

other_agency_plot <- ggplot(title_other_agency, aes(cleaned_responsibility, Share)) +
  geom_col() + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "What agency is primarily responsible for urban manufacturing strategies?") +
  scale_y_continuous(labels = scales::percent) + coord_flip()

#import of urban strategies------

import_strat <- surv1 %>% select(68:72)


import_strat1 <- import_strat %>% 
  select(1,2,3,5)

import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Skills training, and other workforce development policies, are an important part of my city's urban manufacturing strategy` <-
  factor(import_stra1t$`How important are the following areas for your city's urban manufacturing strategy? - Skills training, and other workforce development policies, are an important part of my city's urban manufacturing strategy`,
         levels = c( "Moderately important", "Very important", 
                     "Extremely important"))

import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Conserving industrial land` <-
  factor(import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Conserving industrial land`,
         levels = c( "Moderately important", "Very important", 
                     "Extremely important"))

import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Industrial retention` <-
  factor(import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Industrial retention`,
         levels = c( "Moderately important", "Very important", 
                     "Extremely important"))

import_strat1$`How important are the following areas for your city's urban manufacturing strategy? - Innovation and research and development` <-
  factor(import_strat$`How important are the following areas for your city's urban manufacturing strategy? - Innovation and research and development`,
         levels = c( "Slightly Important", "Moderately important", "Very important", 
                     "Extremely important"))

import_strat1 <- data.frame(import_strat1)

names(import_strat1) <- c("How important are the following areas for your city's urban manufacturing strategy? Skills training and Workforce Development",
                          "How important are the following areas for your city's urban manufacturing strategy? Industrial Retention",
                          "How important are the following areas for your city's urban manufacturing strategy? Conserving Industrial Land",
                          "How important are the following areas for your city's urban manufacturing strategy? Innovation and research and development")

import_strat1.likert <- likert(import_strat1)


# the other columns with 4 factor levels

import_strat$`How important are the following areas for your city's urban manufacturing strategy? - Industrial Recruitment` <-
  factor(import_strat$`How important are the following areas for your city's urban manufacturing strategy? - Industrial Recruitment`,
         levels = c("Slightly important", 
                    "Moderately important", "Very important", 
                    "Extremely important"))



import_strat2 <- import_strat %>% select(4)
import_strat2 <- data.frame(import_strat2)
names(import_strat2) <- "How important are the following areas for your city's urban manufacturing strategy? Industrial Recruitment"
                          

import_strat2.likert <- likert(import_strat2)

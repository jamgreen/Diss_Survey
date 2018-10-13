if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(likert, purrr, ggthemes, tidyverse, janitor, pander, kableExtra, vcd)

surv1 <- read_csv("data/survey_policies_combined.csv")
surv1 <- surv1[-1,]

surv1 <- surv1 %>% 
  filter(StartQuestion == "Start", 
         CityName != "Central Puget Sound region")
#comp plan section dealing with industrial land-----

comp_plan1 <- surv1 %>% 
  group_by(`In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  summarise(N = n())
comp_plan1 <- comp_plan1 %>% 
  rename(CompPlanIndustrial = `In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  filter(!is.na(CompPlanIndustrial))

comp_plot1 <- ggplot(comp_plan1, aes(x = CompPlanIndustrial, y = N)) +
  geom_bar(stat = "identity") + labs(y = "No. of Responses") + theme_minimal() +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14)) +
  labs(x = "In your city's current comprehensive plan is there a section dealing\n specifically with the management of industrial land?")


comp_plan2 <- surv1 %>% 
  group_by(`In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  summarise(N = n()) %>% ungroup() %>% 
  mutate(Share = N/sum(N)) %>% 
  rename(CompPlanIndustrial = `In your city's current comprehensive plan is there a section dealing specifically with the management of industrial land?`) %>% 
  filter(!is.na(CompPlanIndustrial))

comp_plot_compplan <- ggplot(comp_plan2, aes(x = CompPlanIndustrial, y = Share)) +
  geom_bar(stat = "identity") + labs(y = "") + theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "In your city's current comprehensive plan is there a section dealing\n specifically with the management of industrial land?")

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
  theme_minimal() + theme(panel.border = element_blank()) + 
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

comp_plan5[4, ] <- "2017"
comp_plan5[8, ] <- "2016"
comp_plan5[12, ] <- "Not Sure"


comp_plan5 <- comp_plan5 %>%   
  group_by(`In what year was the latest inventory taken? - Year`) %>% 
  summarise(N = n()) %>%  ungroup() %>% 
  rename(inventory_year = `In what year was the latest inventory taken? - Year`) %>% 
  filter(!is.na(inventory_year))

comp_plot5 <- ggplot(comp_plan5, aes(inventory_year, N)) +
  geom_col(position = "stack") + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "What was the latest year an industrial land inventory was taken?", 
       y = "No. of Responses")

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

#crosstab industrial land study and having policy-----------------

surv1 <- surv1 %>% 
  rename(`Has your city conducted an industrial land inventory?` = `Has your city conducted an industrial land inventory? An industrial land inventory is a study that estimates current and future supplies of industrial zoned land including, but not limited to: available acreage, vacancy rates, future demand projections, a`) 

surv1 <- surv1 %>% 
  mutate(IndustrialPolicyCat = if_else(IndustrialPolicy == 1, "Yes", "No")) %>% 
  rename(`Industrial Land Preservation Policy` = IndustrialPolicyCat)



surv1 %>% 
  tabyl(`Has your city conducted an industrial land inventory?`, `Industrial Land Preservation Policy`, 
        show_na = FALSE) %>% 
  adorn_totals(c("row", "col")) %>% 
  pander(caption = "Cities that have performed industrial land surveys have 
         protective policies", style = "rmarkdown")

# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Generate a heat-map that highlights the change in opioid overdose rates from 2017 to 2018 under 
# alternative proportions of opioid involvement in unidentified drug overdoses. This figure is meant to 
# show how the overall inference of a larger decline in opioid overdoses than reported is driven, to a large
# degree, by decreasing #s of unidentified drug overdoses over time.

# Import 2017-2018 drug overdose data ----------------------

overdose_deaths_2017 <- readRDS(paste0('//smdnas/Hill_Lab/Mortality/RDS/mort_drug_overdoses_', as.character(2017), 
                                       '_inter.rds'))
overdose_deaths_2018 <- readRDS(paste0('//smdnas/Hill_Lab/Mortality/RDS/mort_drug_overdoses_', as.character(2018),
                                       '_inter.rds'))

# Establish number of opioid/unidentified overdoses in each year -----------------

overdose_summaries <- data.frame()

for(fff in 2017:2018) {
  
  temp <- get(paste0('overdose_deaths_', fff))
  
  temp %>% filter(any_opioid == 1) %>%
    summarise(opioid_overdoses = n()) %>% mutate(year = fff) -> temp_opioid
  
  temp %>% filter(unidentified_drug_only == 1) %>%
    summarise(missing_overdoses = n()) %>% mutate(year = fff) -> temp_unidentified
  
  temp_summary <- temp_opioid %>% left_join(temp_unidentified)
  
  overdose_summaries %<>% bind_rows(temp_summary)
  
  rm(temp, fff, temp_summary, temp_opioid, temp_unidentified)
  
}

# Spread wide

temp <- overdose_summaries %>% mutate(hi = 1) %>% setDT() %>%
  dcast(hi ~ year, value.var = c('opioid_overdoses', 'missing_overdoses')) %>%
  select(-hi)

# Expand 100 times for each year for heat map

temp <- bind_rows(replicate(20, temp, simplify = FALSE))

# Set up parameters

temp %<>% mutate(opioid_proportion_2017 = row_number() * 0.05)

temp <- bind_rows(replicate(20, temp, simplify = FALSE))

temp %<>% group_by(opioid_proportion_2017) %>%
  mutate(opioid_proportion_2018 = row_number() * 0.05) %>%
  ungroup()

# Establish opioid overdoses and change over time for each parameter set

temp %<>% mutate(new_opioid_overdoses_2017 = opioid_overdoses_2017 + missing_overdoses_2017 * opioid_proportion_2017,
                 new_opioid_overdoses_2018 = opioid_overdoses_2018 + missing_overdoses_2018 * opioid_proportion_2018,
                 change = (new_opioid_overdoses_2018 - new_opioid_overdoses_2017) / new_opioid_overdoses_2017)

# What is the typical proportion of opioid overdoses in the known record? ------------

overdose_deaths_2017 %>% filter(unidentified_drug_only == 0) %>%
  summarise(mean_opioid = mean(any_opioid, na.rm = TRUE)) %>% pull() -> opioid_prop_2017

overdose_deaths_2018 %>% filter(unidentified_drug_only == 0) %>%
  summarise(mean_opioid = mean(any_opioid, na.rm = TRUE)) %>% pull() -> opioid_prop_2018

averages <- data.frame(op_2017 = opioid_prop_2017, op_2018 = opioid_prop_2018)

# Create heat map ------------------

estimates <- data.frame(est_2017 = 0.7757128, est_2018 = 0.7346182)

overdose_heat_map <- ggplot(data = temp,
                            aes(x = opioid_proportion_2017, y = opioid_proportion_2018,
                                fill = change)) + 
  geom_tile() + theme_classic() + labs(title = '2017-2018 change in opioid overdoses under various assumptions?',
                                       subtitle = 'Assumed proportions of opioid involvement in unclassified overdoses, 0.05 increments',
                                       x = '% of unclassified drug overdoses with opioid involvement, 2017',
                                       y = '% of unclasified drug overdoses with opioid involvement, 2018') + 
  scale_fill_gradient(high = 'tomato2', low = 'dodgerblue') + 
  geom_point(data = averages,
             aes(x = op_2017, y = op_2018), 
             shape = 22, size = 5, fill = NA, color = 'black') + 
  geom_point(data = estimates,
             aes(x = est_2017, y = est_2018),
             shape = 24, size = 5, fill = NA, color = 'black') +
  theme(legend.title = element_blank())

overdose_heat_map

ggsave('Opioids_ML_2/Figures/Figure_X_Year_to_Year_Change_Assumption_Driven.jpg')

overdose_heat_map <- ggplot(data = temp,
                            aes(x = opioid_proportion_2017, y = opioid_proportion_2018,
                                fill = change)) + 
  geom_tile() + theme_classic() + labs(x = '% of unidentified drug overdoses with opioid involvement, 2017',
                                       y = '% of unidentified drug overdoses with opioid involvement, 2018') + 
  scale_fill_gradient(high = 'tomato2', low = 'dodgerblue') + 
  geom_point(data = averages,
             aes(x = op_2017, y = op_2018), 
             shape = 22, size = 5, fill = NA, color = 'black') + 
  theme(legend.title = element_blank())

overdose_heat_map

ggsave('Opioids_ML_2/Figures/Figure_X_Year_to_Year_Change_Assumption_Driven_Paper.jpg')


# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Estimate receiver operating curves for the various logistic regression models of interest. In this script, we
# use balanced databases between opioid and non-opioid overdoses. Can be adapted for unbalanced, as well. 

# Import data -----------------------

overdose_deaths <- data.frame()

for(fff in 1999:2019) {
  
  # Import yearly file
  
  temp <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))

  # Bind to overdose data frame
  
  overdose_deaths %<>% bind_rows(temp)
  
  rm(temp)
  
}

# Tabulate any_opioid ------------------

overdose_deaths %>% statar::tab(any_opioid)

overdose_deaths %<>% mutate_at(
  vars(narcotic:ethanol),
  funs(ifelse(. == 'TRUE', 1, 
              ifelse(. == 'FALSE', 0, .)))
)

overdose_deaths$any_opioid %<>% as.numeric()
overdose_deaths %<>% mutate(any_opioid = any_opioid - 1)

overdose_deaths %<>% mutate(
  overdose_type = case_when(
    any_opioid == 1 ~ 'Known opioid overdose',
    any_opioid == 0 & unidentified_drug_only == 0 ~ 'Known non-opioid overdose',
    unidentified_drug_only == 1 ~ 'Unidentified drug overdose'
  )
)

# Create figures -----------------------

# Total deaths by year

overdose_deaths %>% group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup() -> temp_figure

figure_plot <- ggplot(data = temp_figure,
                      aes(x = year, y = n_n)) + 
  geom_line(color = 'black') + theme_classic() + 
  labs(x = "Year", y = "# of drug overdoses")

figure_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Deaths_Total.jpg')

# Total overdoses & opioid overdoses and unidentified drug overdoses --------------------

overdose_deaths %>% group_by(year) %>%
  summarise(`Total Drug Overdoses` = n(),
            `Known Opioid Overdoses` = sum(any_opioid, na.rm = TRUE),
            `Unidentified Drug Overdoses` = sum(unidentified_drug_only, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(variable, value, -year) -> temp

temp$variable <- factor(temp$variable, levels = c('Total Drug Overdoses', 'Known Opioid Overdoses', 
                                                  'Unidentified Drug Overdoses'))
temp_plot <- ggplot(data = temp,
                    aes(x = year, y = value, colour = variable, group = variable)) + 
  theme_classic() + labs(x = 'Year', y = '# of overdoses, by category') + 
  geom_line() + geom_point() + theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_blank()) + 
  scale_colour_manual(values = c('#4575b4', '#d73027', 'wheat4'))

temp_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Drug_Overdoses_by_Category.jpg')

overdose_deaths %>% nrow() # number of overdoses, in total
overdose_deaths %>% summarise(any_opioid = sum(any_opioid, na.rm = TRUE)) # number of opioid overdoses

# % unidentified by year -----------------------

overdose_deaths %>% group_by(year, unidentified_drug_only) %>%
  summarise(n_n = n()) %>%
  ungroup() -> temp_figure

temp_figure %<>% dcast(year ~ unidentified_drug_only, value.var = c('n_n'))

temp_figure %<>% mutate(
  proportion = `1` / (`0` + `1`)
)

figure_plot <- ggplot(data = temp_figure,
                      aes(x = year, y = proportion)) + 
  geom_line(color = 'black') + geom_point(fill = 'blue', color = 'black', size = 3, shape = 21) + 
  theme_classic() + 
  labs(x = "Year", y = "% of drug overdoses with no drug classification") + 
  ylim(0, 0.3) + 
  theme(text = element_text(size = 12))

figure_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Unidentified_Percent.jpg')

# Calculate year-over-year changes

temp_figure %<>% arrange(year) %>%
  mutate(change = `1` - lag(`1`))

# Deaths by category

overdose_deaths %>% group_by(year, overdose_type) %>%
  summarise(n_n = n()) %>%
  ungroup() -> temp_figure

temp_figure$overdose_type <- factor(temp_figure$overdose_type, levels = c('Known opioid overdose',
                                                                          'Known non-opioid overdose',
                                                                          'Unidentified drug overdose'))

figure_plot <- ggplot(data = temp_figure,
                      aes(x = year, y = n_n, colour = overdose_type, group = overdose_type)) + 
  geom_point() + geom_line() + theme_classic() + 
  labs(x = "Year", y = "# of drug overdoses") + 
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.2, .8)) +
  scale_color_manual(values = c('#8e0152', '#f1b6da', '#276419'))

figure_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Deaths_by_Category.jpg')


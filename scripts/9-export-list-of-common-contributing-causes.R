# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Create a list of the top-100 contributing causes of death in 2018.


for(fff in 2017:2018) {
  
  # Import data --------------------
  
  overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # Summarize number of cases by contributing cause
  
  temp <- overdose_deaths %>% filter(year == fff) %>%
    select(104:203) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(`Contributing Cause of Death`, `# of drug overdoses with cause`)
  
  # Save as CSV file
  
  cc_2017 %>%  write_excel_csv(paste0('Opioids_ML_2/Scratch/Contributing_Causes_', as.character(fff), 
                                      '.csv'))
  
  # Create new file
  
  assign(x = paste0('cc_', as.character(fff)), value = temp)
  
  print(fff)
  
  rm(fff, temp)
  
}


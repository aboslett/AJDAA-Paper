# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

rm(list = ls())

# Goal: Add covariates to individual drug overdose files. Most of these operations could have been done
# in 1-set-up-death-database.R but some of the operations are fairly memory-intensive (e.g., the building out
# of a long database with key equal to decedent-contributing cause of death). The resulting file at the end of the loop
# will serve as the basis for most, if not all, scripts that analyze results.

# Set up loop for extract all drug overdoses ----------------------- 

overdose_deaths <- data.frame()

for(fff in 1999:2019) {
  
  # Import file ----------------------------
  
  temp <- readRDS(file = paste0(remote_drive_rds, '/', 'mort_drug_', as.character(fff), '.rds'))
  
  # Clean file ---------------------------
  
  # Characterize all drug-related variables
  
  temp %<>% mutate_at(vars(narcotic:ethanol),
                      funs(as.character(.)))
  
  # Execute basic cleaning
  
  temp %<>% mutate(
    id_overdose = 1:nrow(temp)
  )
  
  temp %<>% select(id_overdose, everything())
  
  names(temp)
  
  temp %<>% select(-asian, -less_than_hs, -age_20)
  
  temp$any_opioid %<>% as.factor()
  
  # Add days of the week
  
  temp$weekday %>% class 
  
  temp %<>% mutate(
    weekday_na = ifelse(is.na(weekday), 1, 0),
    weekday_1 = ifelse(weekday == 1 & !is.na(weekday), 1, 0),
    weekday_2 = ifelse(weekday == 2 & !is.na(weekday), 1, 0),
    weekday_3 = ifelse(weekday == 3 & !is.na(weekday), 1, 0),
    weekday_4 = ifelse(weekday == 4 & !is.na(weekday), 1, 0),
    weekday_5 = ifelse(weekday == 5 & !is.na(weekday), 1, 0),
    weekday_6 = ifelse(weekday == 6 & !is.na(weekday), 1, 0),
    weekday_7 = ifelse(weekday == 7 & !is.na(weekday), 1, 0),
    weekend = ifelse(weekday_1 == 1 | weekday_7 == 1, 1, 0)
  )
  
  names(temp)
  
  # Rename variables so that there are no spaces/limited puncutation 
  
  names(temp) %<>% str_replace_all(pattern = ' ', replacement = '') %>%
    str_replace_all(pattern = "\\,|\\-|\\'", replacement = '')
  
  # Make state FIPS code 
  
  temp %<>% mutate(
    state_fips_occ = str_sub(county_fips_occ, end = 2),
    state_fips_res = str_sub(county_fips_res, end = 2)
  )
  
  # True/False to 1, 0
  
  temp %<>% mutate_at(
    vars(narcotic:ethanol),
    funs(ifelse(. == TRUE, '1', 
                ifelse(. == FALSE, '0', .)))
  ) %>% mutate_at(vars(narcotic:ethanol), funs(as.numeric(.)))
  
  # Create indicator of unidentifed_drug_only
  
  temp %<>% mutate(
    unidentified_drug_only = ifelse(sedative == 0 & opium == 0 & heroin == 0 & cocaine == 0 & 
                                      other_narcotic == 0 & marijuana == 0 & lsd == 0 & hallucinogen == 0 &
                                      natural_opioid == 0 & methadone == 0 & synthetic_opioid == 0 & 
                                      psychotropic == 0 & unidentified_drug == 1 & other_drug == 0, 1, 0)
  )
  
  # Extract contributing causes of death
  
  temp %>% select(id_var, year, ucod, contains('record')) -> conditions
  
  conditions %<>% mutate_at(
    vars(contains('record')),
    funs(ifelse(. == "", NA, str_trim(.)))
  )
  
  # List all (record) conditions
  
  names(conditions)
  
  conditions %>% 
    select(-ucod, -record_1) %>%
    melt(measure = 3:21) %>%
    filter(!is.na(value)) %>%
    group_by(value) %>%
    summarise(number_of_records = n()) %>%
    ungroup() %>%
    arrange(-number_of_records) -> list_of_records
  
  # Drop overdose-related records
  
  list_of_records %<>% filter(!str_detect(value, pattern = 'T40') & 
                                !str_detect(value, pattern = 'T42') & 
                                !str_detect(value, pattern = 'T43') & 
                                !str_detect(value, pattern = 'T509') & 
                                !str_detect(value, pattern = 'T3[6-9]') & 
                                !str_detect(value, pattern = 'T41') & 
                                !str_detect(value, pattern = 'T4[4-9]') & 
                                !str_detect(value, pattern = 'T50[0-8]'))
  
  list_of_records %>% filter(str_detect(value, 
                                        pattern = 'X40|X41|X42|X43|X44|X60|X61|X62|X63|X64|X85|Y10|Y11|Y12|Y13|Y14')) -> ucods_in_records
  
  list_of_records %<>% filter(!str_detect(value, 
                                          pattern = 'X40|X41|X42|X43|X44|X60|X61|X62|X63|X64|X85|Y10|Y11|Y12|Y13|Y14'))
  
  # Keep top-150 CC
  
  list_of_records %<>% filter(row_number() <= 150)
  
  # Turn them into columns
  
  list_of_records$value %>% as.vector() -> records_vector
  
  for(zzz in records_vector) {
    
    conditions %>% mutate_at(vars(starts_with('record_')), 
                             funs(. %in% zzz)) %>% 
      select(record_2:record_20) %>% rowSums() -> conditions$temp
    
    conditions[[zzz]] <- conditions$temp
    
  }
  
  # Columnize UCODs
  
  ucods_vector <- c('X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61', 'X62', 'X63', 'X64', 
                    'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14')
  
  for(ggg in ucods_vector) {
    
    conditions %>% mutate_at(vars(ucod), 
                             funs(. %in% ggg)) %>% 
      select(ucod) %>% rowSums() -> conditions$temp
    
    conditions[[ggg]] <- conditions$temp
    
    rm(ggg)
    
  }
  
  # Merge
  
  conditions %<>% select(everything(), -temp, -ucod, -starts_with('record'))
  
  rm(records_vector, list_of_records, ucods_in_records)
  
  temp %<>% left_join(conditions, by = c('id_var', 'year'))
  
  # Drop educ89 and rectype for consitency across years
  
  temp %<>% select(-contains('educ89'), -contains('rectype'), -contains('autopsy_done'))
  
  # Add dead_hospice if year <= 2002
  
  if(fff <= 2002) {
    
    temp %<>% mutate(dead_hospice = 0)
    
  }
  
  temp %<>% select(-dead_hospice, dead_hospice)
  
  # Make any_opioid variable 0 and 1, not 1 and 2
  
  temp %<>% mutate(max_any_opioid = max(any_opioid, na.rm = TRUE),
                   any_opioid = ifelse(max_any_opioid == 2, any_opioid - 1, any_opioid))
  
  print(fff)
  temp %>% statar::tab(any_opioid)
  temp$any_opioid %>% class()
  
  temp %<>% select(-max_any_opioid)
  
  # Save as independent file by year ----------------------------------
  
  temp %>% saveRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # Bind rows
  
  overdose_deaths %<>% bind_rows(temp)
  
  # Remove files
  
  print(fff)
  
  rm(fff, temp)
  
}

# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Explore opioid abuse in non-overdose deaths. Generate models of opioid involvement in known cases. Predict new estimates
# of the number of opioid overdoses in the U.S. (and within state and county jurisdictions) over time. 

# Predict opioid involvement in unidentified drug overdoses ---------------------
# Note: Use logistic regression for now. 

all_unidentified <- data.frame()

# Regressions --------------------

for(fff in 1999:2019) {
  
  # Import data --------------------
  # Note: Annual data with drug overdoses + contributing cause indicators (top-150 most common) by year. To note, we did
  # not use this same method in our Addiction paper. We used the top-100, I believe, from the entire time period. This is a better
  # approach because some contributing causes have occurred more or less often over time, either due to incidence or due to
  # increased detection.
  
  overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # Select variables of interest
  # Note: Include outcome, decedent characteristics, and indicators of the top-100 contributing causes in the given
  # year.
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:203)
  
  # Filter out those deaths without any drug information.
  
  temp_unidentified <- overdose_deaths %>% filter(unidentified_drug_only == 1 & year == fff)
  
  # Drop all variables with no variation
  # Note: Will vary across time.
  
  temp %>% select(female:ncol(temp)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp %<>% select(-contains(xxx))
      temp_unidentified %<>% select(-contains(xxx))
    }
    
  }
  
  # Logistic regression model by year
  # Note: Of course, these models are ran only on those observations WITH drug information (opioid or not).
  
  temp$any_opioid %<>% as.factor()
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp)
  
  
  # Use model to make prediction based on covariates in data frame (for drug overdoses without information re:
  # drug of cause).
  
  temp_unidentified$any_opioid_pred <- predict(logistic_regression_model, 
                                               temp_unidentified, 
                                               type = "response")
  
  # Bind rows to unidentified data frame
  
  all_unidentified %<>% bind_rows(temp_unidentified)
  
  # Remove files
  
  print(fff)
  
  rm(temp, temp_unidentifeid, fff)
  
}

# Extract identified drug overdoses and merge with unidentified drug overdoses --------------------

known_overdoses <- data.frame()

for(fff in 1999:2019) {
  
  temp <- readRDS(paste0(recent_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  temp %<>% filter(unidentified_drug_only == 0)
  
  known_overdoses %<>% bind_rows(temp)
  
  print(fff)
  
  rm(fff, temp)
  
}

# Bind all overdoses together

drug_overdoses <- known_overdoses %>% bind_rows(all_unidentified)

# Calculate age adjusted rates of opioid overdoses in the U.S. (Census) -------------------------------

rm(list = setdiff(ls(), c('drug_overdoses')))
# Note: Resulting file has all drug overdoses + predictions of opioid involvement in drug overdoses without drug information.

# Get standard population data.
# Note: See Distribution #1 from cdc.gov/nchs/data/statnt/statnt20.pdf. See Distribution #1 (but in 5-year increments).

us_pop <- read.table(url("https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt"),
                     colClasses = c('character'))

# Filter out 2000 standard population
# Note: Dictionary is available at https://seer.cancer.gov/stdpopulations/stdpopdic.html

us_pop %<>% filter(str_sub(us_pop$V1, end = 3) == 201)

# Add age group 

us_pop %<>% mutate(
  age_group = case_when(
    str_sub(V1, start = 4, end = 6) == '000' ~ '00 years',
    str_sub(V1, start = 4, end = 6) == '001' ~ '01-04 years',
    str_sub(V1, start = 4, end = 6) == '002' ~ '05-09 years',
    str_sub(V1, start = 4, end = 6) == '003' ~ '10-14 years',
    str_sub(V1, start = 4, end = 6) == '004' ~ '15-19 years',
    str_sub(V1, start = 4, end = 6) == '005' ~ '20-24 years',
    str_sub(V1, start = 4, end = 6) == '006' ~ '25-29 years',
    str_sub(V1, start = 4, end = 6) == '007' ~ '30-34 years',
    str_sub(V1, start = 4, end = 6) == '008' ~ '35-39 years',
    str_sub(V1, start = 4, end = 6) == '009' ~ '40-44 years',
    str_sub(V1, start = 4, end = 6) == '010' ~ '45-49 years',
    str_sub(V1, start = 4, end = 6) == '011' ~ '50-54 years',
    str_sub(V1, start = 4, end = 6) == '012' ~ '55-59 years',
    str_sub(V1, start = 4, end = 6) == '013' ~ '60-64 years',
    str_sub(V1, start = 4, end = 6) == '014' ~ '65-69 years',
    str_sub(V1, start = 4, end = 6) == '015' ~ '70-74 years',
    str_sub(V1, start = 4, end = 6) == '016' ~ '75-79 years',
    str_sub(V1, start = 4, end = 6) == '017' ~ '80-84 years',
    str_sub(V1, start = 4, end = 6) == '018' ~ '85+ years',
  )
)

# Download recent population data
# Note: From NBER website (easier to download from zip folder and csv)

temp <- tempfile()
download.file("https://data.nber.org/seer-pop/uswbo19agesadj.csv.zip",temp)
recent_pop <- read_csv(unz(temp, "uswbo19agesadj.csv"))
unlink(temp)

names(recent_pop) <-  c('year', 'state', 'state_fips', 'county_fips',
                        'registry', 'race', 'origin', 'sex', 'age', 
                        'population')

# Select variables

us_pop %<>% dplyr::select(age_group, population)

# Filter out only 2017 and 2018 from drug overdose data

temp <- drug_overdoses %>% filter(year >= 2017)
recent_pop %<>% filter(year >= 2017)

# Select relevant variables in drug overdoses database

temp %<>% select(year, contains('age'), unidentified_drug_only,
                 any_opioid, any_opioid_pred)

# Make population numeric

us_pop %<>% mutate_at(vars(population),
                      funs(as.numeric(.)))

recent_pop %<>% mutate_at(vars(population),
                          funs(as.numeric(.)))

# Fill opioid_pred NAs with zeros

temp %<>% mutate_at(vars(any_opioid_pred), funs(ifelse(is.na(.) == TRUE, 0, .)))

# Summarise population by year

pop_summary <- recent_pop %>% group_by(year, age) %>%
  summarise_at(vars(population),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# Summarise drug overdoses by age and year

temp_summary <- temp %>% group_by(year, ager52) %>%
  summarise_at(vars(any_opioid, any_opioid_pred),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# Record drug overdoses age data to match populatin data from SEER

temp_summary %<>% mutate(
  age = case_when(
    ager52 <= 22 ~ '00 years',
    ager52 >= 23 & ager52 <= 26 ~ '01-04 years',
    ager52 == 27 ~ '05-09 years',
    ager52 == 28 ~ '10-14 years',
    ager52 == 29 ~ '15-19 years',
    ager52 == 30 ~ '20-24 years',
    ager52 == 31 ~ '25-29 years',
    ager52 == 32 ~ '30-34 years',
    ager52 == 33 ~ '35-39 years',
    ager52 == 34 ~ '40-44 years',
    ager52 == 35 ~ '45-49 years',
    ager52 == 36 ~ '50-54 years',
    ager52 == 37 ~ '55-59 years',
    ager52 == 38 ~ '60-64 years',
    ager52 == 39 ~ '65-69 years',
    ager52 == 40 ~ '70-74 years',
    ager52 == 41 ~ '75-79 years',
    ager52 == 42 ~ '80-84 years',
    ager52 >= 43 & ager52 <= 51 ~ '85+ years'
  )
)

pop_summary %<>% mutate(
  age = as.numeric(age),
  age = case_when(
    age == 0 ~ '00 years',
    age == 1 ~ '01-04 years',
    age == 2 ~ '05-09 years',
    age == 3 ~ '10-14 years',
    age == 4 ~ '15-19 years',
    age == 5 ~ '20-24 years',
    age == 6 ~ '25-29 years',
    age == 7 ~ '30-34 years',
    age == 8 ~ '35-39 years',
    age == 9 ~ '40-44 years',
    age == 10 ~ '45-49 years',
    age == 11 ~ '50-54 years',
    age == 12 ~ '55-59 years',
    age == 13 ~ '60-64 years',
    age == 14 ~ '65-69 years',
    age == 15 ~ '70-74 years',
    age == 16 ~ '75-79 years',
    age == 17 ~ '80-84 years',
    age == 18 ~ '85+ years'
  )
)
# Resummarise using new definition 

temp_summary %<>% filter(!is.na(age)) %>%
  group_by(year, age) %>% 
  summarise_at(vars(contains('opioid')),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

pop_summary %<>% group_by(age, year) %>%
  summarise(population  = sum(population, na.rm = TRUE)) %>%
  ungroup()

# Rename population variable in SEER databse

pop_summary %<>% select(everything(), population_current = population)

# Join population data to drug overdoses data

temp_summary %<>% left_join(us_pop, by = c('age' = 'age_group')) %>%
  left_join(pop_summary, by = c('age', 'year'))

# Add new estimates of opioid overdoses

temp_summary %<>% mutate(any_opioid_new = any_opioid + any_opioid_pred)

# Add total population data for each year, and follow with age-specific weights

temp_summary %<>% group_by(year) %>%
  mutate(population_total = sum(population_current)) %>%
  ungroup()

# Add group-specific  rates

temp_summary %<>% mutate_at(
  vars(any_opioid, any_opioid_new),
  funs(rate = (. / population_current) * 100000)
)

# Add age-specific weights and weighted rates by the CDC population indicators

temp_summary %<>% group_by(year) %>% 
  mutate(population_total_cdc = sum(population)) %>%
  ungroup() %>%
  mutate(
    weight = population / population_total_cdc
  ) %>%
  mutate_at(
    vars(any_opioid_rate, any_opioid_new_rate),
    funs(weighted = . * weight)
  )

# Generate opioid overdose rates across groups

temp_summary %>% group_by(year) %>%
  summarise_at(vars(contains('weighted')),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() -> total_rates

# Calculate age adjusted rates of opioid overdoses in the U.S. by county (Census) -------------------------------

# Note: Resulting file has all drug overdoses + predictions of opioid involvement in drug overdoses without drug information.

# Get standard population data.
# Note: See Distribution #1 from cdc.gov/nchs/data/statnt/statnt20.pdf. See Distribution #1 (but in 5-year increments).

us_pop <- read.table(url("https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt"),
                     colClasses = c('character'))

# Filter out 2000 standard population
# Note: Dictionary is available at https://seer.cancer.gov/stdpopulations/stdpopdic.html

us_pop %<>% filter(str_sub(us_pop$V1, end = 3) == 201)

# Add age group 

us_pop %<>% mutate(
  age_group = case_when(
    str_sub(V1, start = 4, end = 6) == '000' ~ '00 years',
    str_sub(V1, start = 4, end = 6) == '001' ~ '01-04 years',
    str_sub(V1, start = 4, end = 6) == '002' ~ '05-09 years',
    str_sub(V1, start = 4, end = 6) == '003' ~ '10-14 years',
    str_sub(V1, start = 4, end = 6) == '004' ~ '15-19 years',
    str_sub(V1, start = 4, end = 6) == '005' ~ '20-24 years',
    str_sub(V1, start = 4, end = 6) == '006' ~ '25-29 years',
    str_sub(V1, start = 4, end = 6) == '007' ~ '30-34 years',
    str_sub(V1, start = 4, end = 6) == '008' ~ '35-39 years',
    str_sub(V1, start = 4, end = 6) == '009' ~ '40-44 years',
    str_sub(V1, start = 4, end = 6) == '010' ~ '45-49 years',
    str_sub(V1, start = 4, end = 6) == '011' ~ '50-54 years',
    str_sub(V1, start = 4, end = 6) == '012' ~ '55-59 years',
    str_sub(V1, start = 4, end = 6) == '013' ~ '60-64 years',
    str_sub(V1, start = 4, end = 6) == '014' ~ '65-69 years',
    str_sub(V1, start = 4, end = 6) == '015' ~ '70-74 years',
    str_sub(V1, start = 4, end = 6) == '016' ~ '75-79 years',
    str_sub(V1, start = 4, end = 6) == '017' ~ '80-84 years',
    str_sub(V1, start = 4, end = 6) == '018' ~ '85+ years',
  )
)

# Download recent population data
# Note: From NBER website (easier to download from zip folder and csv)

temp <- tempfile()
download.file("https://data.nber.org/seer-pop/uswbo19agesadj.csv.zip",temp)
recent_pop <- read_csv(unz(temp, "uswbo19agesadj.csv"))
unlink(temp)

names(recent_pop) <-  c('year', 'state', 'state_fips', 'county_fips',
                        'registry', 'race', 'origin', 'sex', 'age', 
                        'population')

# Select variables

us_pop %<>% dplyr::select(age_group, population)

# Filter out only 2017 and 2018 from population data and drug overdose data

recent_pop %<>% filter(year >= 2017)
temp <- drug_overdoses %>% filter(year >= 2017)

# Add county fips code

recent_pop %<>% mutate(county_fips_code = paste0(state_fips, county_fips))

# Select relevant variables in drug overdoses database

temp %<>% select(year, contains('age'), county_fips_res,
                 unidentified_drug_only,
                 any_opioid, any_opioid_pred)

# Make population numeric

recent_pop %<>% mutate_at(vars(population),
                          funs(as.numeric(.)))

us_pop %<>% mutate_at(vars(population),
                      funs(as.numeric(.)))

# Fill opioid_pred NAs with zeros

temp %<>% mutate_at(vars(any_opioid_pred), funs(ifelse(is.na(.) == TRUE, 0, .)))

# Summarise drug overdoses by age and year and state FIPS code

temp_summary <- temp %>% group_by(year, ager52, county_fips_res) %>%
  summarise_at(vars(any_opioid, any_opioid_pred),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# Summarise US population by age group and year

pop_summary <- recent_pop %>% group_by(year, age, county_fips_code) %>%
  summarise_at(vars(population),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# Record drug overdoses age data to match populatin data from SEER

temp_summary %<>% mutate(
  age = case_when(
    ager52 <= 22 ~ '00 years',
    ager52 >= 23 & ager52 <= 26 ~ '01-04 years',
    ager52 == 27 ~ '05-09 years',
    ager52 == 28 ~ '10-14 years',
    ager52 == 29 ~ '15-19 years',
    ager52 == 30 ~ '20-24 years',
    ager52 == 31 ~ '25-29 years',
    ager52 == 32 ~ '30-34 years',
    ager52 == 33 ~ '35-39 years',
    ager52 == 34 ~ '40-44 years',
    ager52 == 35 ~ '45-49 years',
    ager52 == 36 ~ '50-54 years',
    ager52 == 37 ~ '55-59 years',
    ager52 == 38 ~ '60-64 years',
    ager52 == 39 ~ '65-69 years',
    ager52 == 40 ~ '70-74 years',
    ager52 == 41 ~ '75-79 years',
    ager52 == 42 ~ '80-84 years',
    ager52 >= 43 & ager52 <= 51 ~ '85+ years'
  )
)

pop_summary %<>% mutate(
  age = as.numeric(age),
  age = case_when(
    age == 0 ~ '00 years',
    age == 1 ~ '01-04 years',
    age == 2 ~ '05-09 years',
    age == 3 ~ '10-14 years',
    age == 4 ~ '15-19 years',
    age == 5 ~ '20-24 years',
    age == 6 ~ '25-29 years',
    age == 7 ~ '30-34 years',
    age == 8 ~ '35-39 years',
    age == 9 ~ '40-44 years',
    age == 10 ~ '45-49 years',
    age == 11 ~ '50-54 years',
    age == 12 ~ '55-59 years',
    age == 13 ~ '60-64 years',
    age == 14 ~ '65-69 years',
    age == 15 ~ '70-74 years',
    age == 16 ~ '75-79 years',
    age == 17 ~ '80-84 years',
    age == 18 ~ '85+ years'
  )
)

pop_summary %<>% group_by(age, year, county_fips_code) %>%
  summarise(population  = sum(population, na.rm = TRUE)) %>%
  ungroup()

# Resummarise using new definition 

temp_summary %<>% filter(!is.na(age)) %>%
  group_by(year, age, county_fips_res) %>% 
  summarise_at(vars(contains('opioid')),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

pop_summary %<>% select(everything(), population_current = population)

# Create complete index for US population data
# Note: Very important. Found out hard-way. Need to make sure that you have a complete index or, otherwise,
# you will basically place no weights on age groups without drug overdoses in smallish jurisdictions. 
n_distinct(temp_summary$county_fips_res)

counties <- temp_summary %>% dplyr::select(county_fips_res) %>% unique()

counties <- do.call('rbind', replicate(19, counties, simplify = FALSE))

counties %<>% group_by(county_fips_res) %>%
  mutate(age = row_number()) %>%
  ungroup()

us_pop %<>% mutate(index = row_number())

counties %<>% left_join(us_pop, by = c('age' = 'index'))

counties %<>% dplyr::select(-age)

# Expand by two (for year

counties <- do.call('rbind', replicate(2, counties, simplify = FALSE))

counties %<>% group_by(county_fips_res, age_group) %>%
  mutate(year = row_number() + 2016) %>%
  ungroup()

# Join new counties DF to data

counties %<>% left_join(temp_summary, by = c('county_fips_res', 'age_group' = 'age', 
                                             'year'))

counties %<>% left_join(pop_summary, by = c('age_group' = 'age', 'year', 'county_fips_res' = 'county_fips_code'))

# NA to zero

counties %<>% mutate_at(vars(any_opioid, any_opioid_pred),
                        funs(ifelse(is.na(.) == TRUE, 0, .)))

# Rearrange data

counties %<>% arrange(county_fips_res, year, age_group)

# Add new estimates of opioid overdoses

counties %<>% mutate(any_opioid_new = any_opioid + any_opioid_pred)

# Add total population data for each year, and follow with age-specific weights

counties %<>% group_by(year, county_fips_res) %>%
  mutate(population_total = sum(population_current)) %>%
  ungroup()

# Add group-specific  rates

counties %<>% mutate_at(
  vars(any_opioid, any_opioid_new),
  funs(rate = (. / population_current) * 100000)
)

# Add age-specific weights and weighted rates by the CDC population indicators

counties %<>% group_by(year, county_fips_res) %>% 
  mutate(population_total_cdc = sum(population)) %>%
  ungroup() %>%
  mutate(
    weight = population / population_total_cdc
  ) %>%
  mutate_at(
    vars(any_opioid_rate, any_opioid_new_rate),
    funs(weighted = . * weight)
  )

# Generate opioid overdose rates across groups

counties %>% group_by(year, county_fips_res) %>%
  summarise_at(vars(contains('weighted')),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() -> county_rates

# Join with data indicators

county_fips_codes <- county.fips

county_fips_codes %<>% mutate(fips = as.character(fips),
                              fips = ifelse(str_length(fips) == 4, paste0('0', fips), fips))

county_fips_codes %<>% mutate(state_name = str_extract(polyname, pattern = '^.*(?=(\\,))'))

county_fips_codes %<>% mutate(county_name = str_extract(polyname,
                                                        pattern = '(?<=(\\,)).*$'))

county_fips_codes %<>% mutate_at(vars(county_name, state_name),
                                 funs(str_to_title(.)))

county_fips_codes %<>% mutate(full_name = ifelse(state_name != 'Louisiana',
                                                 paste0(county_name, ' County, ', state_name),
                                                 paste0(county_name, ' Parish, ', state_name)))

# Merge with abbreviation/name of state

county_rates %<>% left_join(county_fips_codes, by = c('county_fips_res' = 'fips'))

# Reshape wide by year

county_rates %<>% setDT() %>%
  dcast(county_fips_res + polyname + state_name + county_name + full_name ~ year, 
        value.var = c('any_opioid_rate_weighted', 
                      'any_opioid_new_rate_weighted'))

pop_by_year <- pop_summary %>% group_by(year, county_fips_code) %>%
  summarise(total_population = sum(population_current)) %>%
  ungroup() %>%
  dcast(county_fips_code ~ year, value.var = ('total_population'))

names(pop_by_year) <- c('county_fips_code', 'pop_2017', 'pop_2018')

county_rates %<>% left_join(pop_by_year, by = c('county_fips_res' = 'county_fips_code'))

county_rates %>% saveRDS('Opioids_ML_2/Scratch/County_Opioid_Overdose_Rates_2017_2018.rds')

# Calculate # overdoses by bin in each year --------------------

drug_overdoses %>% filter(any_opioid == 1) %>% 
  group_by(year) %>%
  summarise(`Known Opioid Overdoses` = n()) %>%
  ungroup() -> op_ods

drug_overdoses %>% group_by(year) %>%
  summarise(`Total Drug Overdoses` = n()) %>%
  ungroup() -> all_ods

class(drug_overdoses$any_opioid_pred)

drug_overdoses %>% filter(unidentified_drug_only == 1) %>%
  group_by(year) %>%
  summarise(predicted_opioid_overdoses = sum(any_opioid_pred, na.rm = TRUE)) %>%
  ungroup() -> pred_op_ods

binned_ods <- op_ods %>% left_join(all_ods) %>% left_join(pred_op_ods)

# Calculate # of new opioid estimates

binned_ods %<>% mutate(`Corrected Opioid Overdoses` = `Known Opioid Overdoses` + predicted_opioid_overdoses)

binned_ods %<>% select(year, `Known Opioid Overdoses`, `Corrected Opioid Overdoses`,
                       `Total Drug Overdoses`) %>%
  gather(variable, value, -year)

binned_ods$variable <- factor(binned_ods$variable,
                              levels = c('Total Drug Overdoses', 'Corrected Opioid Overdoses',
                                         'Known Opioid Overdoses'))

# Generate figure of total overdoses over time -------------------
# Note: Include # of opioid (known and corrected)

group_plot <- ggplot(data = binned_ods,
                     aes(x = year, y = value, 
                         shape = variable, linetype = variable, 
                         colour = variable, group = variable)) + 
  geom_point() + geom_line() + 
  theme_classic() + labs(x = 'Year', y = '# of overdoses, by category') + 
  theme(legend.title = element_blank()) + 
  scale_colour_manual(values = c('#88CCEE', '#117733', '#661100')) + 
  theme(legend.position = c(0.8, 0.1)) + 
  theme(text = element_text(size = 12)) 

group_plot

ggsave('Opioids_ML_2/Figures/Figure_X_New_Estimates_of_Opioid_Overdoses_1999_2018.jpg')

# Save drug overdoses as intermediate file ---------------------------
# Note: Can save it to the scratch folder. 

drug_overdoses %>% saveRDS('Opioids_ML_2/Scratch/Drug_Overdoses_1999_2018_Intermediate.rds')

# Generate figure of differences in state opioid overdose rates, 2017 and 2018 ------------------

rm(list = setdiff(ls(), c('drug_overdoses')))

# Import population data

library(httr)

GET(url = 'https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx', 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
pop_data <- read_excel(tf, skip = 3)

# Clean data 

pop_data %<>% select(1, 4:ncol(pop_data))

pop_data %<>% gather(year, pop, -`...1`)

names(pop_data) <- c('state', 'year', 'pop')

pop_data$state %<>% str_replace_all(pattern = '^\\.', replacement = '') %>% tolower()

state_fips_codes <- state.fips

state_fips_codes %<>% mutate(polyname = ifelse(str_detect(string = polyname, pattern = '\\:') == TRUE,
                                               str_extract_all(string = polyname, pattern = '^.*(?=(\\:))'), 
                                               polyname))

state_fips_codes %<>% select(polyname, abb, fips) %>% unique()

temp <- data.frame(name = c('hawaii', 'alaska'),
                   abb = c('HI', 'AK'),
                   fips = c('15', '02'))

names(temp) <- c('polyname', 'abb', 'fips')

temp %<>% mutate_all(funs(as.character(.)))

state_fips_codes %<>% mutate_all(funs(as.character(.)))

state_fips_codes %<>% bind_rows(temp)

pop_data %>% filter(!state %in% state_fips_codes$polyname) %>% select(state) %>% unique() %>% print()

pop_data %<>% filter(state %in% state_fips_codes$polyname)

pop_data %<>% left_join(state_fips_codes, by = c('state' = 'polyname'))

pop_data %<>% mutate(fips = as.character(fips),
                     fips = ifelse(str_length(fips) == 1, paste0('0', fips), fips))

# Set loop

summaries <- data.frame()

for(fff in 2017:2018) {
  
  temp <- drug_overdoses %>% filter(year == fff)
  
  # Calculate summaries of opioid numbers by state
  
  temp %<>% group_by(state_fips_res) %>%
    summarise(total_drug_overdoses = n(),
              known_opioid_overdoses = sum(any_opioid, na.rm = TRUE),
              predicted_opioid_overdoses = sum(any_opioid_pred, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(corrected_opioid_overdoses = known_opioid_overdoses + predicted_opioid_overdoses,
           year = as.character(fff))
  
  # Connect with state population-levels
  
  temp %<>% left_join(pop_data, by = c('year', 'state_fips_res' = 'fips'))
  
  # Clean
  
  temp %<>% mutate(state = str_to_title(state))
  
  # Compute differences in opioid overdose rates
  
  temp %<>% mutate(known_opioid_overdose_rate = (known_opioid_overdoses/pop) * 100000,
                   corrected_opioid_overdose_rate = (corrected_opioid_overdoses/pop) * 100000,
                   total_drug_overdose_rate = (total_drug_overdoses/pop) * 100000)
  
  # Bind to data frame
  
  summaries %<>% bind_rows(temp)
  
  # Generate plot
  
  temp %<>% arrange(corrected_opioid_overdose_rate) %>%
    mutate(state = factor(state, unique(state)))
  
  temp_plot <- ggplot(data = temp,
                      aes(x = known_opioid_overdose_rate, xend = corrected_opioid_overdose_rate,
                          y = state, group = state)) + 
    geom_dumbbell(colour_x = 'tomato1', size = 1, 
                  color = 'grey70', colour_xend = 'tomato4') + 
    labs(x = paste0('Opioid overdose rate (', 
                    as.character(fff), ')'), y = 'State') + 
    theme_classic() + 
    theme(axis.title = element_text(size = 12))
  
  temp_plot
  
  # Save plot
  
  ggsave(paste0('Opioids_ML_2/Figures/Figure_X_Difference_in_Opioid_Overdose_Rates_by_State_', as.character(fff), '.jpg'))
  
  # Remove files
  
  print(fff)
  
  rm(temp, fff, temp_plot)
  
}

# Generate figure of differences in state opioid overdose rates, 2017 and 2018 (age-adjusted) ----------------------

# Import file

state_rates <- readRDS('Opioids_ML_2/Scratch/State_Opioid_Overdose_Rates_2017_2018.rds')

state_rates %<>% arrange(any_opioid_new_rate_weighted_2018) %>%
  mutate(state = factor(polyname, unique(polyname)))

temp_plot <- ggplot(data = state_rates,
                    aes(x = any_opioid_rate_weighted_2018, 
                        xend = any_opioid_new_rate_weighted_2018,
                        y = state, group = state)) + 
  geom_dumbbell(colour_x = 'tomato1', size = 1, 
                color = 'grey70', colour_xend = 'tomato4') + 
  labs(x = paste0('Age-adjusted opioid overdose rate (', 
                  as.character(2018), ')'), y = 'State') + 
  theme_classic() +
  theme(axis.title = element_text(size = 12))

temp_plot

# Save plot

ggsave(paste0('Opioids_ML_2/Figures/Figure_X_Difference_in_Opioid_Overdose_Rates_by_State_', as.character(2018), '.jpg'))

# 2017

state_rates %<>% arrange(any_opioid_new_rate_weighted_2017) %>%
  mutate(state = factor(polyname, unique(polyname)))

temp_plot <- ggplot(data = state_rates,
                    aes(x = any_opioid_rate_weighted_2017, 
                        xend = any_opioid_new_rate_weighted_2017,
                        y = state, group = state)) + 
  geom_dumbbell(colour_x = 'tomato1', size = 1, 
                color = 'grey70', colour_xend = 'tomato4') + 
  labs(x = paste0('Age-adjusted opioid overdose rate (', 
                  as.character(2017), ')'), y = 'State') + 
  theme_classic() +
  theme(axis.title = element_text(size = 12))

temp_plot

# Save plot

ggsave(paste0('Opioids_ML_2/Figures/Figure_X_Difference_in_Opioid_Overdose_Rates_by_State_', as.character(2017), '.jpg'))

# Generate figure of differences in state opioid overdoses between 2017 and 2018, age-adjusted --------------------

state_rates %<>% mutate(`Difference in opioid overdose rates, 2018 vs. 2017` = 
                          (any_opioid_new_rate_weighted_2018 - any_opioid_new_rate_weighted_2017))

state_rates %<>% arrange(`Difference in opioid overdose rates, 2018 vs. 2017`) %>%
  mutate(state = factor(polyname, unique(polyname)))

dot_plot_diff <- ggplot(data = state_rates,
                        aes(x = state, y = `Difference in opioid overdose rates, 2018 vs. 2017`)) + 
  geom_hline(yintercept = 0, color = 'grey80') + 
  geom_segment(aes(x = state, 
                   xend = state, 
                   y = min(`Difference in opioid overdose rates, 2018 vs. 2017`),
                   yend = max(`Difference in opioid overdose rates, 2018 vs. 2017`)),
               linetype = 'dashed', size = 0.1, color = 'grey80') + 
  geom_point(color = 'dodgerblue', size = 3) + 
  labs(x = 'State', y = 'Difference in age-adjusted opioid overdose rates, 2018 vs. 2017') + 
  coord_flip() + theme_classic() + 
  theme(axis.title = element_text(size = 12))

dot_plot_diff

ggsave('Opioids_ML_2/Figures/Figure_X_Opioid_Overdose_Rate_by_State_2017_vs_2018_AA.jpg')

# Generate figure of differences in state opioid overdoses using corrected versions ------------

summaries %<>% select(year, state, corrected_opioid_overdose_rate) %>%
  dcast(state ~ year, value.var = c('corrected_opioid_overdose_rate'))

summaries %<>% mutate(`Difference in opioid overdose rates, 2018 vs. 2017` = 
                        (`2018` - `2017`))

summaries %<>% arrange(`Difference in opioid overdose rates, 2018 vs. 2017`) %>%
  mutate(state = factor(state, unique(state)))

dot_plot_diff <- ggplot(data = summaries,
                        aes(x = state, y = `Difference in opioid overdose rates, 2018 vs. 2017`)) + 
  geom_hline(yintercept = 0, color = 'grey80') + 
  geom_segment(aes(x = state, 
                   xend = state, 
                   y = min(`Difference in opioid overdose rates, 2018 vs. 2017`),
                   yend = max(`Difference in opioid overdose rates, 2018 vs. 2017`)),
               linetype = 'dashed', size = 0.1, color = 'grey80') + 
  geom_point(color = 'dodgerblue', size = 3) + 
  labs(x = 'State', y = 'Difference in opioid overdose rates, 2018 vs. 2017') + 
  coord_flip() + theme_classic() + 
  theme(axis.title = element_text(size = 12))

dot_plot_diff

ggsave('Opioids_ML_2/Figures/Figure_X_Opioid_Overdose_Rate_by_State_2017_vs_2018.jpg')

# Generate figure of differences in county opioid overdose rates between 2017-2018, age-adjusted ------------------

county_rates <- readRDS('Opioids_ML_2/Scratch/County_Opioid_Overdose_Rates_2017_2018.rds')

# Filter greater than 250,000 mean population

county_rates %<>% mutate(mean_pop = (pop_2017 + pop_2018) / 2)

county_rates %<>% filter(mean_pop >= 500000)

county_rates %<>% mutate(`Difference in opioid overdose rates, 2018 vs. 2017` = 
                           any_opioid_new_rate_weighted_2018 - any_opioid_new_rate_weighted_2017)

county_rates %<>% arrange(`Difference in opioid overdose rates, 2018 vs. 2017`) %>%
  mutate(full_name = factor(full_name, unique(full_name)))

data_for_plot <- county_rates %>% filter(row_number() <= 20 | 
                                           row_number() >= nrow(county_rates) - 19)


dot_plot_diff <- ggplot(data = data_for_plot,
                        aes(x = full_name, 
                            y = `Difference in opioid overdose rates, 2018 vs. 2017`)) + 
  geom_hline(yintercept = 0, color = 'grey80') + 
  geom_segment(aes(x = full_name, 
                   xend = full_name, 
                   y = min(`Difference in opioid overdose rates, 2018 vs. 2017`),
                   yend = max(`Difference in opioid overdose rates, 2018 vs. 2017`)),
               linetype = 'dashed', size = 0.1, color = 'grey80') + 
  geom_point(color = 'tomato2', size = 3) + 
  labs(x = ' ', y = 'Diff. in age-adjusted opioid overdose rates, 2018 vs. 2017') + 
  coord_flip() + theme_classic() + 
  theme(axis.title = element_text(size = 12))

dot_plot_diff

ggsave('Opioids_ML_2/Figures/Figure_X_Difference_in_County_Rates.jpg')

# Generate table of top counties with largest increases in opioid overdose rates from corrected ----------------

temp <- drug_overdoses %>% filter(year >= 2017)

rm(list = setdiff(ls(), c('temp')))

# Import county population data

population <- read_csv(url('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv'))

population %<>% dplyr::select(STATE, COUNTY, STNAME, CTYNAME, 
                              starts_with('POPESTIMATE'))

population %<>% mutate(county_fips_code = paste0(STATE, COUNTY),
                       county_name = paste0(CTYNAME, ', ', STNAME)) %>%
  dplyr::select(-STATE, -COUNTY, -CTYNAME, -STNAME) %>%
  gather(variable, value, -county_fips_code, -county_name) %>%
  mutate(variable = str_extract(variable, pattern = '201[0-9]'),
         variable = as.numeric(variable))

# Calculate # of counties with opioid overdoses and corrected opioid overdoses

temp_summary <- temp %>% group_by(county_fips_res, year) %>%
  summarise_at(vars(any_opioid, any_opioid_pred),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

temp_summary %<>% left_join(population, by = c('county_fips_res' = 'county_fips_code',
                                               'year' = 'variable'))

# Calculate rates per 100,000 people

temp_summary %<>% mutate_at(
  vars(any_opioid, any_opioid_pred),
  funs(ifelse(is.na(.) == TRUE, 0, .))
) %>% 
  mutate(any_opioid_new = any_opioid + any_opioid_pred) %>%
  mutate_at(vars(any_opioid, any_opioid_new),
            funs(rate = (. / value) * 100000))

temp_summary %>% dplyr::select(county_fips_res, county_name, year, 
                               value, any_opioid_new_rate, any_opioid, any_opioid_pred) %>%
  setDT() %>%
  dcast(county_fips_res + county_name ~ year, value.var = c('any_opioid_new_rate', 
                                                            'value',
                                                            'any_opioid', 
                                                            'any_opioid_pred')) -> summaries

summaries %<>% mutate_at(vars(`any_opioid_new_rate_2017`, `any_opioid_new_rate_2018`),
                         funs(ifelse(is.na(.) == TRUE, 0, .)))

summaries %<>% mutate(`Difference in opioid overdose rates, 2018 vs. 2017` = 
                        (`any_opioid_new_rate_2018` - `any_opioid_new_rate_2017`))

# Counties with more than 500,000 people

summaries_500000 <- summaries %>% mutate(mean_pop = (value_2017 + value_2018) / 2) %>%
  filter(mean_pop >= 500000)

summaries_500000 %<>% arrange(`Difference in opioid overdose rates, 2018 vs. 2017`) %>%
  mutate(county_name = factor(county_name, unique(county_name)))

data_for_plot <- summaries_500000 %>% filter(row_number() <= 20 | 
                                               row_number() >= nrow(summaries_500000) - 19)

data_for_plot$county_name %<>% str_to_title()

data_for_plot %<>% arrange(`Difference in opioid overdose rates, 2018 vs. 2017`) %>%
  mutate(county_name = factor(county_name, unique(county_name)))

dot_plot_diff <- ggplot(data = data_for_plot,
                        aes(x = county_name, 
                            y = `Difference in opioid overdose rates, 2018 vs. 2017`)) + 
  geom_hline(yintercept = 0, color = 'grey80') + 
  geom_segment(aes(x = county_name, 
                   xend = county_name, 
                   y = min(`Difference in opioid overdose rates, 2018 vs. 2017`),
                   yend = max(`Difference in opioid overdose rates, 2018 vs. 2017`)),
               linetype = 'dashed', size = 0.1, color = 'grey80') + 
  geom_point(color = 'tomato2', size = 3) + 
  labs(x = ' ', y = 'Diff. in opioid overdose rates, 2018 vs. 2017') + 
  coord_flip() + theme_classic() + 
  theme(axis.title = element_text(size = 12))

dot_plot_diff

ggsave('Opioids_ML_2/Figures/Figure_X_Opioid_Overdose_Rate_by_County_2017_vs_2018.jpg')

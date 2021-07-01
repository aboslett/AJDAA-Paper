# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Estimate receiver operating curves for the various logistic regression models of interest. In this script, we
# use balanced databases between opioid and non-opioid overdoses. Can be adapted for unbalanced, as well. 

logistic_accuracy <- data.frame()

for(fff in 2017:2018) {
  
  # Import data --------------------
  
  overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # (2) Logistic regression (Top-100) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:203)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %>% filter(any_opioid == 0) %>% 
    nrow() * 0.80 -> number_of_non_opioid_overdoses
  
  number_of_non_opioid_overdoses %<>% round(., digits = 0)
  
  # Generate index for train vs test definition
  
  temp %<>% mutate(id_overdose = row_number())
  
  # Grab a random samples of non-opioid & opioid overdoses, where n = 80% x # of non-opioid ODs
  
  temp %>% filter(any_opioid == 0) %>% sample_n(number_of_non_opioid_overdoses) -> temp_non_opioids
  temp %>% filter(any_opioid == 1) %>% sample_n(number_of_non_opioid_overdoses) -> temp_opioids
  
  # Bind rows together
  
  temp_analysis <- temp_non_opioids %>% bind_rows(temp_opioids)
  
  # Define test set
  
  temp_test <- anti_join(temp, temp_analysis, by = c('id_overdose'))
  
  # Balance test set
  
  temp_test %>% filter(any_opioid == 0) -> test_non_opioids
  nrow(test_non_opioids) -> test_number_of_non_opioids
  test_number_of_non_opioids %<>% round(., digits = 0)
  
  temp_test %>% filter(any_opioid == 1) %>% sample_n(test_number_of_non_opioids) -> test_opioids
  
  temp_test <- test_opioids %>% bind_rows(test_non_opioids)
  
  # Drop id_overdose variable from train set
  
  temp_analysis %<>% select(-id_overdose)
  temp_test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  temp_analysis %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  temp_test %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  
  logistic_reg_roc <- data.frame()
  
  perc_units <- seq(from = 0.05, to = 0.95, by = 0.05)
  
  for(ggg in perc_units) {
    
    glm.pred = rep(0, nrow(temp_test))
    glm.pred[glm.probs > ggg] = 1
    
    table(glm.pred, temp_test$any_opioid) %>% as.data.frame() %>%
      mutate(
        type = case_when(
          glm.pred == 0 & Var2 == 0 ~ "TN", 
          glm.pred == 0 & Var2 == 1 ~ "FN", 
          glm.pred == 1 & Var2 == 1 ~ "TP", 
          glm.pred == 1 & Var2 == 0 ~ "FP")
      ) %>%
      select(Freq, type) %>%
      spread(type, Freq) -> glm_cm
    
    
    # Add variables if they don't exist
    
    if (!'TP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'TN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TN = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FN = 0)
    } else {
      print("Already in there")
    }
    
    # Generate accuracy statistics
    
    glm_cm %>% mutate(
      approach = "Top-100 CC",
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      one_minus_specificity = 1 - specificity,
      f_measure = (2 * (precision * recall)) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy,
      threshold = ggg,
      year = fff
    ) %>%
      select(approach:year) -> glm_accuracy
    
    logistic_reg_roc %<>% bind_rows(glm_accuracy)
    
  }
  
  assign(x = paste0('logistic_100_', as.character(fff)), value = logistic_reg_roc)
  
  
  # (3) Logistic regression (Top-50) --------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,weekend,
           104:153)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %>% filter(any_opioid == 0) %>% 
    nrow() * 0.80 -> number_of_non_opioid_overdoses
  
  number_of_non_opioid_overdoses %<>% round(., digits = 0)
  
  # Generate index for train vs test definition
  
  temp %<>% mutate(id_overdose = row_number())
  
  # Grab a random samples of non-opioid & opioid overdoses, where n = 80% x # of non-opioid ODs
  
  temp %>% filter(any_opioid == 0) %>% sample_n(number_of_non_opioid_overdoses) -> temp_non_opioids
  temp %>% filter(any_opioid == 1) %>% sample_n(number_of_non_opioid_overdoses) -> temp_opioids
  
  # Bind rows together
  
  temp_analysis <- temp_non_opioids %>% bind_rows(temp_opioids)
  
  # Define test set
  
  temp_test <- anti_join(temp, temp_analysis, by = c('id_overdose'))
  
  # Balance test set
  
  temp_test %>% filter(any_opioid == 0) -> test_non_opioids
  nrow(test_non_opioids) -> test_number_of_non_opioids
  test_number_of_non_opioids %<>% round(., digits = 0)
  
  temp_test %>% filter(any_opioid == 1) %>% sample_n(test_number_of_non_opioids) -> test_opioids
  
  temp_test <- test_opioids %>% bind_rows(test_non_opioids)
  
  # Drop id_overdose variable from train set
  
  temp_analysis %<>% select(-id_overdose)
  temp_test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  temp_analysis %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  temp_test %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  logistic_reg_roc <- data.frame()
  
  perc_units <- seq(from = 0.05, to = 0.95, by = 0.05)
  
  for(ggg in perc_units) {
    
    glm.pred = rep(0, nrow(temp_test))
    glm.pred[glm.probs > ggg] = 1
    
    table(glm.pred, temp_test$any_opioid) %>% as.data.frame() %>%
      mutate(
        type = case_when(
          glm.pred == 0 & Var2 == 0 ~ "TN", 
          glm.pred == 0 & Var2 == 1 ~ "FN", 
          glm.pred == 1 & Var2 == 1 ~ "TP", 
          glm.pred == 1 & Var2 == 0 ~ "FP")
      ) %>%
      select(Freq, type) %>%
      spread(type, Freq) -> glm_cm
    
    
    # Add variables if they don't exist
    
    if (!'TP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'TN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TN = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FN = 0)
    } else {
      print("Already in there")
    }
    
    # Generate accuracy statistics
    
    glm_cm %>% mutate(
      approach = "Top-50 CC",
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      one_minus_specificity = 1 - specificity,
      f_measure = (2 * (precision * recall)) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy,
      threshold = ggg,
      year = fff
    ) %>%
      select(approach:year) -> glm_accuracy
    
    logistic_reg_roc %<>% bind_rows(glm_accuracy)
    
  }
  
  assign(x = paste0('logistic_50_', as.character(fff)), value = logistic_reg_roc)
  
  # Timestamp and remove objects
  
  
  
  rm(logistic_regression_model)
  
  
  
  # (4) Logistic regression (Top-20) --------------------
  
  
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:133)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %>% filter(any_opioid == 0) %>% 
    nrow() * 0.80 -> number_of_non_opioid_overdoses
  
  number_of_non_opioid_overdoses %<>% round(., digits = 0)
  
  # Generate index for train vs test definition
  
  temp %<>% mutate(id_overdose = row_number())
  
  # Grab a random samples of non-opioid & opioid overdoses, where n = 80% x # of non-opioid ODs
  
  temp %>% filter(any_opioid == 0) %>% sample_n(number_of_non_opioid_overdoses) -> temp_non_opioids
  temp %>% filter(any_opioid == 1) %>% sample_n(number_of_non_opioid_overdoses) -> temp_opioids
  
  # Bind rows together
  
  temp_analysis <- temp_non_opioids %>% bind_rows(temp_opioids)
  
  # Define test set
  
  temp_test <- anti_join(temp, temp_analysis, by = c('id_overdose'))
  
  # Balance test set
  
  temp_test %>% filter(any_opioid == 0) -> test_non_opioids
  nrow(test_non_opioids) -> test_number_of_non_opioids
  test_number_of_non_opioids %<>% round(., digits = 0)
  
  temp_test %>% filter(any_opioid == 1) %>% sample_n(test_number_of_non_opioids) -> test_opioids
  
  temp_test <- test_opioids %>% bind_rows(test_non_opioids)
  
  # Drop id_overdose variable from train set
  
  temp_analysis %<>% select(-id_overdose)
  temp_test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  temp_analysis %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  temp_test %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  logistic_reg_roc <- data.frame()
  
  perc_units <- seq(from = 0.05, to = 0.95, by = 0.05)
  
  for(ggg in perc_units) {
    
    glm.pred = rep(0, nrow(temp_test))
    glm.pred[glm.probs > ggg] = 1
    
    table(glm.pred, temp_test$any_opioid) %>% as.data.frame() %>%
      mutate(
        type = case_when(
          glm.pred == 0 & Var2 == 0 ~ "TN", 
          glm.pred == 0 & Var2 == 1 ~ "FN", 
          glm.pred == 1 & Var2 == 1 ~ "TP", 
          glm.pred == 1 & Var2 == 0 ~ "FP")
      ) %>%
      select(Freq, type) %>%
      spread(type, Freq) -> glm_cm
    
    
    # Add variables if they don't exist
    
    if (!'TP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'TN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TN = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FN = 0)
    } else {
      print("Already in there")
    }
    
    # Generate accuracy statistics
    
    glm_cm %>% mutate(
      approach = "Top-20 CC",
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      one_minus_specificity = 1 - specificity,
      f_measure = (2 * (precision * recall)) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy,
      threshold = ggg,
      year = fff
    ) %>%
      select(approach:year) -> glm_accuracy
    
    logistic_reg_roc %<>% bind_rows(glm_accuracy)
    
  }
  
  assign(x = paste0('logistic_20_', as.character(fff)), value = logistic_reg_roc)
  
  
  
  
  
  # (5) Logistic regression (Top-10) --------------------
  
  
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:113)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %>% filter(any_opioid == 0) %>% 
    nrow() * 0.80 -> number_of_non_opioid_overdoses
  
  number_of_non_opioid_overdoses %<>% round(., digits = 0)
  
  # Generate index for train vs test definition
  
  temp %<>% mutate(id_overdose = row_number())
  
  # Grab a random samples of non-opioid & opioid overdoses, where n = 80% x # of non-opioid ODs
  
  temp %>% filter(any_opioid == 0) %>% sample_n(number_of_non_opioid_overdoses) -> temp_non_opioids
  temp %>% filter(any_opioid == 1) %>% sample_n(number_of_non_opioid_overdoses) -> temp_opioids
  
  # Bind rows together
  
  temp_analysis <- temp_non_opioids %>% bind_rows(temp_opioids)
  
  # Define test set
  
  temp_test <- anti_join(temp, temp_analysis, by = c('id_overdose'))
  
  # Balance test set
  
  temp_test %>% filter(any_opioid == 0) -> test_non_opioids
  nrow(test_non_opioids) -> test_number_of_non_opioids
  test_number_of_non_opioids %<>% round(., digits = 0)
  
  temp_test %>% filter(any_opioid == 1) %>% sample_n(test_number_of_non_opioids) -> test_opioids
  
  temp_test <- test_opioids %>% bind_rows(test_non_opioids)
  
  # Drop id_overdose variable from train set
  
  temp_analysis %<>% select(-id_overdose)
  temp_test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  temp_analysis %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  temp_test %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  logistic_reg_roc <- data.frame()
  
  perc_units <- seq(from = 0.05, to = 0.95, by = 0.05)
  
  for(ggg in perc_units) {
    
    glm.pred = rep(0, nrow(temp_test))
    glm.pred[glm.probs > ggg] = 1
    
    table(glm.pred, temp_test$any_opioid) %>% as.data.frame() %>%
      mutate(
        type = case_when(
          glm.pred == 0 & Var2 == 0 ~ "TN", 
          glm.pred == 0 & Var2 == 1 ~ "FN", 
          glm.pred == 1 & Var2 == 1 ~ "TP", 
          glm.pred == 1 & Var2 == 0 ~ "FP")
      ) %>%
      select(Freq, type) %>%
      spread(type, Freq) -> glm_cm
    
    
    # Add variables if they don't exist
    
    if (!'TP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'TN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TN = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FN = 0)
    } else {
      print("Already in there")
    }
    
    # Generate accuracy statistics
    
    glm_cm %>% mutate(
      approach = "Top-10 CC",
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      one_minus_specificity = 1 - specificity,
      f_measure = (2 * (precision * recall)) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy,
      threshold = ggg,
      year = fff
    ) %>%
      select(approach:year) -> glm_accuracy
    
    logistic_reg_roc %<>% bind_rows(glm_accuracy)
    
  }
  
  assign(x = paste0('logistic_10_', as.character(fff)), value = logistic_reg_roc)
  
  
  
  # (6) Logistic regression (Only decedent characteristics) -------------------
  
  
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,
           weekend)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %>% filter(any_opioid == 0) %>% 
    nrow() * 0.80 -> number_of_non_opioid_overdoses
  
  number_of_non_opioid_overdoses %<>% round(., digits = 0)
  
  # Generate index for train vs test definition
  
  temp %<>% mutate(id_overdose = row_number())
  
  # Grab a random samples of non-opioid & opioid overdoses, where n = 80% x # of non-opioid ODs
  
  temp %>% filter(any_opioid == 0) %>% sample_n(number_of_non_opioid_overdoses) -> temp_non_opioids
  temp %>% filter(any_opioid == 1) %>% sample_n(number_of_non_opioid_overdoses) -> temp_opioids
  
  # Bind rows together
  
  temp_analysis <- temp_non_opioids %>% bind_rows(temp_opioids)
  
  # Define test set
  
  temp_test <- anti_join(temp, temp_analysis, by = c('id_overdose'))
  
  # Balance test set
  
  temp_test %>% filter(any_opioid == 0) -> test_non_opioids
  nrow(test_non_opioids) -> test_number_of_non_opioids
  test_number_of_non_opioids %<>% round(., digits = 0)
  
  temp_test %>% filter(any_opioid == 1) %>% sample_n(test_number_of_non_opioids) -> test_opioids
  
  temp_test <- test_opioids %>% bind_rows(test_non_opioids)
  
  # Drop id_overdose variable from train set
  
  temp_analysis %<>% select(-id_overdose)
  temp_test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  temp_analysis %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  temp_test %>% select(female:ncol(temp_analysis)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      temp_analysis %<>% select(-contains(xxx))
      temp_test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  logistic_reg_roc <- data.frame()
  
  perc_units <- seq(from = 0.05, to = 0.95, by = 0.05)
  
  for(ggg in perc_units) {
    
    glm.pred = rep(0, nrow(temp_test))
    glm.pred[glm.probs > ggg] = 1
    
    table(glm.pred, temp_test$any_opioid) %>% as.data.frame() %>%
      mutate(
        type = case_when(
          glm.pred == 0 & Var2 == 0 ~ "TN", 
          glm.pred == 0 & Var2 == 1 ~ "FN", 
          glm.pred == 1 & Var2 == 1 ~ "TP", 
          glm.pred == 1 & Var2 == 0 ~ "FP")
      ) %>%
      select(Freq, type) %>%
      spread(type, Freq) -> glm_cm
    
    
    # Add variables if they don't exist
    
    if (!'TP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'TN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(TN = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FP' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FP = 0)
    } else {
      print("Already in there")
    }
    
    if (!'FN' %in% names(glm_cm)) {
      glm_cm %<>% mutate(FN = 0)
    } else {
      print("Already in there")
    }
    
    # Generate accuracy statistics
    
    glm_cm %>% mutate(
      approach = "Decedent Characteristics",
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      one_minus_specificity = 1 - specificity,
      f_measure = (2 * (precision * recall)) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy,
      threshold = ggg,
      year = fff
    ) %>%
      select(approach:year) -> glm_accuracy
    
    logistic_reg_roc %<>% bind_rows(glm_accuracy)
    
  }
  
  assign(x = paste0('logistic_dec_', as.character(fff)), value = logistic_reg_roc)
  
    # Timestamp -------------------------
  
  print(fff)
  
}

# Bind all data frames together ---------------------

local_dfs <- c('logistic_10_2017', 'logistic_10_2018', 
               'logistic_100_2017', 'logistic_100_2018',
               'logistic_50_2017', 'logistic_50_2018', 
               'logistic_20_2017', 'logistic_20_2018', 
               'logistic_dec_2017', 'logistic_dec_2018')

roc_data <- data.frame()

for(fff in local_dfs) {
  
  temp <- get(fff)
  
  roc_data %<>% bind_rows(temp)
  
  rm(temp, fff)
  
}

rm(local_dfs)

roc_data %>% saveRDS('Opioids_ML_2/Scratch/Table_X_ROC_Balanced.rds')

# Create figure (ROC) ---------------------

for(fff in 2017:2018) {
  
  temp <- roc_data %>% filter(year == fff)
  
  # Set factor levels
  
  temp$approach <- factor(temp$approach, 
                          levels = c('Top-100 CC', 'Top-50 CC', 'Top-20 CC', 
                                     'Top-10 CC', 'Decedent Characteristics'))
  
  # Create plot
  
  temp_plot <- ggplot(data = temp, 
                      aes(x = one_minus_specificity, y = recall,
                          colour = approach)) + 
    theme_classic() + 
    labs(x = 'False-positive rate (1 - Specificity)', 
         y = 'True positive rate (Recall)') + 
    geom_smooth(se = FALSE) + 
    theme(legend.title = element_blank()) + 
    theme(legend.position = c(0.8, 0.3)) + 
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))
  
  # See plot
  
  temp_plot
  
  # Save plot
  
  ggsave(paste0('Opioids_ML_2/Figures/Figure_X_ROC_Curve_Balanced_', as.character(fff), '.jpg'))
  
  # Remove files
  
  rm(fff, temp_plot, temp)
  
}
# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Evaluate accuracy of alternative models with unbalanced opioid or non-opioid classes. The main model alternatives
# include different levels of contributing causes, from the top-150 occurring in a given year within drug overdoses
# to 0. The other main alternative is a shift from logistic regression to random forests. In this code, we have
# dropped all other modeling approaches used in behind-the-scenes evaluation. Used total accuracy and MCC.

logistic_accuracy <- data.frame()

for(fff in 1999:2019) {
  
  # Import data --------------------
  
  overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # Estimate models ----------------------------
  
  temp <- overdose_deaths %>% filter(year == fff & unidentified_drug_only == 0) %>%
    select(any_opioid)
  
  temp$any_opioid %<>% as.numeric()
  
  temp$any_opioid <- temp$any_opioid - 1
  
  temp %<>% summarise(accuracy = mean(any_opioid)) %>%
    mutate(year = fff,
           approach = 'Naive accuracy')
  
  logistic_accuracy %<>% bind_rows(temp)
  
  # (2) Logistic regression (Top-100) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:203)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(test)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = train)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(test))
  glm.pred[glm.probs > .5] = 1
  
  table(glm.pred, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        glm.pred == 0 & Var2 == 0 ~ "TN", 
        glm.pred == 0 & Var2 == 1 ~ "FN", 
        glm.pred == 1 & Var2 == 1 ~ "TP", 
        glm.pred == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_cm
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>% mutate(
                         approach = "Logistic Regression (Top-100)",
                         recall = TP / (TP + FN),
                         precision = TP / (TP + FP),
                         specificity = TN / (TN + FP),
                         f_measure = (precision * recall) / (precision + recall),
                         fallout = FP / (FP + TN),
                         accuracy = (TP + TN) / (TP + FP + FN + TN),
                         total_error_rate = 1 - accuracy,
                         mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                         year = fff
                       ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # (3) Logistic regression (Top-50) --------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,weekend,
           104:153)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = train)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(test))
  glm.pred[glm.probs > .5] = 1
  
  table(glm.pred, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        glm.pred == 0 & Var2 == 0 ~ "TN", 
        glm.pred == 0 & Var2 == 1 ~ "FN", 
        glm.pred == 1 & Var2 == 1 ~ "TP", 
        glm.pred == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_cm
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>% mutate(
                         approach = "Logistic Regression (Top-50 Contributing Causes)",
                         recall = TP / (TP + FN),
                         precision = TP / (TP + FP),
                         specificity = TN / (TN + FP),
                         f_measure = (precision * recall) / (precision + recall),
                         fallout = FP / (FP + TN),
                         accuracy = (TP + TN) / (TP + FP + FN + TN),
                         total_error_rate = 1 - accuracy,
                         mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                         year = fff
                       ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
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
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = train)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(test))
  glm.pred[glm.probs > .5] = 1
  
  table(glm.pred, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        glm.pred == 0 & Var2 == 0 ~ "TN", 
        glm.pred == 0 & Var2 == 1 ~ "FN", 
        glm.pred == 1 & Var2 == 1 ~ "TP", 
        glm.pred == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_cm
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>% mutate(
                         approach = "Logistic Regression (Top-20 Contributing Causes)",
                         recall = TP / (TP + FN),
                         precision = TP / (TP + FP),
                         specificity = TN / (TN + FP),
                         f_measure = (precision * recall) / (precision + recall),
                         fallout = FP / (FP + TN),
                         accuracy = (TP + TN) / (TP + FP + FN + TN),
                         total_error_rate = 1 - accuracy,
                         mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                         year = fff
                       ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # Timestamp and remove objects
  
  
  
  rm(logistic_regression_model)
  
  
  
  
  
  # (5) Logistic regression (Top-10) --------------------
  
  
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:113)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = train)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(test))
  glm.pred[glm.probs > .5] = 1
  
  table(glm.pred, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        glm.pred == 0 & Var2 == 0 ~ "TN", 
        glm.pred == 0 & Var2 == 1 ~ "FN", 
        glm.pred == 1 & Var2 == 1 ~ "TP", 
        glm.pred == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_cm
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>% mutate(
                         approach = "Logistic Regression (Top-10 Contributing Causes)",
                         recall = TP / (TP + FN),
                         precision = TP / (TP + FP),
                         specificity = TN / (TN + FP),
                         f_measure = (precision * recall) / (precision + recall),
                         fallout = FP / (FP + TN),
                         accuracy = (TP + TN) / (TP + FP + FN + TN),
                         total_error_rate = 1 - accuracy,
                         mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                         year = fff
                       ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # Timestamp and remove objects
  
  
  
  rm(logistic_regression_model)
  
  
  
  # (6) Logistic regression (Only decedent characteristics) -------------------
  
  
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,
           weekend)
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = train)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(test))
  glm.pred[glm.probs > .5] = 1
  
  table(glm.pred, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        glm.pred == 0 & Var2 == 0 ~ "TN", 
        glm.pred == 0 & Var2 == 1 ~ "FN", 
        glm.pred == 1 & Var2 == 1 ~ "TP", 
        glm.pred == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_cm
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>% mutate(
                         approach = "Logistic Regression (Decedent Characteristics)",
                         recall = TP / (TP + FN),
                         precision = TP / (TP + FP),
                         specificity = TN / (TN + FP),
                         f_measure = (precision * recall) / (precision + recall),
                         fallout = FP / (FP + TN),
                         accuracy = (TP + TN) / (TP + FP + FN + TN),
                         total_error_rate = 1 - accuracy,
                         mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                         year = fff
                       ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # Timestamp and remove objects
  
  
  
  rm(logistic_regression_model)
  
  
  
  # (8) Random forests (Top-100) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,weekend,
           104:203)
  
  temp$any_opioid %<>% as.factor()
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Factorize outcomes
  
  train$any_opioid %<>% as.factor()
  test$any_opioid %<>% as.factor()
  
  # Perform analysis
  yyy <- round(sqrt(ncol(train) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = train,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, test, type = "response")
  
  mean(rf.probs == test$any_opioid)
  
  table(rf.probs, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate_at(vars(TP, TN, FP, FN),
                          funs(as.numeric(.))) %>% mutate(
                            approach = paste0("Random Forest (Top-100)"),
                            recall = TP / (TP + FN),
                            precision = TP / (TP + FP),
                            specificity = TN / (TN + FP),
                            f_measure = 2*(precision * recall) / (precision + recall),
                            fallout = FP / (FP + TN),
                            accuracy = (TP + TN) / (TP + FP + FN + TN),
                            mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                            total_error_rate = 1 - accuracy
                          ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  
  # Timestamp and remove objects
  
  
  
  rm(temp_rf_model)
  
  
  
  # (9) Random forests (Top-50) --------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:153)
  
  temp$any_opioid %<>% as.factor()
  
  # Split into training and test datasets 
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Factorize outcomes
  
  train$any_opioid %<>% as.factor()
  test$any_opioid %<>% as.factor()
  
  # Perform analysis
  yyy <- round(sqrt(ncol(train) - 1))    
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = train,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, test, type = "response")
  
  mean(rf.probs == test$any_opioid)
  
  table(rf.probs, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate_at(vars(TP, TN, FP, FN),
                          funs(as.numeric(.))) %>% mutate(
                            approach = paste0("Random Forest (Top-50)"),
                            recall = TP / (TP + FN),
                            precision = TP / (TP + FP),
                            specificity = TN / (TN + FP),
                            f_measure = 2*(precision * recall) / (precision + recall),
                            fallout = FP / (FP + TN),
                            accuracy = (TP + TN) / (TP + FP + FN + TN),
                            mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                            total_error_rate = 1 - accuracy
                          ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  
  # Timestamp and remove objects
  
  rm(temp_rf_model)
  
  
  
  # (10) Random forests (Top-20) --------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,weekend,
           104:123)
  
  temp$any_opioid %<>% as.factor()
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Factorize outcomes
  
  train$any_opioid %<>% as.factor()
  test$any_opioid %<>% as.factor()
  
  # Perform analysis
  yyy <- round(sqrt(ncol(train) - 1))    
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = train,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, test, type = "response")
  
  mean(rf.probs == test$any_opioid)
  
  table(rf.probs, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate_at(vars(TP, TN, FP, FN),
                          funs(as.numeric(.))) %>% mutate(
                            approach = paste0("Random Forest (Top-20)"),
                            recall = TP / (TP + FN),
                            precision = TP / (TP + FP),
                            specificity = TN / (TN + FP),
                            f_measure = 2*(precision * recall) / (precision + recall),
                            fallout = FP / (FP + TN),
                            accuracy = (TP + TN) / (TP + FP + FN + TN),
                            mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                            total_error_rate = 1 - accuracy
                          ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  
  # Timestamp and remove objects
  
  
  
  rm(temp_rf_model)
  
  
  
  # (11) Random forests (Top-10) --------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:113)
  
  temp$any_opioid %<>% as.factor()
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Factorize outcomes
  
  train$any_opioid %<>% as.factor()
  test$any_opioid %<>% as.factor()
  
  # Perform analysis
  yyy <- round(sqrt(ncol(train) - 1))    
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = train,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, test, type = "response")
  
  mean(rf.probs == test$any_opioid)
  
  table(rf.probs, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate_at(vars(TP, TN, FP, FN),
                          funs(as.numeric(.))) %>% mutate(
                            approach = paste0("Random Forest (Top-10)"),
                            recall = TP / (TP + FN),
                            precision = TP / (TP + FP),
                            specificity = TN / (TN + FP),
                            f_measure = 2*(precision * recall) / (precision + recall),
                            fallout = FP / (FP + TN),
                            accuracy = (TP + TN) / (TP + FP + FN + TN),
                            mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                            total_error_rate = 1 - accuracy
                          ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  
  # Timestamp and remove objects
  
  rm(temp_rf_model)
  
  
  
  # (12) Random forests (Only decedent characteristics) -------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,
           weekend)
  
  temp$any_opioid %<>% as.factor()
  
  # Split into training and test datasets 
  
  temp %<>% ungroup()
  
  temp %<>% mutate(id_overdose = row_number())
  
  train <- temp %>% sample_frac(0.80)
  test <- anti_join(temp, train, by = c('id_overdose'))
  
  train %<>% select(-id_overdose)
  test %<>% select(-id_overdose)
  
  # Drop variables with no variation in either group
  
  train %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> analysis_vars
  
  test %>% select(female:ncol(train)) %>%
    summarise_all(funs(sum(., na.rm = TRUE))) %>%
    gather(variable, value) %>%
    filter(value == 0) -> test_vars
  
  if(nrow(test_vars) > 0) {
    drop_columns <- test_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  
  if(nrow(analysis_vars) > 0) {
    drop_columns <- analysis_vars %>% pull(variable)
    
    for(xxx in drop_columns) {
      
      train %<>% select(-contains(xxx))
      test %<>% select(-contains(xxx))
    }
    
  }
  rm(test_vars, analysis_vars, drop_columns)
  
  # Factorize outcomes
  
  train$any_opioid %<>% as.factor()
  test$any_opioid %<>% as.factor()
  
  # Perform analysis
  yyy <- round(sqrt(ncol(train) - 1))    
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = train,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, test, type = "response")
  
  mean(rf.probs == test$any_opioid)
  
  table(rf.probs, test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate_at(vars(TP, TN, FP, FN),
                          funs(as.numeric(.))) %>% mutate(
                            approach = paste0("Random Forest (Decedent characteristics)"),
                            recall = TP / (TP + FN),
                            precision = TP / (TP + FP),
                            specificity = TN / (TN + FP),
                            f_measure = 2*(precision * recall) / (precision + recall),
                            fallout = FP / (FP + TN),
                            accuracy = (TP + TN) / (TP + FP + FN + TN),
                            mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
                            total_error_rate = 1 - accuracy
                          ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  # Timestamp -------------------------
  
  print(fff)
  
}

# Save object of accuracy estimates as RDS file --------------------------

logistic_accuracy %>% saveRDS('Opioids_ML_2/Scratch/Table_X_Unbalanced_Accuracy.rds')

# Create figure ------------------------

logistic_accuracy <- readRDS('Opioids_ML_2/Scratch/Table_X_Unbalanced_Accuracy.rds')

temp_summary <- logistic_accuracy %>% filter(year >= 2017)

temp_summary %<>% mutate(accuracy = ifelse(approach == 'Naive accuracy', 1 + accuracy, accuracy))

temp_summary %<>% mutate(method = case_when(
  str_detect(string = approach, pattern = 'Random') == TRUE ~ 'Random Forest',
  str_detect(string = approach, pattern = 'Logistic') == TRUE ~ 'Logistic Regression',
  str_detect(string = approach, pattern = 'Naive') == TRUE ~ 'Majority Class'
)) 

temp_summary$approach %<>% str_replace_all(pattern = 'Logistic Regression', replacement = '') %>%
  str_replace_all(pattern = 'Random Forest', replacement = '') %>%
  str_replace_all(pattern = 'Contributing (C|c)auses', replacement = 'CC') %>%
  str_replace_all(pattern = '\\(', replacement = '') %>%
  str_replace_all(pattern = '\\)', replacement = '') %>%
  str_replace_all(pattern = 'characteristics', replacement = 'Characteristics') %>%
  str_trim()

temp_summary %>% statar::tab(approach)

temp <- temp_summary %>% 
  select(approach, method, year, accuracy) %>%
  gather(variable, value, -year, -approach, -method)

temp$approach %<>% str_replace_all(pattern = 'Naive accuracy', replacement = 'No Vars.')

temp %<>% mutate(approach = ifelse(approach == 'Top-10', 'Top-10 CC', approach),
                 approach = ifelse(approach == 'Top-100', 'Top-100 CC', approach),
                 approach = ifelse(approach == 'Top-125', 'Top-125 CC', approach),
                 approach = ifelse(approach == 'Top-20', 'Top-20 CC', approach),
                 approach = ifelse(approach == 'Top-50', 'Top-50 CC', approach))

temp$approach <- factor(temp$approach, levels = c('Top-125 CC', 'Top-100 CC', 'Top-50 CC', 
                                                  'Top-20 CC', 'Top-10 CC', 'Decedent Characteristics',
                                                  'No Vars.'))

temp$method <- factor(temp$method, levels = c('Random Forest',
                                              'Logistic Regression',
                                              'Majority Class'))

temp_plot <- ggplot(data = temp,
                    aes(x = as.character(year), y = value, 
                        colour = approach, shape = method)) + 
  geom_point(size = 2) + theme_classic() + 
  theme(legend.title = element_blank()) +
  labs(x = 'Year', y = 'Out-of-sample predictive accuracy') + 
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))

temp_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Accuracy_Unbalanced_Proposal.jpg')

# Create figure (MCC) --------------------

temp_summary <- logistic_accuracy %>% filter(year >= 2017)

temp_summary %<>% mutate(accuracy = ifelse(approach == 'Naive accuracy', 1 + accuracy, accuracy))

temp_summary %<>% mutate(method = case_when(
  str_detect(string = approach, pattern = 'Random') == TRUE ~ 'Random Forest',
  str_detect(string = approach, pattern = 'Logistic') == TRUE ~ 'Logistic Regression',
  str_detect(string = approach, pattern = 'Naive') == TRUE ~ 'Majority Class'
)) 

temp_summary$approach %<>% str_replace_all(pattern = 'Logistic Regression', replacement = '') %>%
  str_replace_all(pattern = 'Random Forest', replacement = '') %>%
  str_replace_all(pattern = 'Contributing (C|c)auses', replacement = 'CC') %>%
  str_replace_all(pattern = '\\(', replacement = '') %>%
  str_replace_all(pattern = '\\)', replacement = '') %>%
  str_replace_all(pattern = 'characteristics', replacement = 'Characteristics') %>%
  str_trim()

temp_summary %>% statar::tab(approach)

temp <- temp_summary %>% 
  select(approach, method, year, mcc) %>%
  gather(variable, value, -year, -approach, -method)

temp$approach %<>% str_replace_all(pattern = 'Naive accuracy', replacement = 'No Vars.')

temp %<>% mutate(approach = ifelse(approach == 'Top-10', 'Top-10 CC', approach),
                 approach = ifelse(approach == 'Top-100', 'Top-100 CC', approach),
                 approach = ifelse(approach == 'Top-125', 'Top-125 CC', approach),
                 approach = ifelse(approach == 'Top-20', 'Top-20 CC', approach),
                 approach = ifelse(approach == 'Top-50', 'Top-50 CC', approach))

temp$approach <- factor(temp$approach, levels = c('Top-125 CC', 'Top-100 CC', 'Top-50 CC', 
                                                  'Top-20 CC', 'Top-10 CC', 'Decedent Characteristics',
                                                  'No Vars.'))

temp %<>% filter(method != 'Majority Class')

temp$method <- factor(temp$method, levels = c('Random Forest',
                                              'Logistic Regression',
                                              'Majority Class'))

temp_plot <- ggplot(data = temp,
                    aes(x = as.character(year), y = value, 
                        colour = approach, shape = method)) + 
  geom_point(size = 2) + theme_classic() + 
  theme(legend.title = element_blank()) +
  labs(x = 'Year', y = 'Matthews Correlation Coefficient (MCC)') + 
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))

temp_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Accuracy_Unbalanced_Proposal_MCC.jpg')

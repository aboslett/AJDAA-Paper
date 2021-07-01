# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Evaluate accuracy of alternative models with balanced opioid or non-opioid classes. The main model alternatives
# include different levels of contributing causes, from the top-150 occurring in a given year within drug overdoses
# to 0. The other main alternative is a shift from logistic regression to random forests. In this code, we have
# dropped all other modeling approaches used in behind-the-scenes evaluation. 

logistic_accuracy <- data.frame()

for(fff in 1999:2019) {
  
  # Import data --------------------
  
  overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_', fff, '_inter.rds'))
  
  # Estimate models ----------------------------
  
  temp <- overdose_deaths %>% filter(year == fff & unidentified_drug_only == 0) %>%
    select(any_opioid)
  
  temp$any_opioid %<>% as.numeric()
  
  temp %<>% summarise(accuracy = mean(any_opioid)) %>%
    mutate(year = fff,
           approach = 'Naive accuracy')
  
  logistic_accuracy %<>% bind_rows(temp)
  
  # (1) Logistic regression (Top-125) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:228)
  
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Top-125)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
    year = fff
  ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # (2) Logistic regression (Top-100) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:203)
  
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Top-100)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Top-50 Contributing Causes)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Top-20 Contributing Causes)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Top-10 Contributing Causes)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
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
  
  # Logistic regression model by year
  
  logistic_regression_model <- glm(any_opioid ~ .,
                                   family = "binomial",
                                   data = temp_analysis)
  
  # Use Logistic model to predict likelihood of opioid use
  
  glm.probs <- predict(logistic_regression_model, 
                       temp_test, 
                       type = "response")
  
  glm.probs[1:10]
  
  glm.pred = rep(0, nrow(temp_test))
  glm.pred[glm.probs > .5] = 1
  
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
  
  glm_cm %>% mutate(
    approach = "Logistic Regression (Decedent Characteristics)",
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = (precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy,
    year = fff
  ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  # Timestamp and remove objects
  
  
  
  rm(logistic_regression_model)
  
  
  
  # (7) Random forests (Top-125) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown, weekend,
           104:228)
  
  temp$any_opioid %<>% as.factor()
  
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE, do.trace = 10)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Top-125)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy
  ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  
  # Timestamp and remove objects
  
  rm(temp_rf_model)
  
  # (8) Random forests (Top-100) -----------------------------
  
  # Read file
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0 & year == fff) %>%
    select(any_opioid, 
           female:age_unknown,weekend,
           104:203)
  
  temp$any_opioid %<>% as.factor()
  
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Top-100)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Top-50)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Top-20)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Top-10)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
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
  
  yyy <- round(sqrt(ncol(temp) - 1))
  
  temp_rf_model = randomForest(any_opioid ~ .,
                               data = temp_analysis,
                               mtry = yyy, 
                               ntree = 500, 
                               importance = TRUE)
  
  temp_rf_model
  
  rf.probs = predict(temp_rf_model, temp_test, type = 'response')
  
  mean(rf.probs == temp_test$any_opioid)
  
  table(rf.probs, temp_test$any_opioid) %>% as.data.frame() %>%
    mutate(
      type = case_when(
        rf.probs == 0 & Var2 == 0 ~ "TN", 
        rf.probs == 0 & Var2 == 1 ~ "FN", 
        rf.probs == 1 & Var2 == 1 ~ "TP", 
        rf.probs == 1 & Var2 == 0 ~ "FP")
    ) %>%
    select(Freq, type) %>%
    spread(type, Freq) -> glm_rf_cm
  
  glm_rf_cm %>% mutate(
    approach = paste0("Random Forest (Decedent characteristics)"),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    f_measure = 2*(precision * recall) / (precision + recall),
    fallout = FP / (FP + TN),
    accuracy = (TP + TN) / (TP + FP + FN + TN),
    total_error_rate = 1 - accuracy
  ) %>%
    select(approach:total_error_rate) -> rf_accuracy
  
  rf_accuracy %<>% mutate(year = fff)
  
  logistic_accuracy %<>% bind_rows(rf_accuracy)
  
  # Timestamp -------------------------
  
  print(fff)
  
}

# Save file of out-of-sample accuracy estimates ---------------------------

logistic_accuracy %>% saveRDS('Opioids_ML_2/Scratch/Table_X_Out_of_Sample_Accuracy_Balanced.rds')

# Create figure ------------------------

logistic_accuracy <- readRDS('Opioids_ML_2/Scratch/Table_X_Out_of_Sample_Accuracy_Balanced.rds')

temp_summary <- logistic_accuracy

temp_summary %<>% mutate(method = case_when(
  str_detect(string = approach, pattern = 'Random') == TRUE ~ 'Random Forest',
  str_detect(string = approach, pattern = 'Logistic') == TRUE ~ 'Logistic Regression'
)) 

temp_summary$approach %<>% str_replace_all(pattern = 'Logistic Regression', replacement = '') %>%
  str_replace_all(pattern = 'Random Forest', replacement = '') %>%
  str_replace_all(pattern = 'Contributing Causes', replacement = 'CC') %>%
  str_replace_all(pattern = '\\(', replacement = '') %>%
  str_replace_all(pattern = '\\)', replacement = '') %>%
  str_replace_all(pattern = 'characteristics', replacement = 'Characteristics') %>%
  str_trim()

temp_summary %>% statar::tab(approach)

temp <- temp_summary %>% 
  select(approach, method, year, accuracy) %>%
  gather(variable, value, -year, -approach, -method)

temp %<>% mutate(approach = ifelse(approach == 'Top-10', 'Top-10 CC', approach),
                 approach = ifelse(approach == 'Top-100', 'Top-100 CC', approach),
                 approach = ifelse(approach == 'Top-125', 'Top-125 CC', approach),
                 approach = ifelse(approach == 'Top-20', 'Top-20 CC', approach),
                 approach = ifelse(approach == 'Top-50', 'Top-50 CC', approach))

temp$approach <- factor(temp$approach, levels = c('Top-125 CC', 'Top-100 CC', 'Top-50 CC', 
                                                  'Top-20 CC', 'Top-10 CC', 'Decedent Characteristics'))

temp$method <- factor(temp$method, levels = c('Random Forest',
                                              'Logistic Regression'))

temp_plot <- ggplot(data = temp,
                    aes(x = year, y = value, 
                        colour = approach, linetype = method)) + 
  geom_line() + theme_classic() + 
  theme(legend.title = element_blank()) +
  labs(x = 'Year', y = 'Out-of-sample predictive accuracy',
       title = 'Out-of-sample predictive accuracy',
       subtitle = 'Models of opioid involvement in drug overdoses, 1999-2018') + 
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))

temp_plot

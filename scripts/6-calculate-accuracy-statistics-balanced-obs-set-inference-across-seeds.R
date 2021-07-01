# Andrew Boslett
# University of Rochester Medical Center
# Academic email: andrew_boslett@urmc.rochester.edu
# Permanent email: aboslet1@gmail.com

# Goal: Evaluate accuracy of alternative models with balanced opioid or non-opioid classes. We do this in 200 random
# draws of data for the test and training set. We obtain a distribution of accuracy estimates across these random samples,
# the ultimate goal being that our inference re: the predictive ability of our contributing causes-based approach versus
# a model that only includes decedent characteristics.

# Note: This code is only set up for a single year but could easily be ramped up for multiple years of execution. 

# Choose year -----------------------
# Note: Can make this code operate across 
fff <- 2019

# Import overdose deaths ----------------------

overdose_deaths <- readRDS(paste0(remote_drive_rds, '/', 'mort_drug_overdoses_',
                                  as.character(fff), '_inter.rds'))

# Set loop to randomly set seed and get accuracy in test set -----------------

logistic_accuracy <- data.frame()

for(fff in 1:200) {
  
  # Set random seed 
  
  set.seed(fff)
  
  # Estimate model with top-100 contributing causes of death
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0) %>%
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
  
  # Estimate model
  
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
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>%
    mutate(
      approach = "Logistic Regression (Top-100)",
      random_seed = fff,
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      f_measure = (precision * recall) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy, 
      mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
      year = 2018
    ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  print(fff)
  
}

# Only decedent characteristics 

for(fff in 1:200) {
  
  # Set random seed 
  
  set.seed(fff)
  
  # Estimate model with top-100 contributing causes of death
  
  temp <- overdose_deaths %>% filter(unidentified_drug_only == 0) %>%
    select(any_opioid, 
           female:age_unknown, weekend)
  
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
  
  # Estimate model
  
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
  
  glm_cm %>% mutate_at(vars(TP, TN, FP, FN),
                       funs(as.numeric(.))) %>%
    mutate(
      approach = "Logistic Regression (Decedent Characteristics)",
      random_seed = fff,
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      specificity = TN / (TN + FP),
      f_measure = (precision * recall) / (precision + recall),
      fallout = FP / (FP + TN),
      accuracy = (TP + TN) / (TP + FP + FN + TN),
      total_error_rate = 1 - accuracy, 
      mcc = ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(FP+FN)*(TN+FP)*(TN+FN)),
      year = 2018
    ) %>%
    select(approach:year) -> glm_accuracy
  
  logistic_accuracy %<>% bind_rows(glm_accuracy)
  
  print(fff)
  
}

# Generate a graph of the total predictive accuracy ---------------------

histogram_plot <- ggplot(data = logistic_accuracy,
                         aes(x = accuracy)) + 
  theme_classic() + labs(x = 'Total predictive accuracy', y = 'Count') + 
  geom_density(aes(fill = approach)) + 
  theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c('#ef8a62', '#67a9cf')) + 
  ylim(0, 70) + theme(legend.position = c(0.5, 0.95))

histogram_plot

ggsave('Opioids_ML_2/Figures/Figure_X_Distribution_of_Accuracy_across_Seeds_Density.jpg')

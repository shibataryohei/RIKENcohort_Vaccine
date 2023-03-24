# lasso regression

## Preparation
library(caret)
library(tidyverse)

Hib_Dichotomous_tbw %>% 
  ungroup %>% 
  select(SubjectID, `7Y`) %>%
  mutate(`7Y` = if_else(`7Y` == "≥1.0", "Yes", "No")) %>% 
  mutate_all(as.factor) %>% 
  rename(LTPT = `7Y`) -> LTPT_tbl

Genus_tbl %>% 
  filter(Age == "1W") %>% 
  check_levels("SubjectID") %>% 
  intersect(., SubjectID_Analysis) -> subjectID_analysis_feces

Genus_tbl %>% 
  filter(Age == "1W") %>% 
  filter(SubjectID %in% subjectID_analysis_feces) %>% 
  filter_mb(., 0.2) %>% 
  mutate(Value = Value/30) %>% 
  spread(Variable, Value) %>% 
  select(-Age) %>% 
  inner_join(LTPT_tbl) %>% 
  c2r("SubjectID") %>% 
  as.data.frame -> genus_lasso_df

myTrainingControl <- trainControl(method = "LOOCV", 
                                  returnResamp = "all",
                                  savePredictions = TRUE, 
                                  classProbs = TRUE,
                                  summaryFunction = twoClassSummary,
                                  preProcOptions = NULL)
# Grid
train_grid.lasso <- expand.grid(alpha  = 1, # 1, lasso; 0, ridge
                                lambda = seq(0, 1, by = 0.01)) # 0.001

fit_lasso <- caret::train(LTPT ~ .,  
                          data   = genus_lasso_df, 
                          method = "glmnet", 
                          metric = "ROC",  
                          tuneGrid  = train_grid.lasso,
                          trControl = myTrainingControl)

# Decide model to evaluate
model_lasso_final <- fit_lasso 

# Check the fitted model
plot(model_lasso_final)
# Function get best result 

model_lasso_final$bestTune %>% 
  rownames -> best_n

model_lasso_final$results %>% 
  .[best_n, ] %>% 
  as.data.frame -> best_result

best_result$lambda -> best_lambda


# Result of best parameter1
model_lasso_final$finalModel %>% 
  coef(., s = best_lambda) %>% 
  exp -> lasso_full.or

# Result of best parameter
coef(model_lasso_final$finalModel,
     model_lasso_final$bestTune$lambda) %>% 
  as.matrix %>% 
  as.data.frame %>% 
  r2c("Variable") %>% 
  filter(!grepl("ntercept", Variable)) %>% 
  rename(Coef = s1) %>% 
  filter(Coef != 0) %>% 
  filter(!grepl("Intercept", Variable)) %>% 
  .$Variable -> variable_lasso

genus_lasso_df %>% 
  .[, c(variable_lasso, "LTPT")] -> genus_lasso2_df

# Bootstrap
set.seed(1202)
furrr::future_map(1:2000, function(i){
  
  sample(x = 1:nrow(genus_lasso2_df), 
          size = nrow(genus_lasso2_df), # 21
          replace = T) -> boot_rows
  
  genus_lasso2_df %>% 
    .[boot_rows, ] -> genus_lasso_bootstrap_df
  
  boot.lasso_full <- caret::train(LTPT ~ .,  
                            data   = genus_lasso_bootstrap_df, 
                            method = "glmnet", 
                            metric = "ROC",  
                            tuneGrid  = expand.grid(lambda = best_lambda,
                                                    alpha = 0),
                            trControl = myTrainingControl)
  
  coef(boot.lasso_full$finalModel,
       s = best_lambda) %>% 
    as.matrix %>% 
    as.data.frame %>% 
    r2c("Variable") %>% 
    filter(!grepl("Intercept", Variable)) %>% 
    mutate(Simulation = i) %>% 
    rename(coef = s1)}) -> list

list %>% 
  do.call(bind_rows, .) %>% 
  group_by(Variable) %>% 
  summarise(OR = exp(quantile(coef, 0.5, na.rm = T)),
            lowCI = exp(quantile(coef, 0.025, na.rm = T)),
            upCI = exp(quantile(coef, 0.975, na.rm = T))) -> res_tbw

res_tbw -> Lasso_1W_tbw

Lasso_1W_tbw %>% 
  filter(OR > 1&lowCI>1|OR <= 1&upCI<=1)

write_csv(Lasso_1W_tbw, "CSV/Lasso_1W.csv")

## Lasso
```{R}
library(caret)
library(tidyverse)

Hib_Dichotomous_tbw %>% 
  ungroup %>% 
  select(SubjectID, `7Y`) %>%
  mutate(`7Y` = if_else(`7Y` == "≥1.0", "Yes", "No")) %>% 
  mutate_all(as.factor) %>% 
  rename(LTPT = `7Y`) -> LTPT_tbl

map(Age_Analysis, function(age){
  Genus_tbl %>% 
    filter(Age == age) %>% 
    check_levels("SubjectID") %>% 
    intersect(., SubjectID_Analysis) -> subjectID_analysis_feces
  
  Genus_tbl %>% 
    filter(Age == age) %>% 
    filter(SubjectID %in% subjectID_analysis_feces) %>% 
    filter_mb(., 0.2) %>% 
    mutate(Value = Value/3) %>% 
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
              upCI = exp(quantile(coef, 0.975, na.rm = T)))}) -> res_tbw_list

names(res_tbw_list) <- Age_Analysis

imap(res_tbw_list, ~ write_csv(.x, glue("/Users/shibataryohei/Dropbox/Database/RIKENcohort/CSV/Lasso_{.y}.csv")))

map(res_tbw_list, ~ .x %>% 
      filter(OR < 1 & upCI < 1|OR > 1 & lowCI > 1))
```


```{R}
map(Age_Analysis, function(age){
  Genus_tbl %>% 
    filter(Age == age) %>% 
    check_levels("SubjectID") %>% 
    intersect(., SubjectID_Analysis) -> subjectID_analysis_feces
  
  Genus_tbl %>% 
    filter(Age == age) %>% 
    filter(SubjectID %in% subjectID_analysis_feces) %>% 
    filter_mb(., 0.2) %>% 
    inner_join(LTPT_tbl) %>% 
    group_bye
  ```
  
  
  ```{R}
  res_tbw -> Lasso_1W_tbw
  
  map(res_tbw_list, function(tbw){
    tbw %>% 
      filter(OR > 1&lowCI>1|OR <= 1&upCI<=1)
  })
  
  
  write_csv(Lasso_1W_tbw, "CSV/Lasso_1W.csv")
  ```
  
  
  
  
  
  
  
  
  ## coda4microbiome
  ```{R}
  install.packages("coda4microbiome")
  
  library(coda4microbiome)
  data(Crohn,
       package = "coda4microbiome")
  coda_glmnet(x_Crohn[,(1:10)],y_Crohn,showPlots= FALSE) -> res
  
  res %>% names
  
  res[[4]]
  ```
  
  ```{R}
  Hib_Dichotomous_tbw %>% 
    ungroup %>% 
    select(SubjectID, `7Y`) %>%
    mutate(`7Y` = if_else(`7Y` == "≥1.0", "Yes", "No")) %>% 
    mutate_all(as.factor) %>% 
    rename(LTPT = `7Y`) -> LTPT_tbl
  
  set.seed(1)
  
  map(Age_Analysis, function(age){
    Genus_tbl %>% 
      filter(Age == age) %>% 
      check_levels("SubjectID") %>% 
      intersect(., SubjectID_Analysis) -> subjectID_analysis_feces
    
    Genus_tbl %>% 
      filter(Age == age) %>% 
      filter(SubjectID %in% subjectID_analysis_feces) %>% 
      filter_mb(., 0.2) %>% 
      # mutate(Value = Value/30) %>% 
      spread(Variable, Value) %>% 
      select(-Age) %>% 
      inner_join(LTPT_tbl, .) %>% 
      c2r("SubjectID") %>% 
      as.data.frame -> df
    
    coda_glmnet(df[, -1],
                df[, 1],
                showPlots = FALSE)}) -> res_list
  
  names(res_list) <- Age_Analysis
  map(res_list, ~ .x[[2]])
  ```
  
  ```{R}
  Genus_tbl %>% 
    mutate(Age = droplevels(Age)) %>% 
    split(., .$Age) %>% 
    map(~ .x %>% 
          select(-Age)) -> Genus_tbl_list
  
  Genus_tbl_list %>% 
    imap(~ .x %>% 
           inner_join(LTPT_tbl) %>%
           filter_mb(., 0.15) %>% 
           spread(Variable, Value) %>%
           gather(Variable, Value, -SubjectID) %>%
           ungroup %>%
           mutate(Variable = fct_relevel(Variable, "LTPT")) %>%
           spread(SubjectID, Value) %>%
           write.table(glue("Aim3/Table_Genus_LTPT_{.y}.txt"),
                       col.names = FALSE,
                       row.names = FALSE,
                       quote = FALSE,
                       sep="\t"))
  ```
  
  
  ```{R}
  Ages <- c("1W", "1M", "1Y", "5Y", "7Y")
  
  map(Ages, function(age){
    read_delim(glue("/Users/shibataryohei/Dropbox/Database/RIKENcohort/Vaccine/LEfSe_{age}.lefse_internal_res"),
               col_names = FALSE) %>% 
      rename(Variable = X1,
             LDA = X2,
             LTPT = X3,
             adj.LDA = X4,
             P.value = X5) %>%
      dplyr::select(Variable, LDA, LTPT, P.value) %>% 
      mutate(Age = age)}) %>% 
    do.call(bind_rows, .) -> tbw
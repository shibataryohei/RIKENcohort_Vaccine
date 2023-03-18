# Ridge regression
library(caret)

Hib_Dichotomous_tbw %>% 
  ungroup %>% 
  select(SubjectID, `7Y`) %>%
  mutate(`7Y` = if_else(`7Y` == "≥1.0", "Yes", "No")) %>% 
  mutate_all(as.factor) %>% 
  rename(LTPT = `7Y`) -> LTPT_tbl

#Lipid_Species_Norm_df %>% 
Genus_tbl %>% 
  filter(Age == "1W") %>% 
  check_levels("SubjectID") %>% 
  intersect(., SubjectID_Analysis) -> subjectID_analysis_feces

Genus_tbl %>% 
  filter(Age == "1W") %>% 
  filter(SubjectID %in% subjectID_analysis_feces) %>% 
  filter_mb(., 0.1) %>% 
  spread(Variable, Value) %>% 
  select(-Age) %>% 
  inner_join(LTPT_tbl) %>% 
  c2r("SubjectID") %>% 
  as.data.frame -> genus_lasso_df

LTPT_tbl %>% 
  filter(SubjectID %in% subjectID_analysis_feces) %>% 
  .$LTPT -> ltpt

levels(ltpt)

myTrainingControl <- trainControl(method = "LOOCV", 
                                  returnResamp="all",
                                  savePredictions = TRUE, 
                                  classProbs = TRUE,
                                  summaryFunction = twoClassSummary,
                                  preProcOptions = NULL)
# Grid
train_grid.ridge <- expand.grid(alpha  = 0,
                                lambda = seq(0, 1, by = 0.01)) # 0.001


fit_ridge <- caret::train(LTPT ~ .,  
                          data   = genus_lasso_df, 
                          method = "glmnet", 
                          metric = "ROC",  
                          tuneGrid  = train_grid.ridge,
                          trControl = myTrainingControl)

# Decide model to evaluate
model_ridge_final <- fit_ridge 

# Check the fitted model
plot(model_ridge_final)
# Function get best result 

get_best_result = function(caret_fit){
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

# Best parameters
model_ridge_final$bestTune %>% 
  rownames -> best_n

model_ridge_final$results %>% 
  .[best_n, ] %>% 
  as.data.frame -> best_result

best_result$lambda -> best_lambda


# Result of best parameter1
model_ridge_final$finalModel %>% 
  coef(., s = best_lambda) %>% 
  exp -> ridge_full.or

# Result of best parameter
coef(model_ridge_final$finalModel,
       model_ridge_final$bestTune$lambda) %>% 
  as.data.frame %>% 
  c2r("variable") -> df_coeff_ridge

df_coeff_ridge.pre <- mat_coeff_ridge %>% as.data.frame()

df_coeff_ridge <- rownames_to_column(df_coeff_ridge.pre, var = "variable")
names(df_coeff_ridge)[ which( names(df_coeff_ridge)=="V1") ] <- "coef"
df_coeff_ridge$OR <- exp(df_coeff_ridge$coef)
df_coeff_ridge

####################################
modulename <- df_coeff_ridge[,1]
nsim <- 2000
nofv <- length(modulename)
# Create Data holder
data_boot_holder <- createdf(nsim,nofv, colnames = c( modulename))
result_or <- createdf(nofv,7, colnames = c( "Module","OR","lowCI","upCI", "Coef", "absCoef"))
######################################
# Loop
set.seed(1)
data_boot <- df_models
for (i in 1:nsim)
{
  boot_rows <-sample(x=c(1:nrow(data_boot)), size=nrow(data_boot), replace=T)
  data_boot2 <-data_boot[boot_rows,]
  boot.ridge_full  <-   caret::train(PPVuse ~ .,  
                                     data   = df_integrated, 
                                     method = "glmnet", 
                                     metric = "ROC", 
                                     tuneGrid = expand.grid(lambda = best_lambda , alpha = 0),
                                     trControl = myTrainingControl
  )
  coef <- coef(boot.ridge_full$finalModel, s = best_lambda)
  for (ii in 1:nofv) {
    data_boot_holder[i,ii] <- coef[ii+1,1]
  }}
# Extract Coef(Odds Ratio) -Confidence Interval
for (i in 1:nofv) {
  result_or[i,1] <-  df_coeff_ridge$variable[i] 
  d = quantile(data_boot_holder[,i], c(0.025,0.5, 0.975),na.rm=T)
  result_or[i,2] <- exp(d[2]) #0.5CI
  result_or[i,3] <- exp(d[1]) #0.025CI
  result_or[i,4] <- exp(d[3]) #0.975CI
  result_or[i,5] <- (d[2]) #0.5CI
  result_or[i,6] <- abs(d[2]) #0.5CI
}
Sys.setlocale("LC_ALL", 'UTF-8')
options(scipen = 10)

# Packages
source("~/Git/Database/Library/Library_Basic.R" )
source("~/Git/Database/Library/Library_Bioinformatics.R")
source("~/Git/Database/Function/Function.R")
source("~/Git/Database/Function/Function_GE.R")

source("~/Git/Database/RIKENcohort/RIKENcohort_Function.R")
source("~/Git/Database/RIKENcohort/RIKENcohort_Skin.R")

source("~/Git/Database/RIKENcohort/RIKENcohort_Vaccine.R")

# Clinicaldta_tbw
source("~/Git/Database/RIKENcohort/RIKENcohort_Clinicaldata.R")

Cohort_tbw %>% 
  inner_join(Antibiotics_tbw) %>% 
  inner_join(Smoking_tbw) %>% 
  inner_join(Parents_tbw) %>% 
  inner_join(Perinatal_tbw) %>% 
  inner_join(SolidFood_tbw) %>% 
  inner_join(HBM_tbw) %>% 
  inner_join(Sibling_Pet_tbw) %>% 
  inner_join(Daycare_tbw) -> Clinicaldata_tbw
  
  
  
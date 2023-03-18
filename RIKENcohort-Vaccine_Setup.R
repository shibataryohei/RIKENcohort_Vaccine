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
  inner_join(Daycare_tbw) %>% 
  c2f -> Clinicaldata_tbw

# Clinicaldata_tbw %>% 
#   names %>% 
#   data.frame(Variable_R = .,
#              Variable_Table = NA) %>% 
#   write_csv(.,
#             "/Users/shibataryohei/Dropbox/Manuscript/RIKENcohort-Vaccine/Dictionary.csv")

Clinicaldata_tbw %>% 
  mutate(Sibling = if_else(Sibling > 1, "+", "-")) %>% 
  mutate_if(grepl("HBM@1M|HBM@4M", names(.)), funs(case_when(. == "Exclusive" ~ "Exclusive or almost exclusive",
                                                            . == "Almost" ~ "Exclusive or almost exclusive",
                                                            . == "Some" ~ "Some",
                                                            . == "Little" ~ "Little or no",
                                                            . == "No" ~ "Little or no"))) %>% 
  mutate(Gestation_Week = if_else(Gestation_Week < 37, "+", "-")) %>% 
  mutate(BirthBW = if_else(BirthBW < 2500, "+", "-")) %>% 
  mutate(Hospital = fct_relevel(Hospital, "CUH")) %>% 
  c2f %>% 
  mutate_if(is.factor, fct_relevel_plus) %>% 
  mutate_if(is.factor, fct_relevel_hbm) %>% 
  c2f -> Clinicaldata_Analysis_tbw 

read_excel("/Users/shibataryohei/Dropbox/Manuscript/RIKENcohort-Vaccine/Dictionary_RIKENcohort-Vaccine.xlsx") %>% 
  filter(!is.na(Number)) %>% 
  select(Number, Variable_R, Variable_Table) -> Dictionary_tbw

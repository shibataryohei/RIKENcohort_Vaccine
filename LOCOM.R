# devtools::install_github("yijuanhu/LOCOM")
library(LOCOM)

age <- "1W"
Hib_Dichotomous_tbw %>% 
  ungroup %>% 
  select(SubjectID, `7Y`) %>%
  mutate(`7Y` = if_else(`7Y` == "≥1.0", 1, 0)) %>% 
  rename(LTPT = `7Y`) -> LTPT_tbl

purrr::map(c("1W", "1M", "1Y", "5Y", "7Y"), function(age){
  
#Genus_tbl %>% 
    
OTU_tbl %>% 
  filter(Age == age) %>% 
  filter(SubjectID %in% SubjectID_Analysis) %>% 
  spread(Variable, Value) %>% 
  select(-Age) %>% 
  c2r("SubjectID") %>% 
  as.matrix -> genus_mx

LTPT_tbl %>% 
  filter(SubjectID %in% rownames(genus_mx)) %>% 
  c2r("SubjectID") %>% 
  as.matrix -> lptp_locommx

# running locom
locom(otu.table = genus_mx,
             Y = lptp_locommx[, "LTPT"],
             # C = data.matrix(model.matrix(LTPT_LOCOM_mx[, "LTPT"] ~ LTPT_LOCOM_mx[, "Dammy", drop = FALSE]))[, -1],
             fdr.nominal = 0.1,
             seed = 1202,
             adjustment = "Sandev",
             n.cores = 4) }) -> res_list

names(res_list) <- c("1W", "1M", "1Y", "5Y", "7Y")

imap(res_list, function(list, age){
  imap(list[1:3], function(vec, x){
  vec %>% 
    as.data.frame %>% 
    as_tibble %>% 
    mutate(Result = x) -> tbw}) %>% 
  do.call(bind_rows, .) %>% 
  gather(Variable, Value, -Result) %>% 
    mutate(Age = age)}) %>% 
  do.call(bind_rows, .) -> LOCOM_tbl

LOCOM_tbl %>% 
  filter(Value < 0.05&Result == "p.otu") %>% 
  inner_join(OTUassign_tbw %>% 
               rename(Variable = OTU) %>% 
               select(Variable, Species)) -> tbl


LOCOM_tbl %>% 
  filter(Result == "p.otu") %>% 
  spread(Age, Value) -> tbl

imap(res_list, function(list, age){
  list %>% 
    .$p.global %>% 
    data.frame(P.global = .) %>% 
    mutate(Age = age)}) %>% 
  do.call(bind_rows, .) -> Pglobal_tbl
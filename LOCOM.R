## Figure EX: Genus --> LTPT
```{R}
# devtools::install_github("yijuanhu/LOCOM")
# library(LOCOM)

Hib_Dichotomous_tbw %>% 
  ungroup %>% 
  select(SubjectID, `7Y`) %>%
  mutate(`7Y` = if_else(`7Y` == "≥1.0", 1, 0)) %>% 
  rename(LTPT = `7Y`) -> LTPT_tbl

purrr::map(c("1W", "1M", "1Y", "5Y", "7Y"), function(age){
  
  Genus_tbl %>% 
    # mutate(Value = Value/3000) %>% 
    # OTU_tbl %>% 
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

```{R}
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

imap(res_list, function(list, age){
  list %>% 
    .$p.global %>% 
    data.frame(P.global = .) %>% 
    mutate(Age = age)}) %>% 
  do.call(bind_rows, .) -> LOCOM_Pglobal_tbl

Genus_tbl %>% 
  group_by(Variable, Age) %>% 
  summarise(Value = mean(Value)) %>% 
  group_by(Variable) %>% 
  summarise(Value = mean(Value)) %>% 
  arrange(-Value) %>% 
  .$Variable -> Genus_TopX

LOCOM_tbl %>% 
  filter(Variable %in% Genus_TopX[1:30]) %>% 
  filter(Result != "q.otu") %>% 
  spread(Result, Value) %>% 
  mutate(p.otu = -log(p.otu, 10)) %>% 
  .$effect.size %>% 
  abs %>% 
  max -> range

LOCOM_tbl %>% 
  filter(Variable %in% Genus_TopX[1:30]) %>% 
  filter(Result != "p.otu") %>% 
  spread(Result, Value) %>% 
  mutate(effect.size = exp(effect.size), 10)
mutate(q.otu = -log(q.otu, 10)) %>% 
  mutate(Age = fct_relevel_age(Age)) %>% 
  mutate(Variable = fct_rev(Variable)) %>% 
  ggplot(., aes(x = Age, y = Variable, fill = effect.size, size = q.otu))+
  geom_point(pch = 21)+
  scale_fill_gradientn(colours = c("navy",
                                         "white",
                                         "firebrick3"),
                                         #values = c(-4, 0, 4),
                       limits = c(-4, 4),
                       space = "Lab",
                       na.value = "#C6DBEF",
                       guide = guide_colorbar(reverse = F))+
  labs(size = expression(paste("-log"[10],"-transformed FDR")),
       fill = "Odds ratio")+
  expand_limits(size = c(0.1, 0.75)) +
  theme(strip.text.x = element_text(size = 12),
        axis.text.y = ggtext::element_markdown(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.size = unit(5, 'mm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
ggsave("Aim3/Point_Age_Genus_LOCOM.png",
       dpi = 300,
       h = 5, w = 5.75)
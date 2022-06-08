require(kml3d)

x = "Hib"
map(c("Hib", "Sp"), function(x){
  
  Titer_tbl %>% 
  mutate(Value = log(Value, 2)) %>% 
  filter(Variable == x) %>% 
  filter(Age != "UCB") %>% 
  mutate(Age = gsub("Y", "", Age)) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  spread(Age, Value) %>% 
  rename(id = SubjectID) %>% 
  select(-Variable) %>% 
  as.data.frame -> titer_ag_df
  
  clusterLongData3d(traj = titer_ag_df,
                  timeInData = list(Titer = 2:5)) -> titer_ag_cld3d

  map(2:5, function(i){
  set.seed(1202)
  png(glue::glue("kml3d/{x}_k{i}.png"),
      res = 300, w = 1600, h = 1600)
  kml3d(titer_ag_cld3d,
        nbClusters = i,
        nbRedrawing = 20, # Real analysis needs at least 20 redrawingsd
        toPlot = "traj") 
  dev.off()
  })
  })

# Merge

Titer_tbl %>% 
  mutate(Value = log(Value, 2)) %>% 
  filter(Age != "UCB") %>% 
  unite_var %>% 
  spread(Variable, Value) %>% 
  rename(id = SubjectID) %>% 
  as.data.frame -> Titer_kml3d_df

clusterLongData3d(traj = Titer_kml3d_df,
                  timeInData = list(Hib = 2:5,
                                    Sp = 6:9)) -> Titer_cld3d

map(2:5, function(i){
  set.seed(1202)
  png(glue::glue("kml3d/Both_k{i}.png"),
      res = 300, w = 1600, h = 2000)
  kml3d(Titer_cld3d,
        nbClusters = i,
        nbRedrawing = 20, # Real analysis needs at least 20 redrawingsd
        toPlot = "traj") 
  dev.off()})


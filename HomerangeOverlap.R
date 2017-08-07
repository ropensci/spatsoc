

HomerangeNetwork = function(df){
  
  df$ANIMAL_ID = droplevels(df$ANIMAL_ID)
  
  ## Make a spatial points class
  xy<-cbind(df$EASTING,df$NORTHING)
  
  ### Make ID field
  ids<-data.frame(df$ANIMAL_ID)
  coordinates(ids)<-xy
  
  KOver = kerneloverlap(ids, method = "UDOI",percent = 95,grid = 700)
  
  KOver = as.matrix(KOver)
  diag(KOver) = 0
  
  hr.grph_df<- graph.adjacency(KOver,mode="undirected",diag=FALSE,weighted=TRUE)
  return(list(centrality = evcent(hr.grph_df)$vector,
              strength = graph.strength(hr.grph_df),
              degree = degree(hr.grph_df),
              ID = names(degree(hr.grph_df))))
  
}
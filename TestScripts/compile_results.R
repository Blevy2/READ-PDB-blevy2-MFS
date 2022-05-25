#this script reads in multiple individual iterations (probably saved from Steven Fiedler's ada)
#and puts them into object called result to match simulations coming from NOAA servers

result <- list()
scenario <- "IncPop_IncrTemp"
  
  
for(i in seq(100)){
  

  
  fl <- paste("GB_3species_",scenario,"_RESULTS_iter",i,".RData",sep="")
  
  result[[i]] <- readRDS(file = fl)  
  
} 



#idea: maybe stratified random mean calculations dont match well because populations
#are in the outer/small strata so that samples there are not weighted as much

#this script explores the idea by looking at the percent of the population in each strata
#during sampling weeks and compares it to the percent of cells in each stratum (which 
#represents the weighting of the given stratum)

library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)

#to rotate matrix before fields::image.plot
rotate <- function(x) t(apply(x, 2, rev))


#used in loops and to determine how many species
short_names <- c("YT","Cod","Had") 
#strata that each species occupies. Used to remove unneccesary columsn fro survey for each
strata_species <- list()
strata_species[["YT"]] <-  c(13,14,15,16,17,18,19,20,21)
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
strata_species[["Had"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)


#number of extra years in simulation. subtract this value from year in log matrix
years_cut <- 2

scenario <- "ConPop_IncTemp"

#FIRST READ IN RES WHICH CONTAINS SPATIAL OUTPUT FOR GIVEN SCENARIO

res <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Simulation_Results\\",scenario,"\\res_",scenario,".RDS",sep=""))

#read in habitat
hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution

#use survey log matrix from given scenario to find all population values in each strata of given sample week


species_logmat <- vector("list",length(short_names))
names(species_logmat) <- short_names


for(s in short_names){
  #be sure to eliminate any strata a given species does not occupy
 temp <- res$survey$log.mat[(res$survey$log.mat[,4] %in% strata_species[[s]]),] #stratum is in row 4
 #only take one sample per week  in each year
 species_logmat[[s]] <- temp[!duplicated(temp[,c("year","week")]),]
   }


#MAKE A TEMPLATE FOR EACH STRATUM. THIS WILL BE USED TO EXTRACT VALUES FROM EACH STRATUM
strat_locs <- vector("list",length(short_names))
names(strat_locs) <- short_names
for(s in short_names){
  for(strat in strata_species[[s]]){
    
    temp <- match(hab$stratas , strat)
    strat_locs[[s]][[strat]] <- matrix(temp,nc = ncol(hab$stratas))

      }
  }

#GO THEOUGH SPECIES_LOGMAT FOR EACH SPECIES AND POPULATE WITH SPECIES VALUES IN 
#EACH STRATA AND A COLUMN FOR TOTAL POPULATION

spatial_pop <- vector("list",length(short_names))
names(spatial_pop) <- short_names



for(s in short_names){ #go through each species
  
  temp <- matrix(nr =length(species_logmat[[s]][,1]), nc = (length(strata_species[[s]]) + 1) ) #temp container to store values before appending to log mat. extra cell for total pop
  spatial_pop_temp <- vector("list",52)
  

  for(i in seq(length(species_logmat[[s]][,1]))){ #go through each sample week
    

    idx <- 1
    
    for(strat in strata_species[[s]]){ #go through each stratum
    
      wk = as.numeric(species_logmat[[s]][i,"week"])
      yr = as.numeric(species_logmat[[s]][i,"year"]) - years_cut
      x = as.numeric(species_logmat[[s]][i,"x"])
      y = as.numeric(species_logmat[[s]][i,"y"])
      str = as.numeric(species_logmat[[s]][i,"stratum"])
      
    temp[i,idx] <- sum(   res$pop_bios[[(yr-1)*52 + wk]][[s]]  * strat_locs[[s]][[strat]], na.rm=T ) #results in matrix where only nonzero population are in the given strata. just sum it
     idx <- idx+1
     
     #pull out spatial population values in first week of each season
     if(wk == 13 || wk == 37){
       
       spatial_pop_temp[[wk]][[yr]] <-  res$pop_bios[[(yr-1)*52 + wk]][[s]] / sum( res$pop_bios[[(yr-1)*52 + wk]][[s]], na.rm=T)
       
     }
 
    }
  
    temp[i,idx] <- sum(temp[i,(1:(idx-1))]) #total pop
    temp[i,1:(idx-1)] <-  temp[i,1:(idx-1)] / temp[i,idx] #convert to percentage
    colnames(temp) <- c(strata_species[[s]],"Total Pop")
    
    spatial_pop[[s]] <- spatial_pop_temp 
 
  }
  View(temp)
  #append temp to species_logmat
  species_logmat[[s]] <- cbind(species_logmat[[s]],temp)
  
}



#take strata columns and turn them into rows for plotting
library(tidyr)
species_pct <- list()
for(s in short_names){
  print(s)
 species_pct[[s]]  <- as.data.frame(species_logmat[[s]]) %>%
  pivot_longer(cols = seq((length(species_logmat[[s]][1,])-length(strata_species[[s]])),(length(species_logmat[[s]][1,]))-1), values_to = "% Pop") 
}




#calculate percent of total cells for each strata
total_cells <- vector("list",length(short_names)) #total cells in each domain
for(s in short_names){
  cells <- vector()
  idx<-1
  for(c in strata_species[[s]]){
    cells[idx] <- sum(strat_locs[[s]][[c]],na.rm=T)
    idx<- idx+1
  }
  total_cells[[s]] <- sum(cells)
  
}
strata_pct <- vector("list",length(short_names))
for(s in short_names){
  for(c in strata_species[[s]]){
    strata_pct[[s]][[c]] <- sum(strat_locs[[s]][[c]],na.rm=T) / total_cells[[s]]
  }
  
}




#make table that has stratum in one column and percent of area occupied by each strata in other
#will be one for each species
#join this table with species_pct of same name using the column stratum
species_pct2 <- list()


for(s in short_names){
  temp <- data.frame()
  idx <- 1
  for(strat in strata_species[[s]]){
    
    temp[idx,1] <- as.character(strat)
    temp[idx,2] <- as.numeric(strata_pct[[s]][[strat]])
    idx <- idx+1
    
    colnames(temp, do.NULL = FALSE)
    colnames(temp) <- c("name","pct area") #label 'name' is to match the stratum column label in species_pct
    
      }
   print(temp)
  species_pct2[[s]] <- species_pct[[s]] %>%
    left_join(temp, by="name")
  View(species_pct[[s]])
}





pdf(file=paste0('testfolder/',scenario,'_Spatial_pop_plots','.pdf'))

#################################################################################
# PLOTS
#################################################################################

#PLOT SPATIAL POPULATION IN FIRST WEEK OF EACH SURVEY SEASON 2 WAYS
#1) 1X2 PLOT OF EACH FIRST WEEK (USE FOR CONSTANT TEMP SITUATION)
#AND
#2) 5X4 PLOT OF EACH FIRST WEEK (1 FOR EACH YEAR OVER ENTIRE SIMULATION). USE FOR INCREASING TEMP SITUATION

plot_wks <- c(13,37) #first week of each survey season







#FIRST, 1) 1X2 PLOT OF EACH FIRST WEEK (USE FOR CONSTANT TEMP SITUATION)

#creating new breaks for plotting
Qbreaks_list <- list()


#first for population values
for(s in short_names){
  Qbreaks2 <- list()
  for(wk in plot_wks){
                                          #just using first image to set breaks since thats what we will plot
    Qbreaks <- classInt::classIntervals(var=as.vector(spatial_pop[[s]][[wk]][[1]]), style = "fisher") 
    
    # #remove zeros from breaks
    # Qbreaks2[[wk]] <- Qbreaks[!Qbreaks[["brks"]] %in% 0]
    # Qbreaks2[[wk]] <- append(0,Qbreaks2)#put single 0 back to start
    # 
    Qbreaks_list[[s]][[wk]] <- Qbreaks
  }
}


for(s in short_names){
  par( mfrow = c(2,1), mar = c(1, 1, 1, 1))
  for(wk in plot_wks){
  
    #WITHOUT USING BREAKS
   # fields::image.plot(rotate(spatial_pop[[s]][[wk]][[1]]))
    
    #WITH USING BREAKS
    fields::image.plot(rotate(spatial_pop[[s]][[wk]][[1]]), breaks = Qbreaks_list[[s]][[wk]]$brks, nlevel = (length( Qbreaks_list[[s]][[wk]]$brks)-1) )
    text(0.5, 0.98, labels = paste( s, 'Week', wk), cex = 1)
    
  }
}



#SECOND, 2) 5X4 PLOT OF EACH FIRST WEEK (1 FOR EACH YEAR OVER ENTIRE SIMULATION). USE FOR INCREASING TEMP SITUATION

#creating new breaks for plotting
Qbreaks_list <- list()


#first for population values
for(s in short_names){
  Qbreaks2 <- list()
  for(wk in plot_wks){
    
    Qbreaks <- classInt::classIntervals(var=unlist(as.vector(spatial_pop[[s]][[wk]])), style = "fisher") 
    
    # #remove zeros from breaks
    # Qbreaks2[[wk]] <- Qbreaks[!Qbreaks[["brks"]] %in% 0]
    # Qbreaks2[[wk]] <- append(0,Qbreaks2)#put single 0 back to start
    # 
    Qbreaks_list[[s]][[wk]] <- Qbreaks
  }
}





for(s in short_names){

  for(wk in plot_wks){ 
    par( mfrow = c(5,4), mar = c(1,1,1,1))
    for(yr in seq(length(spatial_pop[[s]][[wk]]))){
    
    #WITHOUT USING BREAKS
   #fields::image.plot(rotate(spatial_pop[[s]][[wk]][[yr]]))
      
    #WITH USING BREAKS
    fields::image.plot(rotate(spatial_pop[[s]][[wk]][[yr]]), breaks = Qbreaks_list[[s]][[wk]]$brks, nlevel = (length( Qbreaks_list[[s]][[wk]]$brks)-1))
    text(0.5, 0.98, labels = paste( s, 'Week', wk, "Year", yr), cex = 1)
    
    }
        par( mfrow = c(2,1), mar = c(1,1,1,1))
    #plot first and last years for easy comparison
    fields::image.plot(rotate(spatial_pop[[s]][[wk]][[1]]), breaks = Qbreaks_list[[s]][[wk]]$brks, nlevel = (length( Qbreaks_list[[s]][[wk]]$brks)-1) )
    text(0.5, 0.98, labels = paste( s, 'Week', wk, "Year", 1), cex = 1)
    
    fields::image.plot(rotate(spatial_pop[[s]][[wk]][[20]]), breaks = Qbreaks_list[[s]][[wk]]$brks, nlevel = (length( Qbreaks_list[[s]][[wk]]$brks)-1) )
    text(0.5, 0.98, labels = paste( s, 'Week', wk, "Year", 20), cex = 1)
    

    
    
  }
}




#PLOT PERCENT OF POPULATION IN EACH STRATA IN EACH WEEK OF SURVEY 
#WITH HORIZONTAL LINE REPRESENTING PERCENT AREA COVERED BY GIVEN STRATA

library(ggplot2)
long_names <- c("Yellowtail Flounder","Cod","Haddock")
idx<-1
for(s in short_names){
p<- ggplot() +
  geom_line(data=species_pct2[[s]], aes(x=as.numeric(year), y=as.numeric(`% Pop`), group=week, color=week))+
  geom_point(data=species_pct2[[s]], aes(x=as.numeric(year), y=as.numeric(`% Pop`), group=week, color=week))+
  
  geom_hline(data = species_pct2[[s]],aes( yintercept = as.numeric(`pct area`), linetype = "")) +
  scale_linetype_manual(name = "Stratum %", values = c(2, 2)) +
                        
  facet_wrap(~ name)+
labs(x="Year",y="% Pop in Each Stratum", title = long_names[idx], color ="" )
idx<-idx+1
print(p)
}




dev.off()




























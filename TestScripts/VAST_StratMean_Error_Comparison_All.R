#THIS SCRIPT IS WAY TO GO BEYOND AIC AND MEASURE ACTUAL ERROR BETWEEN MODEL SELECTION OPTIONS AND 
#TRUE MODEL OUTPUT



##################################################################################################
#THINGS WE NEED
##################################################################################################
scenario <- "IncPop_ConTemp_8_1"

n_spp <- 3

years_sim <- 22

years_cut <- 2

#survey results without noise
list_all_temp <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\list_all_",scenario,".RDS",sep=""))

#pick specific simulation
#For YT:
#ConPop use run 1
#for Incpop run 77 shows strong increase for yellowtail
#for DecPop run 25 shows clear decrease with small values towards the end

#For Cod:
#for ConPop_ConTemp iteration 13 is pretty good
#for ConPop_IncTemp iteration 1 is pretty good
#for IncPop_ConTemp iteration 63 shows clear increase with some variation towards end
#for IncPop_IncTemp iteration 44 shows clear increase with some variation towards end
#for DecPop_ConTemp iteration 18 shows steady decline
#for DecPop_IncTemp iteration 44 shows steady decline

#For Haddock:
#for ConPop_ConTemp iteration 6 shows steady population with some small varability througout
#for ConPop_IncTemp iteration 3 shows steady population 
#IncPop_ConTemp doesnt have great options but 98 pretty good
#IncPop_IncTemp 100 is pretty good
#DecPop_ConTemp 6 is pretty good
#DecPop_IncTemp 9 is pretty good

#CHOOSE WHICH SPECIES TO USE
species <- "Had"

exclude_strata <- TRUE

#choose which simulation iteration to use based on above
good_iter <- 98

list_all <- list()
list_all[[1]] <- list_all_temp[[good_iter]]

#simulation results
memory.limit(45000)
result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\result_",scenario,".RDS",sep=""))

#1- single set of random survey locations used in stratified mean analysis
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))



###############################


#spp1 spp2 spp3
short_names <- c(species)   #fixed above

#strata that each species occupies. Used to calculate stratified random mean of each
strata_species <- list()
strata_species[["YT"]] <-  c(13,14,15,16,17,18,19,20,21)
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
strata_species[["Had"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)

##################################################################################################



#ADD TRUE MODEL POPULATION VALUES TO SURVEY DATA TABLES
#ALSO ADD LAT LON LOCATIONS TO TABLE AS WELL

library(raster)
library(sp)
library(TMB)
library(VAST)
library(dplyr)
library(ggplot2)

#read in habitat matrix
hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution

#read in GB strata
#haddock contains all stratas used
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
#plot(Had_ras)

#translate habitat matrix back into raster
hab_ras <-raster(hab$hab$spp3)
extent(hab_ras) <- extent(Had_ras)
#plot(hab_ras)

for(iter in seq(length(list_all))){
  print(iter)
  temp <- matrix(data=0,nrow=length(list_all[[iter]][,1]),ncol=n_spp) 
  lat <- vector()
  lon <- vector()
  
  for(samp in seq(length(list_all[[iter]][,1]))){
    
    #ADDING TRUE POPULATION
    x = as.numeric(list_all[[iter]][samp,2]) #x in second column
    y = as.numeric(list_all[[iter]][samp,3]) #y in third column
    wk = as.numeric(list_all[[iter]][samp,11]) #week in 11th column
    yr = as.numeric(list_all[[iter]][samp,7]) #year in 7th column
    
    temp[samp,1] <- sum(result[[good_iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp1"]],na.rm=T) #YT is spp1
    temp[samp,2] <- sum(result[[good_iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp2"]],na.rm=T) #Cod is spp2
    temp[samp,3] <- sum(result[[good_iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp3"]],na.rm=T) #Had is spp3
    
    #ADDING LAT LON LOCATIONS
    rw <- as.numeric(list_all[[iter]][samp,"x"])  #x in col 2
    cl <- as.numeric(list_all[[iter]][samp,"y"]) #y in col 3
    
    lon[samp] <- xFromCol(hab_ras, col = cl)
    lat[samp] <- yFromRow(hab_ras, row = rw)
  
  }
  temp <- cbind(temp,lat,lon)
  colnames(temp) <- c("YT","Cod","Had","Latitude","Longitude") 
  list_all[[iter]] <- cbind(list_all[[iter]],temp)
  colnames(list_all[[iter]]) <- c("station_no","x","y","stratum","day","tow","year","YT_samp","Cod_samp","Had_samp","week","Season","YT_pop","Cod_pop","Had_pop","Lat","Lon")
}

#SAVE INDIVIDUAL LIST_ALL AS THEY COME OUT SO DONT HAVE TO REDO THEM
#saveRDS(list_all,paste("list_all_more_",scenario,".RDS",sep=""))




#FIND MEAN VALUE BY SEASON USING ABOVE INFORMATION. USE MEAN OF TWO SURVEY WEEKS FOR EACH SEASON
season_wks <- list(c(13,14),c(37,38))
pop_by_season <- list()

for(iter in seq(length(list_all))){
for(s in short_names){ 
  temp <- data.frame()
  idx <- 1
  for(yr in seq(3,22)){

    for(season in seq(2)){

      
   
    temp[idx,1] <- yr
    temp[idx,2] <- season
    
    #use values in given year for weeks in specified season. only use single strata because entire population summarized in each strata in above loop
    temp[idx,3] <- mean(as.numeric(list_all[[iter]][((as.numeric(list_all[[iter]][,"year"]==yr)) & (as.numeric(list_all[[iter]][,"week"]) %in% season_wks[[season]]) & (as.numeric(list_all[[iter]][,"stratum"]==29)) ),paste(s,"_pop",sep="")]))
    
    
    idx <- idx + 1    
  }  

  }
  colnames(temp) <- c("year","season","biomass")
  pop_by_season[[s]][[iter]] <- temp
  }
}









##########################################################################################
#NOW WE NEED TO CREATE A STRATIFIED MEAN FROM EACH OF THESE SAMPLES
##########################################################################################




#choose some strata to exclude, if desired


#George's Bank Setup by species
#YT            Cod          Haddock
if(species=="YT"){
ifelse(exclude_strata==TRUE, exclude <- c(13,14,15,17,18), exclude <- c(0))
}

if(species=="Cod"){
  ifelse(exclude_strata==TRUE, exclude <- c(23,24,25), exclude <- c(0))
}

if(species=="Had"){
  ifelse(exclude_strata==TRUE, exclude <- c(23,24,25,29,30), exclude <- c(0))
}






#BELOW WILL TAKE A MINUTE



#there are #strata * #iterations * #samp_per_iter total samples

#I AM COPYING FROM CALC_SRS_INDEX_SURVEY_BENS, which was adapted from Liz's code to create below
library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)

#stop output from below using this option
options(dplyr.summarise.inform = FALSE)

#load file to calculate the stratified mean
source("TestScripts/Calc_strat_mean/fn_srs_survey_BENS.R")



#setup dimensions for each species- 1 for each strata
strat_mean_all <- vector("list",length(seq(n_spp)))

for(s in seq(n_spp)){
  
  strat_mean_all[[s]] <- vector("list",length(list_all)) 
}



#go through each strata survey, iteration, sample


for(iter in seq(length(list_all))){
  print(iter)
  
  #if any NA columns make them zero
  list_all[[iter]][is.na(list_all[[iter]])]=0
  
  
  
  # calculate SRS estimates ====
  
  for(s in seq(n_spp)){
    
    
    #DEFINE INDIVIDUAL STRATUM AREAS 
    stratum <- sort(unique(surv_random$log.mat[,4]))
    
    STRATUM_AREA <- na.omit(surv_random$cells_per_strata) # old way: rep(10000/nstrata,nstrata) #100x100 grid so each corner has area 2500
    
    sv.area <- as_tibble(data.frame(stratum,STRATUM_AREA))
    
    #remove stratum that species does not occupy
    sv.area <- sv.area[(sv.area$stratum %in% strata_species[[short_names[s]]]),]#sv.area %>% slice(-exclude)
    
    #remove strata to exclude from stratified mean calculation
    sv.area <- sv.area[!(sv.area$stratum %in% exclude[[s]]),]#sv.area %>% slice(-exclude)
    
    spp <- as_tibble(list_all[[iter]],header=T) #pull out entire survey matrix
    
    ##remove strata to exclude from stratified mean calculation
    spp <- spp[(spp$stratum %in% strata_species[[short_names[s]]]),]
    spp <- spp[!(spp$stratum %in% exclude[[s]]),]
    
    spp$year <- as.numeric(spp$year)
    
    
    # get total area of stock ====
    spp.strata <- unique(spp$stratum)
    spp.strata <- as.numeric(spp.strata)
    
    spp.area <- sum(sv.area$STRATUM_AREA[sv.area$stratum %in% spp.strata]) #TOTAL AREA OF ALL STRATA
    
    
    
    temp <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=1, sppname = paste0(short_names[s],"_samp", sep="")  )   # if strata=NULL, the function will use the unique strata set found in df
    # View(temp)
    strat_mean_all[[s]][[iter]] <- temp %>%
      mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
             CV.absolute=sd.mean.yr.absolute/mean.yr.absolute) # if strata=NULL, the function will use the unique strata set found in df
    
    strat_mean_all[[s]][[iter]] <- data.matrix(strat_mean_all[[s]][[iter]])
    
    
    
  }
  
  colnames(strat_mean_all[[s]][[iter]]) <- c("year","mean.yr","var.mean.yr","sd.mean.yr","CV","season","mean.yr.absolute","sd.mean.yr.absolute","CV.absolute")
  
  
  
  
}

#initial scenario folder
dir.create( paste0(getwd(),"/VAST/",scenario)) #create folder to store upcoming subfolders

names(strat_mean_all) <- short_names
saveRDS(strat_mean_all,paste0(getwd(),"/VAST/",scenario,"/strat_mean_all_",scenario,".RDS"))




































#load VAST fit index approximation, measure error with true value, store


#original project directory so we can switch back to it
orig.dir <- getwd()

#individual strata limits
strata.limits <- list()
strata.limits[["YT"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210)) #THESE ARE YTF STRATA
strata.limits[["Cod"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250)) #THESE ARE COD STRATA
strata.limits[["Had"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300)) #THESE ARE HAD STRATA


#initial scenario folder
setwd( paste0(orig.dir,"/VAST/",scenario,"/",species,sep="")) #create folder to store upcoming subfolders




















model_types <- c("obsmodel1","obsmodel2","obsmodel3","obsmodel4","obsmodel5","obsmodel6")

Model_settings <- list()
Model_AIC <- list()

VAST_Model_error <- list()
VAST_fit <- list()
VAST_est <- list()

FC_settings <- list()
FC_settings[["spring"]] <- data.frame(row.names = model_types)
FC_settings[["fall"]] <- data.frame(row.names = model_types)

for(folder in model_types){
  
  print(folder)
  
  for(sn in c("fall","spring")){
    
    fit <- readRDS(paste0(getwd(),"/",folder,"/",sn,"/fit_",sn,".RDS"))
    
    FC_settings[[sn]][folder,1] <- fit$settings$FieldConfig[[1]]
    FC_settings[[sn]][folder,2] <- fit$settings$FieldConfig[[2]]
    FC_settings[[sn]][folder,3] <- fit$settings$FieldConfig[[3]]
    FC_settings[[sn]][folder,4] <- fit$settings$FieldConfig[[4]]
    
    VAST_fit[[folder]][[sn]] <-  read.csv(paste0(getwd(),"/",folder,"/",sn,"/Index.csv"), header=T)
    
    ifelse(sn == "spring",
           #add year & season to these
    {Year <-  seq(years_cut+1,years_sim)
    season <- rep(1,years_sim-years_cut)},
    {Year <-  seq(years_cut+1,years_sim)
    season <- rep(2,years_sim-years_cut)})
    
    
    VAST_est[[folder]][[sn]] <- cbind(VAST_fit[[folder]][[sn]],Year,season)
    
    VF <- readRDS(paste0(getwd(),"/",folder,"/",sn,"/fit_",sn,".RDS",sep=""))
    Model_AIC[[sn]][[folder]] <- VF$parameter_estimates$AIC
    Model_settings[[sn]][[folder]] <- read.delim(paste0(getwd(),"/",folder,"/",sn,"/settings.txt",sep=""))
    
  }
  }
  

colnames(FC_settings[["spring"]]) <- c("Omega1","Epsilon1","Omega2","Epsilon2")
colnames(FC_settings[["fall"]]) <- c("Omega1","Epsilon1","Omega2","Epsilon2")
















pdf(file=paste(getwd(),"/",scenario,"_ModelSelection_ErrorPlot.pdf",sep=""))

year_min <- 2 #in case you dont want to plot all of the years

SRS_data <- list()
VAST_data <- list()
Obsmodel_plot <- list()

#plot stratified calculation and population estimate on same plot

#first make model output have 2 seasons to match the stratified mean calcs

for(iter in seq(length(list_all))){ #LIST_ALL WILL BE LENGTH 1 FROM ABOVE
  
  print(iter)





for(folder in model_types){



for(s in short_names){ #SHORT NAMES CONTAINS ONLY SINGLE SPECIES NAME
 


  

  #MODEL VALUES
  model_spring = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==1,"biomass"]
    
  model_fall = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==2,"biomass"]
 
  
  
  
# 2NORM
#   #calculate SPRING VAST error from each iteration
#   VAST_Model_error[[s]][[iter]][[folder]][["spring"]] <- norm(model_spring- VAST_est[[folder]][["spring"]][,"Estimate"] , type="2") / norm(model_spring , type ="2")
#   
#   #calculate FALL VAST error from each iteration
#   VAST_Model_error[[s]][[iter]][[folder]][["fall"]] <- norm(model_fall- VAST_est[[folder]][["fall"]][,"Estimate"] , type="2") / norm(model_fall , type ="2")
  
  
  #ABSOLUTE SUM
  #calculate SPRING VAST error from each iteration
  VAST_Model_error[[s]][[iter]][[folder]][["spring"]] <- sum(abs(model_spring- VAST_est[[folder]][["spring"]][,"Estimate"] )) / sum(abs(model_spring ))
  
  #calculate FALL VAST error from each iteration
  VAST_Model_error[[s]][[iter]][[folder]][["fall"]] <- sum(abs(model_fall- VAST_est[[folder]][["fall"]][,"Estimate"] )) / sum(abs(model_fall ))
  
  
  
  #store VAST stuff to plot later
  VAST_data[[s]][[folder]] <-  rbind(VAST_est[[folder]][["spring"]],VAST_est[[folder]][["fall"]])
  
    }


long_names <- c("Yellowtail Flounder", "Atlantic Cod", "Haddock")

  
  #NEW WAY PLOTTING 3 TOGETHER ON SAME PAGE

    #field config settings for plotting
    FC_fall = c(FC_settings$fall[folder,1],FC_settings$fall[folder,2],FC_settings$fall[folder,3],FC_settings$fall[folder,4])
    FC_spring = c(FC_settings$spring[folder,1],FC_settings$spring[folder,2],FC_settings$spring[folder,3],FC_settings$spring[folder,4])
    
    #store each obsmodel plot

    Obsmodel_plot[[folder]] <- ggplot() +

      #this way plots data by season
    geom_point(data = subset(as.data.frame(pop_by_season[[species]][[iter]]),year>=year_min), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=2) +
    geom_line(data = subset(as.data.frame(pop_by_season[[species]][[iter]]),year>=year_min), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=1) +

    #plot VAST estimate
    geom_errorbar(data=subset(VAST_data[[species]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate"),width=.3) +
    geom_point(data=subset(VAST_data[[species]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
    geom_line(data=subset(VAST_data[[species]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+

    facet_wrap(~ season) +
    labs(x="year",y="Biomass", title = paste(folder,"  Sp_err=",round(VAST_Model_error[[s]][[iter]][[folder]][["spring"]],digits=3), "  FC=", toString(FC_fall), "  Fa_err=",round(VAST_Model_error[[s]][[iter]][[folder]][["fall"]],digits=3), "  FC=", toString(FC_spring), sep=""), color ="" )





} 
      gridExtra::grid.arrange(Obsmodel_plot[[1]],Obsmodel_plot[[2]],Obsmodel_plot[[3]],nrow=3)
      gridExtra::grid.arrange(Obsmodel_plot[[4]],Obsmodel_plot[[5]],Obsmodel_plot[[6]],nrow=3)
  
}

dev.off()


































































BELOW HAS NOT YET BEEN UPDATED TO BE GENERALIZED FOR ALL SPECIES. ONLY APPLIES TO YTF



##########################################################################################
#Next plot scatterplot of errors
##########################################################################################

#first create data from for each

#1- stratified mean data
df_SRS_spring <- tibble(iter = rep(1:length(list_all),n_spp),
                  error = c(SRS_error_spring[[1]], SRS_error_spring[[2]], SRS_error_spring[[3]]),
                  species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                  
                  season = rep(rep("spring",length(list_all)),n_spp),
                  Model = rep(rep("Strat. Mean",length(list_all)),n_spp),
)

df_SRS_fall <- tibble(iter = rep(1:length(list_all),n_spp),
             error = c(SRS_error_fall[[1]], SRS_error_fall[[2]], SRS_error_fall[[3]]),
             species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
             
             season = rep(rep("fall",length(list_all)),n_spp),
             Model = rep(rep("Strat. Mean",length(list_all)),n_spp),
             )

df_SRS <- rbind(as.data.frame(df_SRS_fall),as.data.frame(df_SRS_spring))


#create data frame containing mean values for each group
means_SRS <- ddply(df_SRS, .(species,season), summarise, mean = mean(as.numeric(unlist(error)),na.rm=T), Model = "Strat. Mean")



#2- VAST data
df_VAST_spring <- tibble(iter = rep(1:length(list_all),n_spp),
                        error = c(VAST_error_spring[[1]], VAST_error_spring[[2]], VAST_error_spring[[3]]),
                        species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                        
                        season = rep(rep("spring",length(list_all)),n_spp),
                        Model = rep(rep("VAST",length(list_all)),n_spp),
)

df_VAST_fall <- tibble(iter = rep(1:length(list_all),n_spp),
                      error = c(VAST_error_fall[[1]], VAST_error_fall[[2]], VAST_error_fall[[3]]),
                      species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                      
                      season = rep(rep("fall",length(list_all)),n_spp),
                      Model = rep(rep("VAST",length(list_all)),n_spp),
)

df_VAST <- rbind(df_VAST_fall,df_VAST_spring)

#create data frame containing mean values for each group
means_VAST <- ddply(df_VAST, .(species,season), summarise, mean = mean(as.numeric(unlist(error))), Model = "VAST")


#combine both of previous data into single object for plotting

df <- rbind(df_VAST,df_SRS)
means <- rbind(means_SRS,means_VAST)

#Error scatterplots

#1) to plot a single scenario, run just cc below and print(cc)
#2) to plot two scenarios on top of each other, store the first as cc and then run code below doing cc +

library(ggplot2)

# #SRS scatterplot alone
# SRS_scat <-ggplot(data=df_SRS,
#        aes(x=iter,y=as.numeric(error),color=Model)) +
#   geom_point()+
#   ylim(0,1)+
#   facet_grid(season ~ species)+
# geom_hline(aes(yintercept = mean, color = Model), data = means_SRS) 
# 
# print(SRS_scat)
# 
# #VAST scatterplot alone
# VAST_scat <-ggplot(data=df_VAST,
#                   aes(x=iter,y=as.numeric(error),color=Model)) +
#   geom_point()+
#   ylim(0,1)+
#   facet_grid(season ~ species)+
#   geom_hline(aes(yintercept = mean, color = Model), data = means_VAST) 
# 
# print(VAST_scat)


#both scatterplots together
both_scat <-ggplot(data=df,
                   aes(x=iter,y=as.numeric(unlist(error)),color=Model)) +
  geom_point()+
  ylim(0,1)+
  facet_grid(season ~ species)+
  geom_hline(aes(yintercept = mean, color = Model), data = means) 

print(both_scat)


# 
# ggsave(filename = paste("Results/GB_error_plots/Individussssal_SRS_",scenario,".pdf",sep=""),
#        plot = last_plot())
# 



#run this second to plot on top of each other
cc+  geom_point(data=df,color="red",
             aes(x=iter,y=as.numeric(error)))+
   facet_grid(season ~ species) +
  geom_hline(aes(yintercept = mean), data = means, color = "red")





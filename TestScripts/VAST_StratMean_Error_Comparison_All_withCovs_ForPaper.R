#THIS SCRIPT IS WAY TO GO BEYOND AIC AND MEASURE ACTUAL ERROR BETWEEN MODEL SELECTION OPTIONS AND 
#TRUE MODEL OUTPUT

#original project directory so we can switch back to it
orig.dir <- getwd()



setwd(orig.dir)
##################################################################################################
#THINGS WE NEED
##################################################################################################
scenario1 <- "ConPop_ConTemp" #the folder name

#spp1 spp2 spp3
#short_names <- c("YT","Cod","Had")   #fixed above
short_names <- c("YT")#,"Had")

exclude_strata <- FALSE

#for getting into correct subfolder
ifelse(exclude_strata==TRUE, 
       {str_dir <- "ExcludeStrata"},
       {str_dir <- "AllStrata"})

###################################################################################################

n_spp <- 3

years_sim <- 22

years_cut <- 2

#survey results without noise
list_all_temp <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Simulation_Results\\",scenario1,"\\list_all_",scenario1,".RDS",sep=""))

#simulation results
#memory.limit(45000)
#result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario1,"\\result_",scenario1,".RDS",sep=""))

#load existing result_goodones, if it exists
result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Simulation_Results\\",scenario1,"\\result_goodones_",scenario1,".RDS",sep=""))

#1- single set of random survey locations used in stratified mean analysis
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Simulation_Results\\",scenario1,"\\surv_random_",scenario1,".RDS",sep=""))

#pick specific simulation
#For YT:
#for Conpop_ConTemp (also used for IncTemp) run 1 shows decent constant value with single spike early on
#for Incpop_ConTemp (also used for IncTemp) run 77 shows strong increase for yellowtail
#for DecPop_ConTemp run 25 shows clear decrease with small values towards the end
#for DecPop_IncTemp run 13 shows clear decrease with small values towards the end

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


#cov_used <- "_WithCov" #dont need this anymore

#old way
#ifelse(covariates==TRUE,{cov_dir <- paste("_with_",cov_used,sep="")},{cov_dir <- ""})


scenario <- paste("ForPaper/",scenario1,sep="")



#choose which simulation iteration to use based on above
if(scenario1=="ConPop_ConTemp"){good_iter <- c(1,13,6)}
if(scenario1=="ConPop_IncTemp"){good_iter <- c(1,1,3)}
if(scenario1=="IncPop_ConTemp"){good_iter <- c(77,63,98)}
if(scenario1=="IncPop_IncTemp"){good_iter <- c(77,44,100)}
if(scenario1=="DecPop_ConTemp"){good_iter <- c(25,18,6)}
if(scenario1=="DecPop_IncTemp"){good_iter <- c(13,44,9)}


list_all <- list()
list_all[["YT"]] <- list_all_temp[[good_iter[1]]]
list_all[["Cod"]] <- list_all_temp[[good_iter[2]]]
list_all[["Had"]] <- list_all_temp[[good_iter[3]]]




#############################################################################################
# #save the list_all with the correct surveys from good_iter
# result_goodones <- list()
# result_goodones[[good_iter[[1]]]] <- result[[good_iter[[1]]]]
# result_goodones[[good_iter[[2]]]] <- result[[good_iter[[2]]]]
# result_goodones[[good_iter[[3]]]] <- result[[good_iter[[3]]]]
# saveRDS(result_goodones,file = paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario1,"\\result_goodones_",scenario1,".RDS",sep=""))

#############################################################################################

#############################################################################################


#strata that each species occupies. Used to calculate stratified random mean of each
strata_species <- list()
strata_species[["YT"]] <-  c(13,14,15,16,17,18,19,20,21)
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
strata_species[["Had"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)

##################################################################################################

setwd(orig.dir)

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
    
    temp[samp,1] <- sum(result[[good_iter[iter]]]$pop_bios[[(wk+(52*(yr-1)))]][["spp1"]],na.rm=T) #YT is spp1
    temp[samp,2] <- sum(result[[good_iter[iter]]]$pop_bios[[(wk+(52*(yr-1)))]][["spp2"]],na.rm=T) #Cod is spp2
    temp[samp,3] <- sum(result[[good_iter[iter]]]$pop_bios[[(wk+(52*(yr-1)))]][["spp3"]],na.rm=T) #Had is spp3
    
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
    
    temp[idx,4] <- 1 #Est_ratio for plotting later
    
    idx <- idx + 1    
  }  

  }
  colnames(temp) <- c("year","season","biomass","Est_ratio")
  pop_by_season[[s]][[iter]] <- temp
  }
}

pop_by_season[["YT"]] <- pop_by_season[["YT"]][[1]]  #YT should be first in list_all
pop_by_season[["Cod"]] <- pop_by_season[["Cod"]][[2]] #Cod should be second in list_all
pop_by_season[["Had"]] <- pop_by_season[["Had"]][[3]] #Had should be third in list_all







##########################################################################################
#NOW WE NEED TO CREATE A STRATIFIED MEAN FROM EACH OF THESE SAMPLES (WITH AND WITHOUT NOISE)
##########################################################################################




#choose some strata to exclude, if desired
#George's Bank Setup by species
#YT            Cod          Haddock

exclude <- list()

ifelse(exclude_strata==TRUE, 
       {exclude[["YT"]] <- c(13,14,15,17,18)
       exclude[["Cod"]] <- c(23,24,25)
       exclude[["Had"]] <- c(23,24,25,29,30)}, 
       {exclude[["YT"]] <- c(0)
       exclude[["Cod"]] <- c(0)
       exclude[["Had"]] <- c(0)})

setwd(orig.dir)


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

for(s in short_names){
  
  strat_mean_all[[s]] <- vector("list",length(list_all)) 
}


#go through each strata survey, iteration, sample
  
  # calculate SRS estimates ====
   for(noise in c("NoNoise_", "WithNoise_")){

for(iter in seq(length(list_all))){
  print(iter)
  
  #if any NA columns make them zero
  list_all[[iter]][is.na(list_all[[iter]])]=0
  
  

     
     
  for(s in short_names){
    
   
    
    
    #DEFINE INDIVIDUAL STRATUM AREAS 
    stratum <- sort(unique(surv_random$log.mat[,4]))
    
    STRATUM_AREA <- na.omit(surv_random$cells_per_strata) # old way: rep(10000/nstrata,nstrata) #100x100 grid so each corner has area 2500
    
    sv.area <- as_tibble(data.frame(stratum,STRATUM_AREA))
    
    #remove stratum that species does not occupy
    sv.area <- sv.area[(sv.area$stratum %in% strata_species[[s]]),]#sv.area %>% slice(-exclude)
    
    #remove strata to exclude from stratified mean calculation
    sv.area <- sv.area[!(sv.area$stratum %in% exclude[[s]]),]#sv.area %>% slice(-exclude)
    
    spp <- as_tibble(list_all[[iter]],header=T) #pull out entire survey matrix
    

    
    ##remove strata to exclude from stratified mean calculation
    spp <- spp[(spp$stratum %in% strata_species[[s]]),]
    spp <- spp[!(spp$stratum %in% exclude[[s]]),]
    
    spp$year <- as.numeric(spp$year)
    
        #add noise to survey and overwrite existing noise, if it exists
    if(noise=="WithNoise_"){
      print("ADDING NOISE TO DATA")
      
      #add noise to correct column
      #YTF
      if(s=="YT"){ temp_noise <-  sapply(as.numeric(spp$YT_samp) , function(x){rlnorm(1,mean=log(x),sdlog=.35)} ) }
      #Cod
      if(s=="Cod"){ temp_noise <-  sapply(as.numeric(spp$Cod_samp) , function(x){rlnorm(1,mean=log(x),sdlog=.35)} ) }
      #Had
      if(s=="Had"){temp_noise <-  sapply(as.numeric(spp$Had_samp) , function(x){rlnorm(1,mean=log(x),sdlog=.35)} ) }
   
        #this didnt work because it was still a data table
      #temp_noise <-  sapply(as.vector(spp$c(paste(short_names[s],"_samp",sep=""))) , function(x){rlnorm(1,mean=log(x),sdlog=.35)} ) 
      
      
      
      try(temp_noise <- readRDS(paste0(getwd(),"/VAST/",scenario,"/",s,"/",str_dir,"_NoCovs_",noise,"/adios.RDS",sep="")) )
      
      #if try worked above it will be a data table, if not it will be a vector
      #YTF
      if(s=="YT"){ ifelse(is.vector(temp_noise),spp$YT_samp <- temp_noise, spp$YT_samp <- temp_noise$YTF) }
      #Cod
      if(s=="Cod"){ ifelse(is.vector(temp_noise),spp$Cod_samp <- temp_noise, spp$Cod_samp <- temp_noise$Cod) }
      #Had
      if(s=="Had"){ ifelse(is.vector(temp_noise),spp$Had_samp <- temp_noise, spp$Had_samp <- temp_noise$Had) }
      
      
    }
    # get total area of stock ====
    spp.strata <- unique(spp$stratum)
    spp.strata <- as.numeric(spp.strata)
    
    spp.area <- sum(sv.area$STRATUM_AREA[sv.area$stratum %in% spp.strata]) #TOTAL AREA OF ALL STRATA
    
    
    
    temp <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=1, sppname = paste0(s,"_samp", sep="")  )   # if strata=NULL, the function will use the unique strata set found in df
    # View(temp)
    strat_mean_all[[s]][[noise]][[iter]] <- temp %>%
      mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
             CV.absolute=sd.mean.yr.absolute/mean.yr.absolute) # if strata=NULL, the function will use the unique strata set found in df
    
    strat_mean_all[[s]][[noise]][[iter]] <- data.matrix(strat_mean_all[[s]][[noise]][[iter]])
    
    
    
  
  
  colnames(strat_mean_all[[s]][[noise]][[iter]]) <- c("year","mean.yr","var.mean.yr","sd.mean.yr","CV","season","mean.yr.absolute","sd.mean.yr.absolute","CV.absolute")
  
  }
     
  }
  
 
strat_mean_all[["YT"]][[noise]] <- strat_mean_all[["YT"]][[noise]][[1]]  #YT should be first in list_all
strat_mean_all[["Cod"]][[noise]] <- strat_mean_all[["Cod"]][[noise]][[2]] #Cod should be second in list_all
strat_mean_all[["Had"]][[noise]] <- strat_mean_all[["Had"]][[noise]][[3]] #Had should be third in list_all
 
  
}

#initial scenario folder
dir.create( paste0(getwd(),"/VAST/",scenario)) #create folder to store upcoming subfolders

ifelse(exclude_strata==TRUE, strat_ex <- "excludestrata", strat_ex <- "allstrata")

#saveRDS(strat_mean_all,paste0(getwd(),"/VAST/",scenario,"/strat_mean_all_",scenario1,"_",strat_ex,".RDS"))














#load VAST fit index approximation, measure error with true value, store


#individual strata limits
# strata.limits <- list()
# strata.limits[["YT"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210)) #THESE ARE YTF STRATA
# strata.limits[["Cod"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250)) #THESE ARE COD STRATA
# strata.limits[["Had"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300)) #THESE ARE HAD STRATA

#What I tested for all of them, but they dont always work in each scenario
model_types <- list()
model_types[["YT"]] <- c("obsmodel7")
model_types[["Cod"]] <- c("obsmodel7")
model_types[["Had"]] <- c("obsmodel7")
#model_types[["YT"]] <- c("obsmodel5","obsmodel6")
#model_types[["Cod"]] <- c("obsmodel1","obsmodel2","obsmodel5","obsmodel6")
#model_types[["Had"]] <- c("obsmodel1","obsmodel2","obsmodel5","obsmodel6")


#Use these for IncPop_IncTemp
# model_types <- list()
# model_types[["YT"]] <- c("obsmodel5","obsmodel6")
# model_types[["Cod"]] <- c("obsmodel2","obsmodel5") #,"obsmodel6" didnt work
# model_types[["Had"]] <- c("obsmodel1")



# ifelse(exclude_strata==TRUE,
# {model_types[["YT"]] <- c("obsmodel5","obsmodel6")
# model_types[["Cod"]] <- c("obsmodel1","obsmodel2","obsmodel5") #,"obsmodel6" didnt work
# model_types[["Had"]] <- c("obsmodel1","obsmodel2","obsmodel5","obsmodel6")},
# {model_types[["YT"]] <- c("obsmodel1","obsmodel2","obsmodel3","obsmodel4","obsmodel5","obsmodel6")
# model_types[["Cod"]] <- c("obsmodel1","obsmodel2","obsmodel3","obsmodel4","obsmodel5","obsmodel6")
# model_types[["Had"]] <- c("obsmodel1","obsmodel2","obsmodel3","obsmodel4","obsmodel5","obsmodel6")}
# )

Model_settings <- list()
Model_AIC <- list()

SRS_data_all <- list()
SRS_data1 <- list()

VAST_Model_error <- list()
SRS_Model_error <- list()
VAST_fit <- list()
VAST_est <- list()


# #old fc settings
# FC_settings[["YT"]][[cov_direct]][["spring"]] <- data.frame(row.names = model_types[["YT"]])
# FC_settings[["YT"]][[cov_direct]][["fall"]] <- data.frame(row.names = model_types[["YT"]])
# FC_settings[["Cod"]][[cov_direct]][["spring"]] <- data.frame(row.names = model_types[["Cod"]])
# FC_settings[["Cod"]][[cov_direct]][["fall"]] <- data.frame(row.names = model_types[["Cod"]])
# FC_settings[["Had"]][[cov_direct]][["spring"]] <- data.frame(row.names = model_types[["Had"]])
# FC_settings[["Had"]][[cov_direct]][["fall"]] <- data.frame(row.names = model_types[["Had"]])


# FC_settings <- list(vector("list", length(short_names)))
# names(FC_settings) <- short_names
FC_settings <-list()
FC_settings_DF <- list()#list of data frames for viewing all settings at once


#initial scenario folder
setwd( paste0(orig.dir,"/VAST/",scenario,sep="")) #create folder to store upcoming subfolders

for(s in short_names){
  
    
    FC_settings[[s]] <- vector("list", length= 2) #with and without covariates
    names(FC_settings[[s]]) <- c("_NoCovs_","_WithCov_")
    
    FC_settings_DF[[s]] <- data.frame()
    FC_idx <- 1
  
for(folder in model_types[[s]]){
  
  print(folder)
  
  #old way for pre-paper vast runs
  #for(cov_directory in c("", paste("_with_",cov_used,sep=""))){
    
    #ifelse(cov_directory==c(""), cov_direct<-"No_Cov", cov_direct <- cov_directory)
    
    #new way with vast runs for paper
  for(cov_directory in c("_NoCovs_", "_WithCov_")){
    
    for(noise in c("NoNoise_", "WithNoise_")){
    
    
    cov_direct<- cov_directory
    
    print(cov_direct)  

    FC_settings[[s]][[cov_direct]][[noise]][["spring"]] <- data.frame(row.names = model_types[[s]])
    FC_settings[[s]][[cov_direct]][[noise]][["fall"]] <- data.frame(row.names = model_types[[s]])
    
    
  for(sn in c("spring","fall")){
    
    try(fit <- readRDS(paste0(getwd(),"/",s,"/",str_dir,cov_directory,noise,"/",folder,"/",sn,"/fit_",sn,".RDS")),silent=TRUE)
    
    #set FC settings as N and then override them, if possible
    FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,1] <- "N"
    FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,2] <- "N"
    FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,3] <- "N"
    FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,4] <- "N"
    try(FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,1] <- fit$settings$FieldConfig[[1]],silent=TRUE)
    try( FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,2] <- fit$settings$FieldConfig[[2]],silent=TRUE)
    try( FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,3] <- fit$settings$FieldConfig[[3]],silent=TRUE)
    try(FC_settings[[s]][[cov_direct]][[noise]][[sn]][folder,4] <- fit$settings$FieldConfig[[4]],silent=TRUE)

    vals <- c(str_dir, cov_direct,noise,sn,folder,fit$settings$FieldConfig[[1]],fit$settings$FieldConfig[[2]],fit$settings$FieldConfig[[3]],fit$settings$FieldConfig[[4]])
    FC_settings_DF[[s]][FC_idx,1:length(vals)] <- vals
    FC_idx <- FC_idx+1
    
    try(VAST_fit[[s]][[cov_direct]][[noise]][[folder]][[sn]] <-  read.csv(paste0(getwd(),"/",s,"/",str_dir,cov_directory,noise,"/",folder,"/",sn,"/Index.csv"), header=T),silent=TRUE)
    
    #pull out strat mean calc
    SRS_data_all[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- strat_mean_all[[s]][[noise]][strat_mean_all[[s]][[noise]][,"season"]==1,]
    SRS_data_all[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- strat_mean_all[[s]][[noise]][strat_mean_all[[s]][[noise]][,"season"]==2,]
    
    
    ifelse(sn == "spring",
           #add year & season to these
    {Year <-  seq(years_cut+1,years_sim)
    season <- rep(1,years_sim-years_cut)},
    {Year <-  seq(years_cut+1,years_sim)
    season <- rep(2,years_sim-years_cut)})
    
    
   try(VAST_est[[s]][[cov_direct]][[noise]][[folder]][[sn]] <- cbind(VAST_fit[[s]][[cov_direct]][[noise]][[folder]][[sn]],Year,season),silent=TRUE)
    SRS_data1[[s]][[cov_direct]][[noise]][[folder]][[sn]] <- SRS_data_all[[s]][[cov_direct]][[noise]][[folder]][[sn]][,c("mean.yr.absolute","year","season","sd.mean.yr.absolute")]
    
    try(Model_AIC[[s]][[cov_direct]][[noise]][[sn]][[folder]] <- fit$parameter_estimates$AIC,silent=TRUE)
    try(Model_settings[[s]][[cov_direct]][[noise]][[sn]][[folder]] <- read.delim(paste0(getwd(),"/",s,"/",str_dir,cov_directory,noise,"/",folder,"/",sn,"/settings.txt",sep="")),silent=TRUE)
    
   try(remove(fit),silent=TRUE)
    
  }
  }
  }
  }
  colnames(FC_settings[[s]][[cov_direct]][[noise]][["spring"]]) <- c("Omega1","Epsilon1","Omega2","Epsilon2")
  colnames(FC_settings[[s]][[cov_direct]][[noise]][["fall"]]) <- c("Omega1","Epsilon1","Omega2","Epsilon2")
  }

#write.csv(FC_settings_DF,file = paste(getwd(),"/",scenario1,"_",str_dir,"_FC_settings.csv",sep=""))













#All (potentially) species

pdf(file=paste(getwd(),"/",scenario1,"_",str_dir,"_new.pdf",sep=""))

#Just YT
#pdf(file=paste(getwd(),"/YT/",scenario1,"_",str_dir,"_YT.pdf",sep=""))



year_min <- 2 #in case you dont want to plot all of the years

VAST_data <- list()
SRS_data <- list()
Obsmodel_plot <- list()
Obsmodel_plot_log <- list()

Est_ratio <- list()
SRS_ratio <- list()
Est_ratio_plot <- list()
Est_ratio_plot_log <- list()

Ratio_summary_info <- matrix(nrow=8*length(short_names),ncol=13)
ratio_idx <- 1
#colnames(Ratio_summary_info) <- c("Scenario","Estimate", "N_Spring >1", "N_Fall >1","Pct_Spring >1", "Pct_Fall >1", "Total>1", "Pct_Total>1", "Mean_Spring", "Mean_Fall", "Mean_overall","Mean_overall_finite", "SD_overall")



#plot stratified calculation and population estimate on same plot

#first make model output have 2 seasons to match the stratified mean calcs

for(s in short_names){ #LIST_ALL WILL BE LENGTH 3 FROM ABOVE
  
  
for(folder in model_types[[s]]){



  #MODEL VALUES
  model_spring = pop_by_season[[s]][pop_by_season[[s]]$season==1,"biomass"]
    
  model_fall = pop_by_season[[s]][pop_by_season[[s]]$season==2,"biomass"]
 
 
  
  for(cov_directory in c("_NoCovs_", "_WithCov_")){
    
    for(noise in c("NoNoise_", "WithNoise_")){
      
    cov_direct<- cov_directory
  
# 2NORM
#   #calculate SPRING VAST error from each iteration
#   VAST_Model_error[[s]][[folder]][["spring"]] <- norm(model_spring- VAST_est[[s]][[folder]][["spring"]][,"Estimate"] , type="2") / norm(model_spring , type ="2")
#   
#   #calculate FALL VAST error from each iteration
#   VAST_Model_error[[s]][[folder]][["fall"]] <- norm(model_fall- VAST_est[[s]][[folder]][["fall"]][,"Estimate"] , type="2") / norm(model_fall , type ="2")
  
  print(s)
  print(folder)
  print(cov_direct)
  print(noise)
  

  #ABSOLUTE SUM
  #calculate SPRING VAST error from each iteration
  #set as 99 and then override if possible
  VAST_Model_error[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- 99
  try(VAST_Model_error[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- sum(abs(model_spring- VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]][,"Estimate"] )) / sum(abs(model_spring )),silent=TRUE)
  VAST_Model_error[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- 99
  #calculate FALL VAST error from each iteration
  try(VAST_Model_error[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- sum(abs(model_fall- VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]][,"Estimate"] )) / sum(abs(model_fall )),silent=TRUE)

  #calculate ratio of estimate vs true value
  Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- 99  
  try(Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]][,"Estimate"]/model_spring,silent=TRUE)
  Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- 99  
  try(Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]][,"Estimate"]/model_fall,silent=TRUE)
  
  oldnames = names(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]])
  #add Est_ratio to VAST_est to plot later
  VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- cbind(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]],Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["spring"]])
  names(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]]) <- c(oldnames,"Est_ratio") 
  
  oldnames = names(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]])
  VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- cbind(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]],Est_ratio[[s]][[cov_direct]][[noise]][[folder]][["fall"]])
  names(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]]) <- c(oldnames,"Est_ratio") 
  
  #calculate SPRING SRS error from each iteration
  SRS_Model_error[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- sum(abs(model_spring- SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]][,"mean.yr.absolute"] )) / sum(abs(model_spring ))
  
  #calculate FALL SRS error from each iteration
  SRS_Model_error[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- sum(abs(model_fall- SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]][,"mean.yr.absolute"] )) / sum(abs(model_fall ))
  
  
  #store VAST stuff to plot later
  #first load a blank version and override if possible
  VAST_data[[s]][[cov_direct]][[noise]][[folder]] <- readRDS(file = paste0(orig.dir,"/VAST/zero_VAST_est.RDS",sep="") )
 
  #for decpop_contemp
    #if(((!((s=="Cod"&folder=="obsmodel6")|(s=="Had"&folder=="obsmodel6"))))){ try(VAST_data[[s]][[folder]] <-  rbind(VAST_est[[s]][[folder]][["spring"]],VAST_est[[s]][[folder]][["fall"]]),silent=TRUE)}
  
  #if vast estimate for each season, use them
  if((length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]][1,])>2)&(length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]][1,])>2)){VAST_data[[s]][[cov_direct]][[noise]][[folder]] <-  rbind(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]],VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]])}
  #if missing one, use zeros in one and the estimate for the other
  if((length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]][1,])==2)&(length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]][1,])>2)){VAST_data[[s]][[cov_direct]][[noise]][[folder]]<-  rbind(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]],VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2,] )}
  if((length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]][1,])>2)&(length(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["spring"]][1,])==2)){VAST_data[[s]][[cov_direct]][[noise]][[folder]]<-  rbind(VAST_est[[s]][[cov_direct]][[noise]][[folder]][["fall"]],VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1,] )}
  
  
  #calculate SRS ratio and add to data
  SRS_ratio[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]][,"mean.yr.absolute"]/model_fall
  SRS_ratio[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]][,"mean.yr.absolute"]/model_spring
  
  namesorig=colnames(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]])
  SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]] <- cbind(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]],SRS_ratio[[s]][[cov_direct]][[noise]][[folder]][["spring"]])
  colnames(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]]) <- c(namesorig,"Est_ratio")
  
  namesorig=colnames(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]])
  SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]] <- cbind(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]],SRS_ratio[[s]][[cov_direct]][[noise]][[folder]][["fall"]])
  colnames(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]]) <- c(namesorig,"Est_ratio")
  
  
  SRS_data[[s]][[cov_direct]][[noise]][[folder]] <- rbind(SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["spring"]],SRS_data1[[s]][[cov_direct]][[noise]][[folder]][["fall"]])
    
  
  
  
  
  
  ######################################################
  # GO THROUGH SRS_DATA AND VAST_DATA IN CURRENT FOLDER TO
  # 1) COUNT NUMBER OF OVER/UNDER ESTIMATE
  # 2) CALCULATE AVERAGE OVER/UNDER ESTIMATE
  ######################################################

  #colnames(Ratio_summary_info) <- c("Scenario","Estimate", "N_Spring >1", "N_Fall >1","Pct_Spring >1", "Pct_Fall >1", "Total>1", "Pct_Total>1", "Mean_Spring", "Mean_Fall", "Mean_overall", "Mean_overall_finite", "SD_overall")
 
   Ratio_summary_info[ratio_idx,1] <-  paste(s,"_",cov_direct,"_",noise,"_",folder,"_",sep="")
   Ratio_summary_info[ratio_idx,2] <-  "Strat Mean"
   Ratio_summary_info[ratio_idx,3] <-  as.numeric(length(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 & SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,1]))  #as.numeric(nrow(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 & SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,]))  
   Ratio_summary_info[ratio_idx,4] <-  as.numeric(length(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 & SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,1]))  #as.numeric(nrow(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 & SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,]))  
   Ratio_summary_info[ratio_idx,5] <-  as.numeric(Ratio_summary_info[ratio_idx,3])/20
   Ratio_summary_info[ratio_idx,6] <-  as.numeric(Ratio_summary_info[ratio_idx,4])/20
   Ratio_summary_info[ratio_idx,7] <-  as.numeric(Ratio_summary_info[ratio_idx,3])+as.numeric(Ratio_summary_info[ratio_idx,4])
   Ratio_summary_info[ratio_idx,8] <-  as.numeric(Ratio_summary_info[ratio_idx,7])/40
   Ratio_summary_info[ratio_idx,9] <-  mean(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 ,"Est_ratio"])
   Ratio_summary_info[ratio_idx,10] <- mean(SRS_data[[s]][[cov_direct]][[noise]][[folder]][SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 ,"Est_ratio"])
   Ratio_summary_info[ratio_idx,11] <- mean(SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])
   Ratio_summary_info[ratio_idx,12] <- mean(SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"][is.finite(SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])])
   Ratio_summary_info[ratio_idx,13] <- sd(SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"][is.finite(SRS_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])])
   
   Ratio_summary_info[ratio_idx+1,1] <-  paste(s,"_",cov_direct,"_",noise,"_",folder,"_",sep="")
   Ratio_summary_info[ratio_idx+1,2] <-  "VAST"
   Ratio_summary_info[ratio_idx+1,3] <-  as.numeric(length(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 & VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,1]))  #as.numeric(nrow(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 & VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,]))  
   Ratio_summary_info[ratio_idx+1,4] <-  as.numeric(length(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 & VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,1])) #as.numeric(nrow(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 & VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"]>1,]))  
   Ratio_summary_info[ratio_idx+1,5] <-  as.numeric(Ratio_summary_info[ratio_idx+1,3])/20
   Ratio_summary_info[ratio_idx+1,6] <-  as.numeric(Ratio_summary_info[ratio_idx+1,4])/20
   Ratio_summary_info[ratio_idx+1,7] <-  as.numeric(Ratio_summary_info[ratio_idx+1,3])+as.numeric(Ratio_summary_info[ratio_idx+1,4])
   Ratio_summary_info[ratio_idx+1,8] <-  as.numeric(Ratio_summary_info[ratio_idx+1,7])/40
   Ratio_summary_info[ratio_idx+1,9] <-  mean(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==1 ,"Est_ratio"])
   Ratio_summary_info[ratio_idx+1,10] <- mean(VAST_data[[s]][[cov_direct]][[noise]][[folder]][VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"season"]==2 ,"Est_ratio"])
   Ratio_summary_info[ratio_idx+1,11] <- mean(VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])
   Ratio_summary_info[ratio_idx+1,12] <- mean(VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"][is.finite(VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])])
   Ratio_summary_info[ratio_idx+1,13] <- sd(VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"][is.finite(VAST_data[[s]][[cov_direct]][[noise]][[folder]][,"Est_ratio"])])
   
        ratio_idx <- ratio_idx + 2
  
  
  
  
  
  
  
}
}
  
long_names <- c("Yellowtail Flounder", "Atlantic Cod", "Haddock")

  
  #NEW WAY PLOTTING 3 TOGETHER ON SAME PAGE

    # #field config settings for plotting
    # FC_fall = c(FC_settings[[s]]$fall[folder,1],FC_settings[[s]]$fall[folder,2],FC_settings[[s]]$fall[folder,3],FC_settings[[s]]$fall[folder,4])
    # FC_spring = c(FC_settings[[s]]$spring[folder,1],FC_settings[[s]]$spring[folder,2],FC_settings[[s]]$spring[folder,3],FC_settings[[s]]$spring[folder,4])
    # 
    #store each obsmodel plot

    Obsmodel_plot[[s]][[cov_direct]][[noise]][[folder]] <- ggplot() +

      #this way plots data by season
    geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=3) +
    geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=1) +

    #plot VAST estimate without covariates with NO noise
    geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov No Noise"),width=.3) +
    #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
    geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"),size=2)+
    geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"))+
      
      #plot VAST estimate without covariates with WITH noise
      geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov W Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"),size=2)+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"))+

      #plot VAST estimate with covariates with NO noise
      geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov No Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"))+
      
      #plot VAST estimate with covariates WITH noise
      geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov w Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
      
      
    #plot stratified calculation data with NO noise
    geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean No Noise"),width=.3) +
   # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
    geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean No Noise"))+
    geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean No Noise"))+
      
      #plot stratified calculation data with NO noise
      geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean W Noise"),width=.3) +
      # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
      
      
    facet_wrap(~ season, ncol =1) +
    # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
    #                                          "  FC=", toString(FC_spring), 
    #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
    #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
    #                                          "  FC=", toString(FC_fall),
    #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )

      labs(x="year",y="Biomass", title = paste(paste(s," ",folder,sep=""), "\n", #new line
                                               paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                                     "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
                                                     " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                               "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
                                               "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                               "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),sep=""), "\n", #new line
                                               paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
                                                     " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
                                                     "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),sep=""),sep=""), color ="" )+
      
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            title=element_text(size=8))

#one plot per page
    print(Obsmodel_plot[[s]][[cov_direct]][[noise]][[folder]])
    
    
    
    
    
    #NEW WAY PLOTTING 3 TOGETHER ON SAME PAGE  ON LOGARITHMIC SCALE
    
    # #field config settings for plotting
    # FC_fall = c(FC_settings[[s]]$fall[folder,1],FC_settings[[s]]$fall[folder,2],FC_settings[[s]]$fall[folder,3],FC_settings[[s]]$fall[folder,4])
    # FC_spring = c(FC_settings[[s]]$spring[folder,1],FC_settings[[s]]$spring[folder,2],FC_settings[[s]]$spring[folder,3],FC_settings[[s]]$spring[folder,4])
    # 
    #store each obsmodel plot
    
    Obsmodel_plot_log[[s]][[cov_direct]][[noise]][[folder]] <- ggplot() +
      
      #this way plots data by season
      geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=log(biomass), group = season, color = "Model"),size=3) +
      geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=log(biomass), group =season, color = "Model"),size=1) +
      
      #plot VAST estimate without covariates with NO noise
      geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season,ymin=log(Estimate)-(1.96*Std..Error.for.ln.Estimate.), ymax=log(Estimate)+(1.96*Std..Error.for.ln.Estimate.), color = "VAST No Cov No Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.ln.Estimate.), ymax=Estimate+(1.96*Std..Error.for.ln.Estimate.), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST No Cov No Noise"),size=2)+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST No Cov No Noise"))+
      
      #plot VAST estimate without covariates with WITH noise
      geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season,ymin=log(Estimate)-(1.96*Std..Error.for.ln.Estimate.), ymax=log(Estimate)+(1.96*Std..Error.for.ln.Estimate.), color = "VAST No Cov W Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.ln.Estimate.), ymax=Estimate+(1.96*Std..Error.for.ln.Estimate.), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST No Cov W Noise"),size=2)+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST No Cov W Noise"))+
      
      #plot VAST estimate with covariates with NO noise
      geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season,ymin=log(Estimate)-(1.96*Std..Error.for.ln.Estimate.), ymax=log(Estimate)+(1.96*Std..Error.for.ln.Estimate.), color = "VAST w Cov No Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.ln.Estimate.), ymax=Estimate+(1.96*Std..Error.for.ln.Estimate.), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST w Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST w Cov No Noise"))+
      
      #plot VAST estimate with covariates WITH noise
      geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season,ymin=log(Estimate)-(1.96*Std..Error.for.ln.Estimate.), ymax=log(Estimate)+(1.96*Std..Error.for.ln.Estimate.), color = "VAST w Cov w Noise"),width=.3) +
      #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.ln.Estimate.), ymax=Estimate+(1.96*Std..Error.for.ln.Estimate.), color = "VAST Estimate")) +
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST w Cov w Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Estimate),group=season, color = "VAST w Cov w Noise"))+
      
      
      #plot stratified calculation data with NO noise
      # geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season,ymin=log(mean.yr.absolute)-(1.96*log(sd.mean.yr.absolute)), ymax=log(mean.yr.absolute)+(1.96*log(sd.mean.yr.absolute)), color = "Strat Mean No Noise"),width=.3) +
      # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season, color = "Strat Mean No Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season, color = "Strat Mean No Noise"))+
      
      #plot stratified calculation data with NO nois
      # geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean W Noise"),width=.3) +
      # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season, color = "Strat Mean W Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=log(mean.yr.absolute),group=season, color = "Strat Mean W Noise"))+
      
      
      facet_wrap(~ season, ncol =1) +
      # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FC=", toString(FC_spring), 
      #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
      #                                          "  FC=", toString(FC_fall),
      #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
      
      labs(x="year",y="log(Biomass)", title = paste(paste(s," ",folder,sep=""), "\n", #new line
                                               paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                                     "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
                                                     " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                                     "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
                                                     "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
                                                     "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),sep=""), "\n", #new line
                                               paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
                                                     " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
                                                     "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
                                                     "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),sep=""),sep=""), color ="" )+
      
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            title=element_text(size=8))
    
    #one plot per page
    print(Obsmodel_plot_log[[s]][[cov_direct]][[noise]][[folder]])
    
    
    
    
    # for more than one plot per page
      # gridExtra::grid.arrange(Obsmodel_plot[[1]],Obsmodel_plot[[2]],Obsmodel_plot[[3]],nrow=3)
      # gridExtra::grid.arrange(Obsmodel_plot[[4]],Obsmodel_plot[[5]],Obsmodel_plot[[6]],nrow=3)
  
    
    
    
    
    
    #plot estimate ratios by season
    
    Est_ratio_plot[[s]][[cov_direct]][[noise]][[folder]] <- ggplot() +
      
      
      #this way plots data by season divided by itself so it equals 1
      geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group = season, color = "Model"),size=3) +
      geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group =season, color = "Model"),size=1) +
      
      #plot VAST estimate without covariates with NO noise
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST No Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST No Cov No Noise"))+
      
      #plot VAST estimate without covariates with WITH noise
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST No Cov W Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST No Cov W Noise"))+
      
      #plot VAST estimate with covariates with NO noise
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST w Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST w Cov No Noise"))+
      
      #plot VAST estimate with covariates WITH noise
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST w Cov w Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season, color = "VAST w Cov w Noise"))+
      
      
      #plot stratified calculation data with NO noise
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),aes(x=year,y=Est_ratio,group=season, color = "Strat Mean No Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),aes(x=year,y=Est_ratio,group=season, color = "Strat Mean No Noise"))+
      
      #plot stratified calculation data with NO noise
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),aes(x=year,y=Est_ratio,group=season, color = "Strat Mean W Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),aes(x=year,y=Est_ratio,group=season, color = "Strat Mean W Noise"))+
      
    
      facet_wrap(~ season, ncol =1) +
      # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FC=", toString(FC_spring), 
      #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
      #                                          "  FC=", toString(FC_fall),
      #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
      
      labs(x="year",y="Model/Estimate", title = paste(paste(s," ",folder,sep=""),sep=""), color ="" )+
      
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            title=element_text(size=8))
    
    #one plot per page
    print(Est_ratio_plot[[s]][[cov_direct]][[noise]][[folder]])
    
    
    
    
    #plot estimate ratio by season ON LOG SCALE 
    
    Est_ratio_plot_log[[s]][[cov_direct]][[noise]][[folder]] <- ggplot() +
      
      
      #this way plots data by season divided by itself so it equals 1
      geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=log(Est_ratio), group = season, color = "Model"),size=3) +
      geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=log(Est_ratio), group =season, color = "Model"),size=1) +
      
      #plot VAST estimate without covariates with NO noise
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov No Noise"))+
      
      #plot VAST estimate without covariates with WITH noise
      geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov W Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov W Noise"))+
      
      #plot VAST estimate with covariates with NO noise
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov No Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov No Noise"))+
      
      #plot VAST estimate with covariates WITH noise
      geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov w Noise"))+
      geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],Year>=year_min),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov w Noise"))+
      
      
      #plot stratified calculation data with NO noise
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean No Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean No Noise"))+
      
      #plot stratified calculation data with NO noise
      geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean W Noise"))+
      geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean W Noise"))+
      
      
      facet_wrap(~ season, ncol =1) +
      # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FC=", toString(FC_spring), 
      #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
      #                                          "  FC=", toString(FC_fall),
      #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
      
      labs(x="year",y="log(Model/Estimate)", title = paste(paste(s," ",folder,sep=""),sep=""), color ="" )+
      
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            title=element_text(size=8))
    
    #one plot per page
    print(Est_ratio_plot_log[[s]][[cov_direct]][[noise]][[folder]])

}
}









plotss_VAST <- list()
plotss_SRS <- list()

for(s in short_names){ #LIST_ALL WILL BE LENGTH 3 FROM ABOVE
   s_idx <- 1
   s_idx2 <- 1
  
  for(folder in model_types[[s]]){
    
    
 for(noise in c("NoNoise_", "WithNoise_")){
    
    for(cov_directory in c("_NoCovs_", "_WithCov_")){
      
     
        
        cov_direct<- cov_directory
        



plotss_VAST[[s_idx]] <- ggplot() +
  
  #this way plots data by season divided by itself so it equals 1
  geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group = season, color = "Model"),size=3) +
  geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group =season, color = "Model"),size=1) +
  

  # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FC=", toString(FC_spring), 
  #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
  #                                          "  FC=", toString(FC_fall),
  #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
  #plot VAST estimate without covariates with NO noise
  geom_point(data=subset(VAST_data[[s]][[cov_direct]][[noise]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season))+
  geom_line(data=subset(VAST_data[[s]][[cov_direct]][[noise]][[folder]],Year>=year_min),aes(x=Year,y=Est_ratio,group=season))+
  
  labs(x="year",y="Model/Estimate", title = paste(paste(s," ",folder," VAST",cov_direct,noise,sep=""),sep=""), color ="" )+

  facet_wrap(~ season, ncol =1) +
  
theme(axis.text=element_text(size=12),
      axis.title=element_text(size=12),
      title=element_text(size=8))
s_idx=s_idx+1

    }
    
    plotss_SRS[[s_idx2]] <- ggplot() +
      
      #this way plots data by season divided by itself so it equals 1
      geom_point(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group = season, color = "Model"),size=3) +
      geom_line(data = subset(as.data.frame(pop_by_season[[s]]),year>=year_min), aes(x=as.numeric(year),y=Est_ratio, group =season, color = "Model"),size=1) +
      
      
      # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FC=", toString(FC_spring), 
      #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
      #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
      #                                          "  FC=", toString(FC_fall),
      #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
      #plot VAST estimate without covariates with NO noise
      geom_point(data=subset(as.data.frame(SRS_data[[s]][[cov_direct]][[noise]][[folder]]),Year>=year_min),aes(x=year,y=Est_ratio,group=season))+
      geom_line(data=subset(as.data.frame(SRS_data[[s]][[cov_direct]][[noise]][[folder]]),Year>=year_min),aes(x=year,y=Est_ratio,group=season))+
      
      labs(x="year",y="Model/Estimate", title = paste(paste(s," ",folder," SRS",noise,sep=""),sep=""), color ="" )+
      
      facet_wrap(~ season, ncol =1) +
      
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            title=element_text(size=8))
    
    s_idx2=s_idx2+1
    
    
    }}
   
   gridExtra::grid.arrange(plotss_VAST[[1]],plotss_VAST[[2]],plotss_VAST[[3]],plotss_VAST[[4]],plotss_SRS[[1]],plotss_SRS[[2]],ncol=2)
   # gridExtra::grid.arrange(Obsmodel_plot[[4]],Obsmodel_plot[[5]],Obsmodel_plot[[6]],nrow=3)
   
   }



#name columns
colnames(Ratio_summary_info) <- c("Scenario","Estimate", "N_Spring >1", "N_Fall >1","Pct_Spring >1", "Pct_Fall >1", "Total>1", "Pct_Total>1", "Mean_Spring", "Mean_Fall", "Mean_overall", "Mean_overall_finite","SD_overall")
#save as csv
#write.csv(Ratio_summary_info,file = paste(getwd(),"/",scenario1,"_",str_dir,"_Ratio_summary_info_new.csv",sep=""))



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





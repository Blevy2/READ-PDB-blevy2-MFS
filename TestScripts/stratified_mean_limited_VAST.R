#THIS SCRIPT IS A PIECE OF RUN_SURVEY_BENS_NEW WHERE THE STRATIFIED MEAN CALCULATION IS MADE
#THIS SCRIPT ALLOWS US TO CALULATE THE STRATIFIED MEAN FOR A SMALLER NUMER OF NOISELESS SURVEYS
#WILL ALSO CALCULATE THE VAST ESIMATE AS WELL



##################################################################################################
#THINGS WE NEED
##################################################################################################
scenario <- "ConPop_IncTemp"

n_spp <- 3

years_sim <- 22

years_cut <- 2

#survey results without noise
list_all <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\list_all_",scenario,".RDS",sep=""))

#simulation results
result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\result_",scenario,".RDS",sep=""))

#random survey locations
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))

#spp1 spp2 spp3
short_names <- c("YT","Cod","Had") 

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
    
    temp[samp,1] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp1"]],na.rm=T) #YT is spp1
    temp[samp,2] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp2"]],na.rm=T) #Cod is spp2
    temp[samp,3] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp3"]],na.rm=T) #Had is spp3
    
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
saveRDS(list_all,paste("list_all_more_",scenario,".RDS",sep=""))




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


#BELOW WILL TAKE A MINUTE

#choose some strata to exclude, if desired


#George's Bank Setup by species
#YT            Cod          Haddock
exclude <- list(c(13,14,15,17,18), c(23,24,25), c(23,24,25,29,30))

#exclude none
exclude <- list(c(0),c(0),c(0)) #3 species


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









##########################################################################################
#Next create VAST estimates of the survey output
##########################################################################################

#FIGURE OUT HOW BIG EACH CELL OF RASTER IS IN KM^2 TO SET AREASWEPT_KM2 SETTING BELOW

#read in habitat matrix
hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution

#read in GB strata
library(raster)
library(sp)
#haddock contains all and wa sused
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Had_ras)


#translate habitat matrix back into raster
hab_ras <-raster(hab$hab$spp3)
extent(hab_ras) <- extent(Had_ras)
plot(hab_ras)
#check range and mean value of cells
#range(cell_size)
#mean(cell_size)#get sizes of all cells in raster [km2]
cell_size<-raster::area(hab_ras, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]

cell_sz <- mean(cell_size)

VAST_fit_spring <- list() #all model fit info
VAST_fit_fall <- list()

#original project directory so we can switch back to it
orig.dir <- getwd()

#individual strata limits
strata.limits <- list()
strata.limits[["YT"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210)) #THESE ARE YTF STRATA
strata.limits[["Cod"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250)) #THESE ARE COD STRATA
strata.limits[["Had"]] <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300)) #THESE ARE HAD STRATA

#make_settings seems like the way to impliment most desired settings

# settings <- make_settings(n_x = 50,
#                           Region=example$Region, 
#                           purpose="index2", 
#                           strata.limits=example$strata.limits, 
#                           bias.correct=TRUE)
#ABOVE SETTINGS PRODUCE ERRORS. CHECK_FIT SUGGESTS ADDITIONAL FIELDCONFIG SETTINGS

#WHEN ADDING ADDITIONAL FIELDCONFIG SETTINGS ALL 4 SETTINGS BELOW MUST BE INCLUDED
settings_species <- list()

#first attempt at settings. failed on ConPop_IncTemp exclude strata interation #56 fall
settings_species[["YT"]] <- make_settings(n_x = 1000,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          bias.correct=TRUE,
                          FieldConfig= c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=0))

#second attempt which fixes the previous fail
settings_species[["YT"]] <- make_settings(n_x = 1000,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          bias.correct=TRUE,
                          FieldConfig= c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=0),
                          RhoConfig = c("Beta1" = 0, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0))
#' Specification of \code{FieldConfig} can be seen by calling \code{\link[FishStatsUtils]{make_settings}},
#'   which is the recommended way of generating this input for beginning users.
#dafault FieldConfig settings:
# if(missing(FieldConfig)) FieldConfig = c("Omega1"=0, "Epsilon1"=n_categories, "Omega2"=0, "Epsilon2"=0)

#settings

settings_species[["Cod"]] <-  make_settings(n_x = 1000,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          bias.correct=TRUE,
                          FieldConfig= c("Omega1"=1, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0))

settings_species[["Had"]] <-  make_settings(n_x = 1000,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                                            Region=example$Region,
                                            purpose="index2",
                                            strata.limits=example$strata.limits,
                                            bias.correct=TRUE,
                                            FieldConfig= c("Omega1"=1, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0))


#initial scenario folder
dir.create( paste0(getwd(),"/VAST/",scenario)) #create folder to store upcoming subfolders

library(dplyr)

#YT            Cod          Haddock
exclude <- list(c(13,14,15,17,18), c(23,24,25), c(23,24,25,29,30))

#exclude none
exclude <- list(c(0),c(0),c(0)) #3 species



for(iter in seq(length(list_all))){
  
  #pull out survey  
  surv_random_VAST <- list_all[[iter]]
  names(exclude) <- c("YT","Cod","Had")

  
  for(s in short_names){
  
 
  
  setwd(orig.dir)
   
    #create directories first time through
    if(iter == 1){
  
    dir.create( paste0(getwd(),"/VAST/",scenario,"/",s)) #create species directory  
  
    dir.create( paste0(getwd(),"/VAST/",scenario,"/",s,"/spring")) #create spring directory
    
    dir.create( paste0(getwd(),"/VAST/",scenario,"/",s,"/fall")) #create fall directory
       }



#following from Chris' file...

adios <- as.data.frame(surv_random_VAST)
#delete values outside of populations region and those being excluded
adios <- adios[(adios$stratum %in% strata_species[[s]]),]
adios <- adios[!(adios$stratum %in% exclude[[s]]),]
#head(adios)

#PULLING OUT YELLOTWTAIL FLOUNDER IN THIS SCRIPT

# format for use in VAST
spring <- adios %>%
  filter(Season == "SPRING") %>%
  # filter(YEAR >= 2009) %>%
 # mutate(mycatch = paste0(s,"_samp",sep="")) %>%  #OLD WAY DIDNT WORK
  tidyr::unite("mycatch",  paste0(s,"","_samp"),remove=F) %>% #NEW WAY
  dplyr::select(Year = year,
         Catch_KG = mycatch,
         Lat = Lat,
         Lon = Lon) %>%
  mutate(Vessel = "missing",
         AreaSwept_km2 = cell_sz) #CORRECT AREA SWEPT?

fall <- adios %>%
  filter(Season == "FALL") %>%
  # filter(YEAR >= 2009) %>%
  # mutate(mycatch = paste0(s,"_samp",sep="")) %>%  #OLD WAY DIDNT WORK
  tidyr::unite("mycatch",  paste0(s,"","_samp"),remove=F) %>% #NEW WAY
  dplyr::select(Year = year,
         Catch_KG = mycatch,
         Lat = Lat,
         Lon = Lon) %>%
  mutate(Vessel = "missing",
         AreaSwept_km2 = cell_sz) #CORRECT AREA SWEPT?


#summary(spring)
#names(spring)

# reorder the data for use in VAST
#DOESNT SEEM TO BE USED BELOW...??
# nrows <- length(spring[,1])
# reorder <- sample(1:nrows, nrows, replace = FALSE)
# spring_reorder <- spring
# spring_reorder[1:nrows, ] <- spring[reorder, ]
# head(spring)
# head(spring_reorder)


# model with original data and default settings (Poisson link)
example <- list(spring)
example$Region <- "northwest_atlantic"
example$strata.limits <- strata.limits[[s]] 

settings <- settings_species[[s]]


setwd(paste0(getwd(),"/VAST/",scenario,"/",s))

#SPRING FIT
VAST_fit_spring[[s]][[iter]] <- fit_model(settings = settings,
                 "Lat_i"=as.numeric(spring[,'Lat']), 
                 "Lon_i"=as.numeric(spring[,'Lon']), 
                 "t_i"=as.numeric(spring[,'Year']), 
                 "c_i"=as.numeric(rep(0,nrow(spring))), 
                 "b_i"=as.numeric(spring[,'Catch_KG']), 
                 "a_i"=as.numeric(spring[,'AreaSwept_km2']), 
                 "v_i"=spring[,'Vessel'])

#1- THIS PART SAVES CSV AND PNG FOR THE INDEX VALUE
dir.create( paste0(getwd(),"/spring/iter",iter,sep=""))

setwd(paste0(getwd(),"/spring/iter",iter))

plot_biomass_index(VAST_fit_spring[[s]][[iter]])

#copy parameter files into iteration folder
file.rename(from= paste(orig.dir,"/VAST/",scenario,"/",s,"/parameter_estimates.txt",sep="") 
            ,to =paste(orig.dir,"/VAST/",scenario,"/",s,"/spring/iter",iter,"/parameter_estimates.txt",sep=""))

file.rename(from= paste(orig.dir,"/VAST/",scenario,"/",s,"/parameter_estimates.RData",sep="") 
            ,to =paste(orig.dir,"/VAST/",scenario,"/",s,"/spring/iter",iter,"/parameter_estimates.RDATA",sep=""))

#DECIDED NOT TO CALCULATE VALUES DIRECTLY BELOW BECAUSE NOT CONFIDENT OUTPUT WOULD BE SAME 
#
# #2- THIS PART CALCULATES AND EXTRACTS THE VAST POPULATION ESTIMATE AND STD ERROR FOR PLOTTING LATER
# #FOLLOW FROM plot_biomass_index.R SOURCE CODE
# par_SE_sp = TMB:::as.list.sdreport( VAST_fit_spring[[iter]]$parameter_estimates$SD, what="Std. Error", report=TRUE )
# par_hat_sp = TMB:::as.list.sdreport( VAST_fit_spring[[iter]]$parameter_estimates$SD, what="Estimate", report=TRUE )


setwd(paste(orig.dir,"/VAST/",scenario,"/",s,sep=""))

#FALL FIT
VAST_fit_fall[[s]][[iter]] <- fit_model(settings = settings,
                                     "Lat_i"=as.numeric(fall[,'Lat']), 
                                     "Lon_i"=as.numeric(fall[,'Lon']), 
                                     "t_i"=as.numeric(fall[,'Year']), 
                                     "c_i"=as.numeric(rep(0,nrow(fall))), 
                                     "b_i"=as.numeric(fall[,'Catch_KG']), 
                                     "a_i"=as.numeric(fall[,'AreaSwept_km2']), 
                                     "v_i"=fall[,'Vessel'])

dir.create( paste0(getwd(),"/fall/iter",iter,sep=""))

setwd(paste0(getwd(),"/fall/iter",iter))

plot_biomass_index(VAST_fit_fall[[s]][[iter]])

#copy parameter files into iteration folder
file.rename(from= paste(orig.dir,"/VAST/",scenario,"/",s,"/parameter_estimates.txt",sep="") 
            ,to =paste(orig.dir,"/VAST/",scenario,"/",s,"/fall/iter",iter,"/parameter_estimates.txt",sep=""))

file.rename(from= paste(orig.dir,"/VAST/",scenario,"/",s,"/parameter_estimates.RData",sep="") 
            ,to =paste(orig.dir,"/VAST/",scenario,"/",s,"/fall/iter",iter,"/parameter_estimates.RDATA",sep=""))


}

}


#reset working directory
setwd(orig.dir)

#save all individual fits
VAST_fit_all <- list(VAST_fit_spring,VAST_fit_fall)
names(VAST_fit_all) <- c("spring","fall")

saveRDS(VAST_fit_all,paste0(getwd(),"/VAST/",scenario,"/VAST_fit_all_",scenario,".RDS"))






# 
# 
# ##########################################################################################
# # Preparing things to plot
# ##########################################################################################
# 
# 
# #copying plot_pop_summary to summarize yearly population estimates
# #assumes we have summarized version of simulations called res
# results_df <- list()
# 
# for(iter in seq(length(list_all))){
#   
# res <- result[[iter]]
# 
# n_spp <- length(res[["pop_summary"]]) 
# res_df <- lapply(seq_len(n_spp), function(x) {
#   res_spp <- lapply(names(res[["pop_summary"]][[x]]), function(x1) {
#     x1_res <- tidyr::gather(as.data.frame(t(res[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
#     if(x1 == "Bio.mat" | x1 == "Bio.mat.sd") {	res_out <- data.frame("pop" = rep(short_names[[x]], length.out = nrow(x1_res)), 
#                                                                      "metric" = x1, 
#                                                                      "year" = as.numeric(x1_res$year), 
#                                                                      "day" = rep(1:358, length.out = nrow(x1_res)),#changed 362 to 358
#                                                                      "julien_day" = seq_len(nrow(x1_res)),
#                                                                      "data" = x1_res$value) 
#     
#     return(res_out)
#     
#     }
#     if(x1 == "Rec.mat" | x1 == "Rec.mat.sd") { res_out <- data.frame("pop" = rep(short_names[[x]], length.out = nrow(x1_res)), 
#                                                                      "metric" = x1 , 
#                                                                      "year" = as.numeric(seq_len(nrow(x1_res))), 
#                                                                      "day" = rep(1, length.out = nrow(x1_res)),
#                                                                      "julien_day" = rep(1, length.out = nrow(x1_res)),
#                                                                      "data" = x1_res$value) 
#     
#     return(res_out)
#     
#     }
#     
#   })
#   return(do.call(rbind, res_spp))
# })
# 
# results_df[[iter]] <- do.call(rbind, res_df)
# 
# }
# 
# 
# 
# 
# 
# # #ANNUAL POP BY SPECIES
# 
# annual_species <- list()
# 
# for(iter in seq(length(list_all))){
# 
# for(s in seq(length(short_names))){
#   annual_species[[short_names[s]]][[iter]] <- results_df[[iter]] %>% filter(metric == "Bio.mat", day == 1, pop == short_names[s]) %>% 
#     group_by(pop,year) %>% summarise(data = sum(data))
#   
# }
# }



##########################################################################################
#Next measure error between estimates and true values and plot estimates
##########################################################################################



pdf(file=paste("Results/GB_error_plots/Individual_SRS_",scenario,".pdf",sep=""))


nyears <- 20

#for error calculation
SRS_error_spring <- list()
SRS_error_fall <- list()
model <- list()
SRS_spring <- list()
SRS_fall <- list()

VAST_error_spring <- list()
VAST_error_fall <- list()
VAST_spring <- list()
VAST_fall <- list()

vast.dir <- paste("VAST/",scenario,sep="")

#plot stratified calculation and population estimate on same plot

#first make model output have 2 seasons to match the stratified mean calcs

for(iter in seq(length(list_all))){
  
  print(iter)
  
# for(s in seq(length(annual_species))){
#   annual_species[[short_names[s]]][[iter]] <- rbind(annual_species[[short_names[s]]][[iter]][3:22,],annual_species[[short_names[s]]][[iter]][3:22,])
#   annual_species[[short_names[s]]][[iter]]$season <- c(rep(1,nyears),rep(2,nyears)) #spring = season 1, fall = season 2
#   annual_species[[short_names[s]]][[iter]]$year <- as.numeric(rep(seq(3,22),2))
# }
  


SRS_data <- list()
VAST_data <- list()

for(s in short_names){ 
 
  #pull out strat mean calc
  SRS_data[[s]] <-  strat_mean_all[[s]][[iter]] 

  

  #MODEL VALUES
  model_spring = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==1,"biomass"]
    
  model_fall = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==2,"biomass"]
  
  #SRS VALUES
  
  #SRS spring estimate
  SRS_spring[[s]][[iter]] <- strat_mean_all[[s]][[iter]][strat_mean_all[[s]][[iter]][,"season"]==1,"mean.yr.absolute"]
  
  #SRS fall estimate
  SRS_fall[[s]][[iter]] <- strat_mean_all[[s]][[iter]][strat_mean_all[[s]][[iter]][,"season"]==2,"mean.yr.absolute"]
  
  #calculate SPRING SRS error from each iteration
  SRS_error_spring[[s]][[iter]] <- norm(model_spring- SRS_spring[[s]][[iter]] , type="2") / norm(model_spring , type ="2")
  
  #calculate FALL SRS error from each iteration
  SRS_error_fall[[s]][[iter]] <- norm(model_fall - SRS_fall[[s]][[iter]] , type="2") / norm(model_fall , type ="2")
  
  
  #VAST VALUES
  
  #read in csv for estimate
  Vast_sp_est <- read.csv(paste0(getwd(),"/VAST/",scenario,"/",s,"/spring/iter",iter,"/Index.csv"), header=T)  
  Vast_fa_est <- read.csv(paste0(getwd(),"/VAST/",scenario,"/",s,"/fall/iter",iter,"/Index.csv"), header=T)
  
  #add year & season to these
  Year <-  seq(years_cut+1,years_sim)
  season <- rep(1,years_sim-years_cut)
  Vast_sp_est <- cbind(Vast_sp_est,Year,season)
  season <- rep(2,years_sim-years_cut)
  Vast_fa_est <- cbind(Vast_fa_est,Year,season)
    
  #VAST spring estimate
  VAST_spring[[s]][[iter]] <- Vast_sp_est[,"Estimate"]

  #VAST fall estimate
  VAST_fall[[s]][[iter]] <- Vast_fa_est[,"Estimate"]
  
  #calculate SPRING VAST error from each iteration
  VAST_error_spring[[s]][[iter]] <- norm(model_spring- VAST_spring[[s]][[iter]] , type="2") / norm(model_spring , type ="2")
  
  #calculate FALL VAST error from each iteration
  VAST_error_fall[[s]][[iter]] <- norm(model_fall - VAST_fall[[s]][[iter]] , type="2") / norm(model_fall , type ="2")
  
  
  #store VAST stuff to plot later
  VAST_data[[s]] <-  rbind(Vast_sp_est,Vast_fa_est)
  
    }


long_names <- c("Yellowtail Flounder","Cod","Haddock")


#par(mfrow = c(1,3), mar = c(1, 1, 1, 1))

#for(s in short_names){
  # 
  # #OLD WAY
  # #initiate ggplot
  # p<- ggplot() +
  #   #plot stratified calculation data
  #   geom_errorbar(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
  #   geom_point(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
  #   geom_line(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
  #   #plot model data
  #   geom_point(data = as.data.frame(annual_species[[s]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
  #   geom_line(data = as.data.frame(annual_species[[s]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
  #   
  #   facet_wrap(~ season) +
  #   labs(x="year",y="Biomass", title = long_names[idx], color ="" ) 
  # idx<-idx+1
  # 
  # print(p)
  #}
  
  #NEW WAY PLOTTING 3 TOGETHER ON SAME PAGE
    
    #YTF
    p1<- ggplot() +   
      
      #this way plots data by season
    geom_point(data = as.data.frame(pop_by_season[[1]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=2) +
    geom_line(data = as.data.frame(pop_by_season[[1]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=1) +
    
    #plot stratified calculation data
    geom_errorbar(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
    geom_point(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    geom_line(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    
      #plot model data
      #this way plots annual data
    #geom_point(data = as.data.frame(annual_species[[1]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    #geom_line(data = as.data.frame(annual_species[[1]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    
    #plot VAST estimate
    geom_errorbar(data=VAST_data[[1]],aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate"),width=.3) +
    geom_point(data=VAST_data[[1]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
    geom_line(data=VAST_data[[1]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
      
    facet_wrap(~ season) +
    labs(x="year",y="Biomass", title = long_names[1], color ="" ) 

    #COD
    p2<- ggplot() +  
      
      #this way plots data by season
      geom_point(data = as.data.frame(pop_by_season[[2]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=2) +
      geom_line(data = as.data.frame(pop_by_season[[2]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=1) +
      
      #plot stratified calculation data
      geom_errorbar(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
      geom_point(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
      geom_line(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
      
      #plot model data
      #this way plots annual data
      #geom_point(data = as.data.frame(annual_species[[2]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
      #geom_line(data = as.data.frame(annual_species[[2]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
 
      #plot VAST estimate
      geom_errorbar(data=VAST_data[[2]],aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate"),width=.3) +
      geom_point(data=VAST_data[[2]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
      geom_line(data=VAST_data[[2]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
      
      facet_wrap(~ season) +
      labs(x="year",y="Biomass", title = long_names[2], color ="" )
    
    #HAD
    p3<- ggplot() +     
      
      #this way plots data by season
      geom_point(data = as.data.frame(pop_by_season[[3]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=2) +
      geom_line(data = as.data.frame(pop_by_season[[3]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=1) +
      
      #plot stratified calculation data
      geom_errorbar(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
      geom_point(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
      geom_line(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
      
      #plot model data
      #this way plots annual data
      #geom_point(data = as.data.frame(annual_species[[3]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
      #geom_line(data = as.data.frame(annual_species[[3]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
      

      #plot VAST estimate
      geom_errorbar(data=VAST_data[[3]],aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate"),width=.3) +
      geom_point(data=VAST_data[[3]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
      geom_line(data=VAST_data[[3]],aes(x=Year,y=Estimate,group=season, color = "VAST Estimate"))+
      
      facet_wrap(~ season) +
      labs(x="year",y="Biomass", title = long_names[3], color ="" )

    gridExtra::grid.arrange(p1,p2,p3,nrow=3)


}

dev.off()













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

df_SRS <- rbind(df_SRS_fall,df_SRS_spring)


#create data frame containing mean values for each group
means_SRS <- ddply(df_SRS, .(species,season), summarise, mean = mean(as.numeric(error)), Model = "Strat. Mean")



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
means_VAST <- ddply(df_VAST, .(species,season), summarise, mean = mean(as.numeric(error)), Model = "VAST")


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
                   aes(x=iter,y=as.numeric(error),color=Model)) +
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





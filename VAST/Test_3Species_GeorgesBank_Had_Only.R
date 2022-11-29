#Using GBYT_VAST.R from Chris as template 

library(raster)
library(sp)
library(TMB)
library(VAST)
library(dplyr)
library(ggplot2)
library(beepr)

orig.dir <- getwd()

#FIRST READ IN SURVEY VALUES AND ADD COLUMNS THAT CONVERY X,Y INTO LAT,LON


################################################################################
#read in sample survey
# surv_random_sample <- readRDS(file="surv_random_sample.RDS")
# surv_random_sample <- as.matrix(surv_random_sample,ncol= 12)

scenario1 <- "IncPop_IncTemp"
#survey results without noise
list_all <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario1,"\\list_all_",scenario1,".RDS",sep=""))

exclude_strata <- FALSE

with_noise <- FALSE

covariates <- FALSE

cov_used <- "_WithCov"  #"Temp_LinearBasic" 

#for ConPop_ConTemp iteration 6 shows steady population with some small varability througout
#for ConPop_IncTemp iteration 3 shows steady population 
#IncPop_ConTemp doesnt have great options but 98 pretty good
#IncPop_IncTemp 100 is pretty good
#DecPop_ConTemp 6 is pretty good
#DecPop_IncTemp 9 is pretty good

surv_random_sample <- list_all[[100]]


setwd(orig.dir)

scenario <- paste("ForPaper/",scenario1,sep="")
################################################################################

#read in habitat matrix
hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution
names(hab$hab) <- c("YT","Cod","Had")
#read in GB strata

#haddock contains all stratas used
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Had_ras)
#load others to extract covariate values
#Yellowtail
YT_ras <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/Yell_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(YT_ras)
#Cod
Cod_ras <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Cod_ras)

#translate habitat matrix back into raster
hab_ras <-raster(hab$hab$Had)
extent(hab_ras) <- extent(Had_ras)
plot(hab_ras)

#create Haddock matrix
Had_matrix <- as.matrix(Had_ras)


#ADD COLUMNS TO SURVEY THAT CONTAIN LAT/LON INFORMATION

#longitude is NS. These are X values or rows
#obtained via longitude = yFromRow(raster,row = ) 

#latitude is EW, These are Y values or columns
#obtained via latitude = xFromCol(raster,col= )

#load increasing or constant temp gradient based on scenario
tmp <- substr(scenario1,8,10)

if(tmp == "Con"){moveCov <- readRDS(paste("20 year moveCov matrices/GeorgesBank/GB_22yr_",tmp,"stTemp_HaddockStrata_res2",sep=""))}
if(tmp == "Inc"){moveCov <- readRDS(paste("20 year moveCov matrices/GeorgesBank/GB_22yr_",tmp,"rTemp_HaddockStrata_res2",sep=""))}

#temp tolerances
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("YT" = list("mu" = 9, "va" = 4),  #Yellowtail
                             "Cod" = list("mu" = 8.75, "va" = 4.25),  #Cod
                             "Had" = list("mu" = 9, "va" = 4) )    #Haddock


lat <- vector()
lon <- vector()
temperature <- vector()
hab_cov <- list()
movement_cov <- list()

for(i in seq(length(surv_random_sample[,1]))){
  
  rw <- as.numeric(surv_random_sample[i,"x"])  #x in col 2
  cl <- as.numeric(surv_random_sample[i,"y"]) #y in col 3
  
  lon[i] <- xFromCol(hab_ras, col = cl)
  lat[i] <- yFromRow(hab_ras, row = rw)
  
  #record covarite temp
  wk =  as.numeric(surv_random_sample[i,"week"])  
  yr = as.numeric(surv_random_sample[i,"year"]) 
  
  temperature[i] <- moveCov$cov.matrix[[52*(yr-1)+wk]][rw,cl]
  
  #PULLING MOVEMENT = HAB^2*TEMP_PREFERENCE VALUES
  movement_cov[["YT"]][i] <- (hab$hab[["YT"]][rw,cl]^2)*MixFishSim::norm_fun(temperature[i],mu=moveCov$spp_tol$YT$mu,va=moveCov$spp_tol$YT$va)
  movement_cov[["Cod"]][i] <- (hab$hab[["Cod"]][rw,cl]^2)*MixFishSim::norm_fun(temperature[i],mu=moveCov$spp_tol$Cod$mu,va=moveCov$spp_tol$Cod$va)    
  movement_cov[["Had"]][i] <- (hab$hab[["Had"]][rw,cl]^2)*MixFishSim::norm_fun(temperature[i],mu=moveCov$spp_tol$Had$mu,va=moveCov$spp_tol$Had$va)
  
  #PULLING HABITAT VALUES
  #This uses the normalized habitat values, which are extremely small 
  #and therefore may have been interfering with parameter estimation
  # hab_cov[["YT"]][i] <- hab$hab[["YT"]][rw,cl]
  # hab_cov[["Cod"]][i] <- hab$hab[["Cod"]][rw,cl]
  # hab_cov[["Had"]][i] <- hab$hab[["Had"]][rw,cl]
  
  #Instead try using non-normalize values
  hab_cov[["YT"]][i] <- YT_ras[rw,cl]
  hab_cov[["Cod"]][i] <- Cod_ras[rw,cl]
  hab_cov[["Had"]][i] <- Had_ras[rw,cl]
  
  #replace NA with 0
  if(is.na(hab_cov[["YT"]][i])){hab_cov[["YT"]][i]<-0}
  if(is.na(hab_cov[["Cod"]][i])){hab_cov[["Cod"]][i]<-0}
  if(is.na(hab_cov[["Had"]][i])){hab_cov[["Had"]][i]<-0}
  
}


#add columns to survey table
temp <- cbind.data.frame(surv_random_sample[,1:(length(surv_random_sample[1,])-1)],as.numeric(lat),lon,temperature, hab_cov[["YT"]], hab_cov[["Cod"]], hab_cov[["Had"]], movement_cov[["YT"]], movement_cov[["Cod"]], movement_cov[["Had"]])

x<-matrix(as.numeric(unlist(temp)),nrow = nrow(temp))
surv_random_sample<-cbind.data.frame(x,surv_random_sample[,length(surv_random_sample[1,])])
colnames(surv_random_sample) <- c("station_no","x","y","stratum","day","tow","year","YTF","Cod","Had","week","Lat","Lon","Temp","Hab_YT","Hab_Cod","Hab_Had","MoveCov_YT","MoveCov_Cod","MoveCov_Had","Season")


#get sizes of all cells in raster [km2]
cell_size<-raster::area(hab_ras, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]

#check range and mean value of cells
range(cell_size)
mean(cell_size)












# 
# 
# 
# 
# library(spatstat.geom)
# #plot lat and lon coordinates to check that they look correct
# all_points <- spatstat.geom::ppp(x=lon,y=lat, marks = surv_random_sample[,"stratum"] , window=owin(c(-70.99, -65) ,c(40,43)))
# plot(all_points, use.marks=T) #here we see the stratas appear which makes me feel like it worked
# 
# 
# #load gb polygon which has area info in it
# #load stratas for clipping etc
# strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
# library(rgdal)
# # get the shapefiles
# strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 
# 
# #define georges bank
# GB_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250", "01290", "01300")
# #pull out indices corresponding to GB strata
# GB_strata_idx <- match(GB_strata_num,strata.areas@data[["STRATUMA"]])
# #plot them
# #plot(strata.areas[GB_strata_idx,])
# #define GB strata as own object
# GB_strata <- strata.areas[GB_strata_idx,]
# 
# 
# 
# #load random survey
# scenario <- "ConPop_ConTemp"
# #random survey locations
# surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))
# 
# #area info is in...
# st_area <- GB_strata$A2
# #corresponding to these strata...
# st_num <- GB_strata$STR2
# #number of cells in each strata...
# cell_str <- surv_random$cells_per_strata[!is.na(surv_random$cells_per_strata)]
# #area per cell...
# area_per_cell <- st_area/cell_str
# 
# 
# #following from https://gis.stackexchange.com/questions/200420/calculate-area-for-each-polygon-in-r
# 
# #check coordinate system
# crs(GB_strata)
# #add area value to GB_strata
# GB_strata$area_sqkm <- area(GB_strata)/1000000
# #number of cells in each strata...
# cell_str <- surv_random$cells_per_strata[!is.na(surv_random$cells_per_strata)]
# #area per cell...
# area_per_cell <- GB_strata$area_sqkm/cell_str
# 







#change directory
setwd(paste(orig.dir,"/VAST", sep=""))
#create new one
dir.create(paste(getwd(),"/",scenario,sep=""))


#HAD   
ifelse(exclude_strata==TRUE, exclude <- c(23,24,25,29,30),exclude <- c(0))
ifelse(exclude_strata==TRUE, exclude_full <- c(1230,1240,1250,1290,1300),exclude_full <- c(0))

strata_species <-  c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)

#do some model selection things
model_aic <- list()

#SAMPLE DATA
adios <- as.data.frame(surv_random_sample)
adios <- adios[(adios$stratum %in% strata_species),]
adios <- adios[!(adios$stratum %in% exclude),]




##################################################################################
# Add noise to sample data here, if desired
##################################################################################
if(with_noise==TRUE){
  print("ADDING NOISE TO DATA")
  temp_noise <-  sapply(adios$Had , function(x){rlnorm(1,mean=log(x),sdlog=.35)} ) 
  
  adios$Had <- temp_noise
  
}
##################################################################################
##################################################################################


#create directory for model specific output
dir.create(paste(getwd(),"/",scenario,"/Had",sep=""))

ifelse(with_noise==TRUE,{noise_dir <- "_WithNoise_"},{noise_dir <- "_NoNoise_"})

ifelse(covariates==TRUE,{cov_dir <- paste(cov_used,sep="")},{cov_dir <- "_NoCovs"})


#save adios to have a record, for ex to create strat. mean later
ifelse(exclude_strata==TRUE, 
       {dir.create(paste(getwd(),"/",scenario,"/Had/ExcludeStrata",cov_dir,noise_dir,sep=""))
         str_dir <- "ExcludeStrata"},
       {dir.create(paste(getwd(),"/",scenario,"/Had/AllStrata",cov_dir,noise_dir,sep=""))
         str_dir <- "AllStrata"})


saveRDS(adios,file=paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/adios.RDS",sep=""))







##################################################################################
#covariates
#create covariate data
##################################################################################


covdata <- data.frame(cbind.data.frame(as.numeric(adios[,"Lat"]),as.numeric(adios[,"Lon"]),adios[,"x"],adios[,"y"],adios[,"week"],adios[,"year"],as.numeric(adios[,"Temp"]),as.numeric(adios[,"Hab_Had"]),adios[,"MoveCov_Had"],adios[,"Season"]))
names(covdata) <- c("Lat","Lon","x","y","Week","Year","Temp","Habitat","MoveCov","Season")



#################################################################################
#IF USING A STATIC COVARIATE DATA, SUCH AS HABITAT, DEPTH, SEDIMENT SIZE ETC


#THIS CHUNK ATTEMPTS TO CREATE COVARITE DATA FOR TEMP AND HABITAT
#per the covariate wiki example, go through covariate table and copy each static habitat value for each of the years
years <- seq(3,22)
temp_cov <- data.frame()
for(row in seq(length(covdata[,1]))){
  #repeat each row the correct number of times
  temp = covdata[rep(row,length(years)),]
  
  #remove temporally changing covariate(s) USED TO DO THIS BUT WONT RUN. INSTEAD NEED TO RECORD ACTUAL TEMP FOR GIVEN WEEK/YEAR
  #temp$Temp = NA
  temper <- vector()
  for(i in seq(length(years))){
    rw <- covdata[row,"x"]
    cl <- covdata[row,"y"]
    wk =  covdata[row,"Week"]  #same location, different year
    yr = years[i]
    
    temper[i] <- moveCov$cov.matrix[[52*(yr-1)+wk]][rw,cl]
  }
  temp$Temp = temper
  
  #change year column to years we are using
  temp$Year = years
  
  temp_cov <- rbind.data.frame(temp_cov,temp)
  
  
}


#lat, lon, year, temp, habitat
covdata <- temp_cov[, c("Lat","Lon","Year","Temp","Habitat","Season")]
names(covdata) <- c("Lat","Lon","Year","Temp","Habitat","Season")



#THIS CHUNK ATTEMPTS TO CREATE COVARITE DATA FOR JUST HABITAT
#per the covariate wiki example, go through covariate table and copy each static habitat value for each of the years
#lat, lon, year, habitat
covdata <- covdata[,c(1,2,6,8)]
covdata$Year <- NA

#################################################################################  

#HABITAT AND TEMP
#INCLUDE LAT, LON, YEAR, TEMP, HABITAT (1:5)
covdata_fall <- covdata[covdata$Season=="FALL",c("Lat","Lon","Year","Temp","Habitat")]
covdata_spring <- covdata[covdata$Season=="SPRING",c("Lat","Lon","Year","Temp","Habitat")]
#View(covdata_fall)

#MoveCov and Temp
#INCLUDE LAT, LONG, YEAR, TEMP, MOVECOV  
covdata_fall <- covdata[covdata$Season=="FALL",c("Lat","Lon","Year","Temp","MoveCov")]
covdata_spring <- covdata[covdata$Season=="SPRING",c("Lat","Lon","Year","Temp","MoveCov")]
#make the few NA values 0
covdata_fall$MoveCov[is.na(covdata_fall$MoveCov)] <- 0
covdata_spring$MoveCov[is.na(covdata_spring$MoveCov)] <- 0 

#JUST TEMP
#INCLUDE LAT, LON, YEAR, TEMP
covdata_fall <- covdata[covdata$Season=="FALL",c(1,2,6,7)]
covdata_spring <- covdata[covdata$Season=="SPRING",c(1,2,6,7)]

#JUST MOVEMENT COVARIATE HAB^2*TEMP_TOLERANCE
#INCLUDE LAT, LON, YEAR, MOVECOV
covdata_fall <- covdata[covdata$Season=="FALL",c("Lat","Lon","Year","MoveCov")]
covdata_spring <- covdata[covdata$Season=="SPRING",c("Lat","Lon","Year","MoveCov")]
#make the few NA values 0
covdata_fall$MoveCov[is.na(covdata_fall$MoveCov)] <- 0
covdata_spring$MoveCov[is.na(covdata_spring$MoveCov)] <- 0 

#################################################################################





for(j in 1:6){
  
  # OLD ONES FROM CHUCK ADAMS' PAPER WITH CHRIS AND LIZ
  # if(j == 1) {obsmodel <- c(2, 0); run <- 1}
  # if(j == 2) {obsmodel <- c(2, 1); run <- 3} #model selection
  # if(j == 3) {obsmodel <- c(1, 0); run <- 4}
  # if(j == 4) {obsmodel <- c(1, 1); run <- 5}
  
  # NEW ONES BASED ON CHRIS C'S RECOMMENDATION
  if(j == 1) {obsmodel <- c(2, 0); run <- 1}
  if(j == 2) {obsmodel <- c(2, 1); run <- 2} #model selection
  if(j == 3) {obsmodel <- c(4, 0); run <- 3}
  if(j == 4) {obsmodel <- c(4, 1); run <- 4}
  if(j == 5) {obsmodel <- c(9, 0); run <- 5}
  if(j == 6) {obsmodel <- c(9, 1); run <- 6}
  if(j == 7) {obsmodel <- c(10, 2); run <- 7}
  
  
  
  ifelse(exclude_strata==TRUE, 
         {dir.create(paste(getwd(),"/",scenario,"/Had/ExcludeStrata",cov_dir,noise_dir,sep=""))
           str_dir <- "ExcludeStrata"},
         {dir.create(paste(getwd(),"/",scenario,"/Had/AllStrata",cov_dir,noise_dir,sep=""))
           str_dir <- "AllStrata"})
  
  dir.create(paste(getwd(),"/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,sep=""))
  
  setwd((paste(getwd(),"/",scenario,"/Had/",str_dir,cov_dir,noise_dir,sep="")))
  

  head(adios)
  
  #PULLING OUT YELLOTWTAIL FLOUNDER IN THIS SCRIPT
  
  # format for use in VAST
  spring <- adios %>%
    filter(Season == "SPRING") %>%
    # filter(YEAR >= 2009) %>%
    mutate(mycatch = Had) %>%
    select(Year = year,
           Catch_KG = mycatch,
           Lat = Lat,
           Lon = Lon) %>%
    mutate(Vessel = "missing",
           AreaSwept_km2 = mean(cell_size)) #CORRECT AREA SWEPT?
  # summary(spring)
  # names(spring)
  
  # reorder the data for use in VAST
  #DOESNT SEEM TO BE USED BELOW...??
  # nrows <- length(spring[,1])
  # reorder <- sample(1:nrows, nrows, replace = FALSE)
  # spring_reorder <- spring
  # spring_reorder[1:nrows, ] <- spring[reorder, ]
  # head(spring)
  # head(spring_reorder)
  
  GB_strat <- c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300)
  GB_strat <- GB_strat[!(GB_strat %in% exclude_full)]
  
  # model with original data and default settings (Poisson link)
  example <- list(spring)
  example$Region <- "northwest_atlantic"
  example$strata.limits <- data.frame(Georges_Bank = GB_strat) #THESE ARE HAD STRATA
  
  #make_settings seems like the way to impliment most desired settings
  
  
  #FC1 = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1) 
  
  
  FC1 = c("Omega1" = 0, "Epsilon1" =0, "Omega2" = 1, "Epsilon2" = 1) 
  RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 4)
  
  #FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"=0
  
  settings <- make_settings(n_x = 500,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                            Region=example$Region,
                            purpose="index2",
                            strata.limits=example$strata.limits,
                            bias.correct=TRUE,
                            FieldConfig= FC1,
                            RhoConfig = RhoConfig,
                            ObsModel = obsmodel,
                            knot_method = "samples") #ABOVE SETTINGS PRODUCE ERRORS. CHECK_FIT SUGGESTS ADDITIONAL FIELDCONFIG SETTINGS
  
  #WHEN ADDING ADDITIONAL FIELDCONFIG SETTINGS ALL 4 SETTINGS BELOW MUST BE INCLUDED
  # settings <- make_settings(n_x = 500,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
  #                           Region=example$Region,
  #                           purpose="index2",
  #                           strata.limits=example$strata.limits,
  #                           bias.correct=TRUE,
  #                           FieldConfig= c("Omega1"=1, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0),
  #                           ObsModel = obsmodel)
  #' Specification of \code{FieldConfig} can be seen by calling \code{\link[FishStatsUtils]{make_settings}},
  #'   which is the recommended way of generating this input for beginning users.
  #dafault FieldConfig settings:
  # if(missing(FieldConfig)) FieldConfig = c("Omega1"=0, "Epsilon1"=n_categories, "Omega2"=0, "Epsilon2"=0)
  
  #settings
  
  #setwd("C:\\Users\\benjamin.levy\\Desktop\\Github\\READ-PDB-blevy2-MFS2\\VAST\\ConPop_IncTemp_exclude_most")
  
  
  
  #######################################################################################
  # Try this first
  #######################################################################################
  ifelse(covariates == "TRUE",{
    print("USING COVARIATES")
    
    #try domed-shaped response for temp and linear for habitat
    
    # #MoveCov = hab^2*temp_tolerance
    #  X1_formula = ~ poly(MoveCov, degree=2 )
    #  X2_formula = ~ poly(MoveCov, degree=2 )
    #   
    #Chris C idea for including 2 covariates
    X1_formula = ~ poly(Temp, degree=2 )
    X2_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    
    # X1_formula = ~ 1
    # X2_formula = ~ poly(Habitat, degree=3 )
    
    # X1_formula = ~poly(Temp, degree = 2)
    # X2_formula = ~poly(Temp, degree = 2)
    
    
    # # #JUST TEMP
    # X1_formula = ~ poly(Temp, degree=2 )
    # X2_formula = ~ poly(Temp, degree=2 )
    #   #TEMP and HABITAT
    
    #JUST HABITAT
    # X1_formula = ~ poly(Habitat, degree=2 )
    # X2_formula = ~ poly(Habitat, degree=2 )
    
    # #TEMP and HABITAT
    # X1_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    # X2_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    #   
    # 
    # X1_formula = ~ poly(Habitat, degree=2 )
    # X2_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    
    
    fit_spring <- try(fit_model(settings = settings,
                                "Lat_i"=as.numeric(spring[,'Lat']), 
                                "Lon_i"=as.numeric(spring[,'Lon']), 
                                "t_i"=as.numeric(spring[,'Year']), 
                                "c_iz"=as.numeric(rep(0,nrow(spring))), 
                                "b_i"=as.numeric(spring[,'Catch_KG']), 
                                "a_i"=as.numeric(spring[,'AreaSwept_km2']),
                                X1_formula = X1_formula,
                                X2_formula = X2_formula,
                                covariate_data = covdata_spring,
                                optimize_args=list("lower"=-Inf,"upper"=Inf)),
                      silent = TRUE)
    
    #   optimize_args=list("lower"=-Inf,"upper"=Inf)),
    
  },{
    print("NOT USING COVARIATES")
    fit_spring <- try(fit_model(settings = settings,
                                "Lat_i"=as.numeric(spring[,'Lat']), 
                                "Lon_i"=as.numeric(spring[,'Lon']), 
                                "t_i"=as.numeric(spring[,'Year']), 
                                "c_iz"=as.numeric(rep(0,nrow(spring))), 
                                "b_i"=as.numeric(spring[,'Catch_KG']), 
                                "a_i"=as.numeric(spring[,'AreaSwept_km2']),
                                optimize_args=list("lower"=-Inf,"upper"=Inf)), 
                      silent = TRUE)
    
  })
  beep(sound=8)
  
  
  
  model_aic[["spring"]][[j]] <- fit_spring$parameter_estimates$AIC
  
  #create directory for season specific output
  dir.create(paste(getwd(),"/obsmodel",j,"/spring",sep=""))
  setwd(paste(getwd(),"/obsmodel",j,"/spring",sep=""))
  #silent = TRUE might stop output in console
  
  saveRDS(fit_spring,file = paste(getwd(),"/fit_spring.RDS",sep=""))
  
  
  #plot_biomass_index(fit_spring)
  
  plot(fit_spring)
  
  #copy parameter files into iteration folder
#  remove(fit_spring)
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/settings.txt",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/spring/settings.txt",sep=""))
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/parameter_estimates.txt",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/spring/parameter_estimates.txt",sep=""))
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/parameter_estimates.RDATA",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/spring/parameter_estimates.RDATA",sep=""))
  
  
  
  #plot covariate respopne, if applicable 
  
  print("PLOTTING COVARIATE RESPONSE")
  pdf(file=paste(getwd(),"/",cov_used,"_cov_res_spring",".pdf",sep=""))
  fittt = fit_spring
  covariate_data_full = fittt$effects$covariate_data_full
  catchability_data_full = fittt$effects$catchability_data_full
  
  # Plot 1st linear predictor, but could use `transformation` to apply link function
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Temp"),
                           which_formula = "X1",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Habitat"),
                           which_formula = "X2",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Temp"),
                           which_formula = "X2",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  # 
  # pred2 = Effect.fit_model( fittt,
  #                           focal.predictors = c("Temp"),
  #                           which_formula = "X2",
  #                           xlevels = 100,
  #                           transformation = list(link=identity, inverse=identity) )
  # plot(pred2)
  # 
  # pred3 = Effect.fit_model( fittt,
  #                           focal.predictors = c("Habitat"),
  #                           which_formula = "X2",
  #                           xlevels = 100,
  #                           transformation = list(link=identity, inverse=identity) )
  # plot(pred3)
  # 
  
  #####################
  # pdp package
  #####################
  #    
  #     
  #     #might need to add yea rback in to habitat
  # #    fittt$covariate_data$Year=covdata_spring$Year
  #     
  #     library(pdp)
  #     
  #     # Make function to interface with pdp
  #     pred.fun = function( object, newdata ){
  #       predict( x=object,
  #                Lat_i = object$data_frame$Lat_i,
  #                Lon_i = object$data_frame$Lon_i,
  #                t_i = object$data_frame$t_i,
  #                a_i = object$data_frame$a_i,
  #                what = "P1_iz",
  #                new_covariate_data = newdata,
  #                do_checks = FALSE )
  #     }
  #     
  #     # Run partial
  #     Partial = partial( object = fittt,
  #                        pred.var = "Habitat",
  #                        pred.fun = pred.fun,
  #                        train = fittt$covariate_data )
  #     
  #     # Make plot using ggplot2
  #     library(ggplot2)
  #     autoplot(Partial)
  
  dev.off()
  remove(fittt)
  
  
  remove(fit_spring)
  
  
  
  setwd('..') #move up one directory
  dir.create(paste(getwd(),"/fall",sep="")) #create fall directory
  
  setwd('..') #move up one directory
  
  fall <- adios %>%
    filter(Season == "FALL") %>%
    # filter(YEAR >= 2009) %>%
    mutate(mycatch = Had) %>%
    select(Year = year,
           Catch_KG = mycatch,
           Lat = Lat,
           Lon = Lon) %>%
    mutate(Vessel = "missing",
           AreaSwept_km2 = mean(cell_size)) #CORRECT AREA SWEPT?
  # summary(fall)
  # names(fall)
  
  # reorder the data for use in VAST
  #DOESNT SEEM TO BE USED BELOW...??
  # nrows <- length(spring[,1])
  # reorder <- sample(1:nrows, nrows, replace = FALSE)
  # spring_reorder <- spring
  # spring_reorder[1:nrows, ] <- spring[reorder, ]
  # head(spring)
  # head(spring_reorder)
  
  
  # model with original data and default settings (Poisson link)
  example <- list(fall)
  example$Region <- "northwest_atlantic"
  example$strata.limits <- data.frame(Georges_Bank = GB_strat) #THESE ARE HAD STRATA
  
  
  
  #FC2 = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1) 
  
  FC2 = c("Omega1" = 0, "Epsilon1" =0, "Omega2" = 1, "Epsilon2" = 1) 
  RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" =4)
  
  settings <- make_settings(n_x = 500,
                            Region=example$Region,
                            purpose="index2",
                            strata.limits=example$strata.limits,
                            bias.correct=TRUE,  
                            FieldConfig= FC2,
                            RhoConfig=RhoConfig,
                            ObsModel = obsmodel,
                            knot_method = "samples")

  #WHEN ADDING ADDITIONAL FIELDCONFIG SETTINGS ALL 4 SETTINGS BELOW MUST BE INCLUDED
  # settings <- make_settings(n_x = 500,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
  #                           Region=example$Region,
  #                           purpose="index2",
  #                           strata.limits=example$strata.limits,
  #                           bias.correct=TRUE,
  #                           FieldConfig= c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=0),
  #                           RhoConfig = c("Beta1" = 0, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0))
  #' Specification of \code{FieldConfig} can be seen by calling \code{\link[FishStatsUtils]{make_settings}},
  #'   which is the recommended way of generating this input for beginning users.
  #dafault FieldConfig settings:
  # if(missing(FieldConfig)) FieldConfig = c("Omega1"=0, "Epsilon1"=n_categories, "Omega2"=0, "Epsilon2"=0)
  
  #######################################################################################
  # Try this first
  #######################################################################################
  ifelse(covariates == "TRUE",{
    print("USING COVARIATES")
    
    #MoveCov = hab^2*temp_tolerance
    # X1_formula = ~ poly(MoveCov, degree=2 )
    # X2_formula = ~ poly(MoveCov, degree=2 )
    
    #Chris C idea for including 2 covariates
    # X1_formula = ~ poly(Temp, degree=2 )
    # X2_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    # X1_formula = ~ 1
    # X2_formula = ~ poly(Habitat, degree=3 )
    
    #HABITAT AND TEMP
    # X1_formula = ~ poly(Habitat, degree=2 )
    # X2_formula = ~ poly(Temp, degree=2 ) + poly(Habitat, degree=2 )
    
    
    fit_fall <- try(fit_model(settings = settings,
                              "Lat_i"=as.numeric(fall[,'Lat']), 
                              "Lon_i"=as.numeric(fall[,'Lon']), 
                              "t_i"=as.numeric(fall[,'Year']), 
                              "c_iz"=as.numeric(rep(0,nrow(fall))), 
                              "b_i"=as.numeric(fall[,'Catch_KG']), 
                              "a_i"=as.numeric(fall[,'AreaSwept_km2']),
                              X1_formula = X1_formula,
                              X2_formula = X2_formula,
                              covariate_data = covdata_fall,
                              optimize_args=list("lower"=-Inf,"upper"=Inf)),
                    
                    silent = TRUE)
    
  },{
    print("NOT USING COVARIATES")
    fit_fall <- try(fit_model(settings = settings,
                              "Lat_i"=as.numeric(fall[,'Lat']), 
                              "Lon_i"=as.numeric(fall[,'Lon']), 
                              "t_i"=as.numeric(fall[,'Year']), 
                              "c_iz"=as.numeric(rep(0,nrow(fall))), 
                              "b_i"=as.numeric(fall[,'Catch_KG']), 
                              "a_i"=as.numeric(fall[,'AreaSwept_km2']),
                              optimize_args=list("lower"=-Inf,"upper"=Inf)), 
                    silent = TRUE)
    
  })
  beep(sound=8) 
  
  
  
  
  
  model_aic[["fall"]][[j]] <- fit_fall$parameter_estimates$AIC
  
  #silent = TRUE might stop output in console
  
  setwd(paste(getwd(),"/obsmodel",j,"/fall",sep=""))  #set it
  
  saveRDS(fit_fall,file = paste(getwd(),"/fit_fall.RDS",sep=""))
  # plot_biomass_index(fit_fall)
  
  plot(fit_fall)
  
 # remove(fit_fall)
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/settings.txt",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/fall/settings.txt",sep=""))
  
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/parameter_estimates.txt",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/fall/parameter_estimates.txt",sep=""))
  
  file.rename(from= paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/parameter_estimates.RDATA",sep="") 
              ,to =paste(orig.dir,"/VAST/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/obsmodel",j,"/fall/parameter_estimates.RDATA",sep=""))
  
  
  
  #plot covariate respopne, if applicable 
  
  print("PLOTTING COVARIATE RESPONSE")
  pdf(file=paste(getwd(),"/",cov_used,"_cov_res_fall",".pdf",sep=""))
  fittt = fit_fall
  covariate_data_full = fittt$effects$covariate_data_full
  catchability_data_full = fittt$effects$catchability_data_full
  
  # Plot 1st linear predictor, but could use `transformation` to apply link function
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Temp"),
                           which_formula = "X1",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Habitat"),
                           which_formula = "X2",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  
  pred = Effect.fit_model( fittt,
                           focal.predictors = c("Temp"),
                           which_formula = "X2",
                           xlevels = 100,
                           transformation = list(link=identity, inverse=identity) )
  plot(pred)
  
  # 
  # pred2 = Effect.fit_model( fittt,
  #                           focal.predictors = c("Temp"),
  #                           which_formula = "X2",
  #                           xlevels = 100,
  #                           transformation = list(link=identity, inverse=identity) )
  # plot(pred2)
  # 
  # pred3 = Effect.fit_model( fittt,
  #                           focal.predictors = c("Habitat"),
  #                           which_formula = "X2",
  #                           xlevels = 100,
  #                           transformation = list(link=identity, inverse=identity) )
  # plot(pred3)
  # 
  
  #####################
  # pdp package
  #####################
  #    
  #     
  #     #might need to add yea rback in to habitat
  # #    fittt$covariate_data$Year=covdata_spring$Year
  #     
  #     library(pdp)
  #     
  #     # Make function to interface with pdp
  #     pred.fun = function( object, newdata ){
  #       predict( x=object,
  #                Lat_i = object$data_frame$Lat_i,
  #                Lon_i = object$data_frame$Lon_i,
  #                t_i = object$data_frame$t_i,
  #                a_i = object$data_frame$a_i,
  #                what = "P1_iz",
  #                new_covariate_data = newdata,
  #                do_checks = FALSE )
  #     }
  #     
  #     # Run partial
  #     Partial = partial( object = fittt,
  #                        pred.var = "Habitat",
  #                        pred.fun = pred.fun,
  #                        train = fittt$covariate_data )
  #     
  #     # Make plot using ggplot2
  #     library(ggplot2)
  #     autoplot(Partial)
  
  dev.off()
  
  remove(fittt)
  
  
  
  
  remove(fit_fall)
  
  
  #go back to scenario directory before moving to next model case
  setwd('..')
  setwd('..')
  setwd('..')
  setwd('..')
  setwd('..')
  
}


#might need to go up another directory
setwd('..')

saveRDS(model_aic, file = paste(getwd(),"/",scenario,"/Had/",str_dir,cov_dir,noise_dir,"/model_aic.RDS",sep=""))






#In the event I forgot to save model_aic above, this chunk will extract aic values from each model fit

scen <- "ConPop_ConTemp"

orig.dir <- getwd()

modl_aic <- list()

for(j in 1:4){
  for(sn in c("spring","fall")){
    
    fit <- try(readRDS(paste(orig.dir,"/VAST/",scen,"/Had/obsmodel",j,"/",sn,"/fit_",sn,".RDS",sep="") ), silent = TRUE)
    
    modl_aic[[sn]][[j]] <- try(fit$parameter_estimates$AIC, silent = TRUE)
    
  }
}


saveRDS(modl_aic, file = paste(getwd(),"/VAST/",scen,"/Had/model_aic.RDS",sep=""))





#read in old model_aic to extract values
aic <- readRDS(file = paste(getwd(),"/VAST/",scen,"/Had/model_aic.RDS",sep=""))




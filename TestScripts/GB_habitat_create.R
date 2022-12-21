#will create the "hab" object for mixfish sim using previously created habiats for each species

#for working in just R
setwd("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-MFS2/")




loadedPackages <- c("rgdal", "data.table", "maptools","envi", "raster", "RStoolbox", "spatstat.data", "spatstat.geom", "spatstat.core")
invisible(lapply(loadedPackages, library, character.only = TRUE))

#to rotate matrix before fields::image.plot
rotate <- function(x) t(apply(x, 2, rev))



# 
# #load habitat matrices previously created and store values in hab
# 
# #haddock
# Had_mat <- readRDS(file="TestScripts/Habitat_plots/Haddock/Haddock_Weighted_AdaptFalse_MATRIX.RDS")
# fields::image.plot(Had_mat)
# 
# #cod
# Cod_mat <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_MATRIX.RDS")
# fields::image.plot(Cod_mat)
# 
# #yellowtail
# Yell_mat <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/YellowtailFlounder_Weighted_AdaptFalse_MATRIX.RDS")
# fields::image.plot(Yell_mat)





#load habitat raster previously created and increase resolution
library(raster)

#haddock
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Had_ras)

#cod
Cod_ras <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Cod_ras)

#yellowtail
Yell_ras <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/Yell_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Yell_ras)


#alter resolution. 
#Yell_ras1 <- raster::aggregate(Yell_ras,fact=2) #can only use interger factor

res_factor <- .65  #amount to increase resolution
r <- raster(extent(Yell_ras), nrow = round(res_factor*nrow(Yell_ras)), ncol = round(res_factor*ncol(Yell_ras)) , crs = crs(Yell_ras))
nrow(r)

#Yellowtail
Yell_ras1 <- resample(x=Yell_ras, y=r, method="ngb")
nrow(Yell_ras1)
plot(Yell_ras1)
plot(Yell_ras)
fields::image.plot(as.matrix(Yell_ras1))

#total cells
ncol(as.matrix(Yell_ras))*nrow(as.matrix(Yell_ras))
ncol(as.matrix(Yell_ras1))*nrow(as.matrix(Yell_ras1))


#see how many zero and nonzero values there are
sum(colSums(as.matrix(Yell_ras1)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Yell_ras1)>0,na.rm = T)) #nonzero
length(as.matrix(Yell_ras1)[,1])*length(as.matrix(Yell_ras1)[1,]) #total cells including NAs

#see how many zero and nonzero values there are
sum(colSums(as.matrix(Yell_ras)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Yell_ras)>0,na.rm = T)) #nonzero
length(as.matrix(Yell_ras)[,1])*length(as.matrix(Yell_ras)[1,]) #total cells including NAs


#Cod
Cod_ras1 <- resample(x=Cod_ras, y=r, method="ngb")
plot(Cod_ras)
plot(Cod_ras1)

#see how many zero and nonzero values there are
sum(colSums(as.matrix(Cod_ras1)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Cod_ras1)>0,na.rm = T)) #nonzero
length(as.matrix(Cod_ras1)[,1])*length(as.matrix(Cod_ras1)[1,]) #total cells including NAs

#see how many zero and nonzero values there are
sum(colSums(as.matrix(Cod_ras)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Cod_ras)>0,na.rm = T)) #nonzero
length(as.matrix(Cod_ras)[,1])*length(as.matrix(Cod_ras)[1,]) #total cells including NAs


#Haddock
Had_ras1 <- resample(x=Had_ras, y=r, method="ngb")
plot(Had_ras)
plot(Had_ras1)

#see how many zero and nonzero values there are
sum(colSums(as.matrix(Had_ras1)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Had_ras1)>0,na.rm = T)) #nonzero
length(as.matrix(Had_ras1)[,1])*length(as.matrix(Had_ras1)[1,]) #total cells including NAs

#see how many zero and nonzero values there are
sum(colSums(as.matrix(Had_ras)==0,na.rm = T)) #zero
sum(colSums(as.matrix(Had_ras)>0,na.rm = T)) #nonzero
length(as.matrix(Had_ras)[,1])*length(as.matrix(Had_ras)[1,]) #total cells including NAs


#redefine final objects
Had_mat <- as.matrix(Had_ras1)
Cod_mat <- as.matrix(Cod_ras1)
Yell_mat <- as.matrix(Yell_ras1)


Had_ras <- Had_ras1
Cod_ras <- Cod_ras1
Yell_ras <- Yell_ras1



hab<- list()
hab[["hab"]][["spp1"]] <- Yell_mat / sum(Yell_mat, na.rm=T)
hab[["hab"]][["spp2"]] <- Cod_mat / sum(Cod_mat, na.rm=T)
hab[["hab"]][["spp3"]] <- Had_mat / sum(Had_mat,na.rm = T) #normalize like MFS does

#save new rasters and matrices

saveRDS(Yell_ras, file="Yell_Weighted_AdaptFalse_RASTER_res2.RDS")
saveRDS(Cod_ras, file="Cod_Weighted_AdaptFalse_RASTER_res2.RDS")
saveRDS(Had_ras, file="Had_Weighted_AdaptFalse_RASTER_res2.RDS")

saveRDS(Yell_mat, file="Yell_Weighted_AdaptFalse_MATRIX_res2.RDS")
saveRDS(Cod_mat, file="Cod_Weighted_AdaptFalse_MATRIX_res2.RDS")
saveRDS(Had_mat, file="Had_Weighted_AdaptFalse_MATRIX_res2.RDS")



#CREATE HADDOCK STRATA
strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
# get the shapefiles
strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 
#define georges bank
GB_Had_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250", "01290", "01300")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Had_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_had_strata <- strata.areas[GB_strata_idx,]
plot(GB_had_strata,main='Haddock Strata')

#CREATE COD STRATA
#define georges bank
GB_Cod_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Cod_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_cod_strata <- strata.areas[GB_strata_idx,]
plot(GB_cod_strata,main='Atlantic Cod Strata')

#CREATE YELLOWTAIL STRATA
#define georges bank
GB_Yel_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Yel_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_yell_strata <- strata.areas[GB_strata_idx,]
plot(GB_yell_strata,main='Yellowtail Flounder Strata')


#ADD "STRATA" list to hab which is used to determine how many total strata there are in Bens_init_survey
hab[["strata"]] <- GB_had_strata@data$FINSTR_ID






#load previously created rasters

#haddock
#Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Haddock_Weighted_AdaptFalse_RASTER.RDS")
plot(Had_ras)
plot(GB_had_strata,add=T)

#cod
#Cod_ras <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_RASTER.RDS")
plot(Cod_ras)
plot(GB_cod_strata,add=T)

#yellowtail
#Yell_ras <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/YellowtailFlounder_Weighted_AdaptFalse_RASTER.RDS")
plot(Yell_ras)
plot(GB_yell_strata,add=T)




###########################################################################
#create matrix with strata number inside each strata (same as hab$stratas)


#HADDOCK
all_had_strat_num <- list()

for(i in seq(length(GB_had_strata))){
  
  GB_strata_idx <- match(GB_had_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])
  
  specific_strata <- strata.areas[GB_strata_idx,]
  strat_num <- specific_strata$STR2
  #first make everything outside strata 0
  x1<- mask(Had_ras,specific_strata,updatevalue=0)
  #then make everything inside given strata the strata number
  x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)
  
  all_had_strat_num[[i]]<-x2
  
}

had_stratas <- Reduce('+',all_had_strat_num)
had_stratas<- as.matrix(had_stratas)
fields::image.plot(had_stratas)

#COD
all_cod_strat_num <- list()

for(i in seq(length(GB_cod_strata))){
  
  GB_strata_idx <- match(GB_cod_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])
  
  specific_strata <- strata.areas[GB_strata_idx,]
  strat_num <- specific_strata$STR2
  #first make everything outside strata 0
  x1<- mask(Cod_ras,specific_strata,updatevalue=0)
  #then make everything inside given strata the strata number
  x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)
  
  all_cod_strat_num[[i]]<-x2
  
}

cod_stratas <- Reduce('+',all_cod_strat_num)
cod_stratas<- as.matrix(cod_stratas)
fields::image.plot(cod_stratas)


#YELLOWTAIL FLOUNDER
all_yell_strat_num <- list()

for(i in seq(length(GB_yell_strata))){
  
  GB_strata_idx <- match(GB_yell_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])
  
  specific_strata <- strata.areas[GB_strata_idx,]
  strat_num <- specific_strata$STR2
  #first make everything outside strata 0
  x1<- mask(Yell_ras,specific_strata,updatevalue=0)
  #then make everything inside given strata the strata number
  x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)
  
  all_yell_strat_num[[i]]<-x2
  
}

yell_stratas <- Reduce('+',all_yell_strat_num)
yell_stratas<- as.matrix(yell_stratas)
fields::image.plot(yell_stratas)


#store values just created
#hab[["stratas"]] <- list(had_stratas,cod_stratas,yell_stratas)
hab[["stratas"]] <- had_stratas #just use haddock because it contains others

###########################################################################















###########################################################################
# NEED TO DEFINE SPAWNING GROUNDS

source("R/create_spawn_hab_Bens.R")
source("R/define_spawn_Bens.R")

#yellowtail in May (weeks 9, 10, 11, 12)


max(hab$hab$spp1,na.rm=T)  #max is 0.0006653

YT_spwn_ind <-which(hab$hab$spp1 >= 0 , arr.ind=T) #4,279 total non NA cells
YT_spwn_ind <-which(hab$hab$spp1 > 0 , arr.ind=T)  #3,110 are >0
YT_spwn_ind <-which(hab$hab$spp1 >= .0006 , arr.ind=T) #832 are above .0006

#will use southwest red area and northeast red area for spawning
#northeast between rows 40-80 and columns 155-196 
#use .0002 in NE corner 
YT_spwn_ind <-which(hab$hab$spp1 >= .0006 , arr.ind=T) #832 are above .0006
YT_spwn_NE <- YT_spwn_ind[(YT_spwn_ind[,1]>=34) & (YT_spwn_ind[,1]<=60) & (YT_spwn_ind[,2]>=95) & (YT_spwn_ind[,2]<=128), ]


#will use southwest red area and northeast red area for spawning
#SW between rows 96 to 127 and columns 50 to 82 
#use .0001 in SW corner 
YT_spwn_ind <-which(hab$hab$spp1 >= .0002 , arr.ind=T) #3,833 are above .0001
YT_spwn_SW <- YT_spwn_ind[(YT_spwn_ind[,1]>=62) & (YT_spwn_ind[,1]<=83) & (YT_spwn_ind[,2]>=33) & (YT_spwn_ind[,2]<=53), ]

YT_spwn <- rbind(YT_spwn_NE,YT_spwn_SW)

spwn_mult <- 10
YT_spwn_hab <- create_spawn_hab_Bens(hab = hab$hab$spp1, spwnareas = YT_spwn, mult = spwn_mult)
fields::image.plot(rotate(YT_spwn_hab))



#cod in weeks 8-13

max(hab$hab$spp2,na.rm=T)  #max is 0.000713

Cod_spwn_ind <-which(hab$hab$spp2 >= 0 , arr.ind=T) #5,999 total non NA cells
Cod_spwn_ind <-which(hab$hab$spp2 > 0 , arr.ind=T)  #3,710 are >0
Cod_spwn_ind <-which(hab$hab$spp2 >= .0007 , arr.ind=T) #514 are above .0007

#will use northeast area between rows 0-44 and columns 60-144
#use .0002 in NE corner 
Cod_spwn_ind <-which(hab$hab$spp2 >= .0007 , arr.ind=T) #832 are above .0006
Cod_spwn_NE <- Cod_spwn_ind[(Cod_spwn_ind[,1]>=0) & (Cod_spwn_ind[,1]<=44) & (Cod_spwn_ind[,2]>=90) & (Cod_spwn_ind[,2]<=144), ]


spwn_mult <- 10
Cod_spwn_hab <- create_spawn_hab_Bens(hab = hab$hab$spp2, spwnareas = Cod_spwn_NE, mult = spwn_mult)
fields::image.plot(rotate(Cod_spwn_hab))



#haddock in weeks 11-14

max(hab$hab$spp3,na.rm=T)  #max is 0.000355

Had_spwn_ind <-which(hab$hab$spp3 >= 0 , arr.ind=T) #7,557 total non NA cells
Had_spwn_ind <-which(hab$hab$spp3 > 0 , arr.ind=T)  #5,830 are >0
Had_spwn_ind <-which(hab$hab$spp3 >= .0003 , arr.ind=T) #1431 are above .0003

#will use northeast area between rows 0-44 and columns 60-144
#use .0002 in NE corner 
Had_spwn_ind <-which(hab$hab$spp3 >= .0003 , arr.ind=T) 
Had_spwn_NE <- Had_spwn_ind[(Had_spwn_ind[,1]>=20) & (Had_spwn_ind[,1]<=36) & (Had_spwn_ind[,2]>=90) & (Had_spwn_ind[,2]<=144), ]


#will use great south channel
#SW between rows 20 to 50  and columns 25 to 45 

Had_spwn_ind <-which(hab$hab$spp3 >= .00009 , arr.ind=T) 
Had_spwn_SW <- Had_spwn_ind[(Had_spwn_ind[,1]>=35) & (Had_spwn_ind[,1]<=50) & (Had_spwn_ind[,2]>=20) & (Had_spwn_ind[,2]<=45), ]

Had_spwn <- rbind(Had_spwn_NE,Had_spwn_SW)


spwn_mult <- 10
Had_spwn_hab <- create_spawn_hab_Bens(hab = hab$hab$spp3, spwnareas = Had_spwn, mult = spwn_mult)
fields::image.plot(rotate(Had_spwn_hab))






hab[["spwn_hab"]] <- list()
hab[["spwn_hab"]][["spp1"]] <- YT_spwn_hab 
hab[["spwn_hab"]][["spp2"]] <- Cod_spwn_hab 
hab[["spwn_hab"]][["spp3"]] <- Had_spwn_hab  



#save for later use
saveRDS(hab, file="hab_GB_3species.RDS")


# 
# 
# #CREATE HABITAT WITH FEWER SPECIES FOR TEST
# temp <- hab
# hab <- list()
# hab[["hab"]] <- temp$hab$spp3
# hab$strata <- temp$strata
# hab$stratas <- temp$stratas
# hab$spwn_hab <- temp$spwn_hab$spp3
# saveRDS(hab, file="hab_justYT2.RDS")
# 





#load habitat
hab <- readRDS("hab_GB_3species.RDS")




#INTEGRATE WITH TEMP GRADIENT

#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

#increasing temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata_res2")


#order: , Yellowtail, Cod, Haddock
tol_list <- list("spp1" = list("mu" = 9, "va" = 4),  #Yellowtail
                 "spp2" = list("mu" = 8.75, "va" = 4.25),  #Cod
                 "spp3" = list("mu" = 9, "va" = 4) )    #Haddock




spp_names <- c("Yellowtail Flounder","Cod","Haddock")

spp_names_short <- c("YTF","COD","HAD")

month_nm <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")




#1) PLOTTING JUST TEMPERATURE OVER TIME




##########################################################
#OLD WAY USING IMAGE.PLOT
##########################################################


#plot increasing temp gradient over time similar to how 
#plot_spatiotemp_hab_justtemp works, but without species-specific 
#influences


yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

pdf(file=paste0('testfolder/Monthly_temp_plots','.pdf'))

#figure out max/min temp to set color limits below
zmax <- max(unlist(lapply(moveCov$cov.matrix,FUN=max, na.rm=T)))
zmin <- min(unlist(lapply(moveCov$cov.matrix,FUN=min, na.rm=T)))



#plot same week on each page. GOOD FOR INCREASING TEMP SITUATION
for(k in seq(12)){
  
  par(mfrow = c(5,4),mar = c(1, 1, 1, 1))
  
  
  for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
    
    month_shift <- 4*(k-1)
    
    
    temp_rotate <- rotate(moveCov$cov.matrix[[i+month_shift]])
    
    fields::image.plot(temp_rotate, zlim = c(zmin,zmax))
    
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste( month_nm[floor(i/(13/3))+1] ,'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
    
    
  }
  
  
}





  for(i in seq(52)){
 
    
     par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
  
  

    
    temp_rotate <- rotate(moveCov$cov.matrix[[i]])
    
    fields::image.plot(temp_rotate, zlim = c(zmin,zmax))
    
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste( 'Week', (i)), cex = 1)
    
    
  }
  
  

dev.off()







##########################################################
#NEW WAY USING GGPLOT
##########################################################


#plot increasing temp gradient over time similar to how 
#plot_spatiotemp_hab_justtemp works, but without species-specific 
#influences


yearscut <- 2

pdf(file=paste0('testfolder/Monthly_temp_plots','.pdf'))

#figure out max/min temp to set color limits below
zmax <- max(unlist(lapply(moveCov$cov.matrix,FUN=max, na.rm=T)))
zmin <- min(unlist(lapply(moveCov$cov.matrix,FUN=min, na.rm=T)))


surv_temp1 <- list()

#plot same week on each page. GOOD FOR INCREASING TEMP SITUATION
for(k in seq(12)){
all_idx <- 1
  
  for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
    
    month_shift <- 4*(k-1)
    
    
    temp_rotate <- moveCov$cov.matrix[[i+month_shift]]
    

    temp_ <- reshape2::melt(temp_rotate, c("x", "y"), value.name = "Temperature") #temperature
    
    surv_temp1[[all_idx]] <-  ggplot() +
      geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
      scale_fill_gradientn(colours=c("yellow","red"),limits = range(zmin, zmax))+ #set the color pallet and color limits
      theme_void()+ #remove x and y axis ticks and labels
      #labs(x="lat", y="lon",title=paste('Week', (i+month_shift)%%52, 'Year', ceiling((i+month_shift)/52))) +
      theme(legend.position="none" ) #remove legend
    
    
    all_idx<- all_idx+1

    
    
  }
  
      
  do.call("grid.arrange", c(surv_temp1, ncol=4, top=paste(spp_names_short[s],  'Month', i)))
}





for(i in seq(52)){
  
  
  
  
  temp_rotate <- moveCov$cov.matrix[[i]]
  
  
  temp_ <- reshape2::melt(temp_rotate, c("x", "y"), value.name = "Temperature") #temperature
  
    p <-  ggplot() +
    geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
    scale_fill_gradientn(colours=c("yellow","red"),limits = range(zmin, zmax))+ #set the color pallet and color limits
    theme_void()+ #remove x and y axis ticks and labels
    labs(x="lat", y="lon",title=paste('Week', ((i+month_shift)%%52 + 1))) +
    theme(plot.title = element_text(hjust = 0.5),legend.position="none" ) #remove legend
  
    print(p)
  
}



dev.off()








spp_names <- c("Yellowtail Flounder","Cod","Haddock")

spp_names_short <- c("YTF","COD","HAD")

month_nm <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#deifne spawning weeks (made up for now)
spwn_wk = list("spp1" = 9:12, "spp2" = 8:13, "spp3" = 11:14  )



#order: , Yellowtail, Cod, Haddock
tol_list <- list("spp1" = list("mu" = 9, "va" = 4),  #Yellowtail
                 "spp2" = list("mu" = 8.75, "va" = 4.25),  #Cod
                 "spp3" = list("mu" = 9, "va" = 4) )    #Haddock




################################################################################

#2A) PLOTTING SPECIES-SPECIFIC TEMPERATURE OVER TIME CONSTANT TEMP
#ie, applying each species temp preferences to temp gradient



##########################################################
#OLD WAY USING IMAGE.PLOT
##########################################################

#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata_res2")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list


library(MixFishSim)

yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))





pdf(file=paste0('testfolder/Monthly_species_temp_plots_ConstTemp','.pdf'))


maxtemp1 <-vector()
mintemp1<-vector()
maxtemp <- vector()
mintemp <- vector()


#obtain zlim bounds from maxtemp
zmax <- c(.25,.25,.26)
zmin <- c(0,0,0)

#PLOT EACH SPECIES in groups
for(s in seq_len(length(hab[["hab"]]))) {
  
  
  
  
  par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    #par( mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate( move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F,zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]))
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate( move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]) )
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste(spp_names_short[s],  month_nm[floor(i/(13/3))+1] , 'Week', (i)%%52), cex = 1)
    
    
    
  }
  
}



#PLOT EACH ON OWN PAGE
for(s in seq_len(length(hab[["hab"]]))) {
  
  
  # par(mfrow = c(10,6), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    par( mfrow = c(1,1), mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F, zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]))
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate( move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]) )
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))  month_nm[floor(i/(13/3))+1] ,
    text(0.5, 0.98, labels = paste(spp_names[s],  'Week', (i)%%52), cex = 1)
    
    maxtemp1[i]<- max(temp_rotate,na.rm=T)
    mintemp1[i]<- min(temp_rotate,na.rm=T)
    
  }
  
  maxtemp[s] <- max(maxtemp1)
  mintemp[s] <- min(mintemp1)
  
}




dev.off()




#############################################
#NEW WAY USIN GGPLOT
#############################################

library(ggplot2)
library(gridExtra)
library(plotly)

#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata_res2")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list


library(MixFishSim)

yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))





pdf(file=paste0('testfolder/Monthly_species_temp_plots_ConstTemp','.pdf'))


surv_temp1 <- list()


#obtain zlim bounds from maxtemp
zmax <- c(.25,.25,.26)
zmin <- c(0,0,0)

#PLOT EACH SPECIES in groups
for(s in seq_len(length(hab[["hab"]]))) {
  
  all_idx<- 1
  
  
  for(i in seq(1,52)){
    #par( mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
 
    
    temp_ <- reshape2::melt(move_cov_wk_spp, c("x", "y"), value.name = "Temperature") #temperature
    
    surv_temp1[[all_idx]] <-  ggplot() +
      geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
      scale_fill_gradientn(colours=c("yellow","red"),limits = range(0, zmax[[s]]))+ #set the color pallet and color limits
      theme_void()+ #remove x and y axis ticks and labels
      #labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', i )) +
      theme(plot.title = element_text(hjust = 0.5),legend.position="none" ) #remove legend
    
    
    
    
    all_idx<- all_idx+1

    
  }
  
  
  do.call("grid.arrange", c(surv_temp1, ncol=4, top=paste(spp_names_short[s],  'Month', i)))
  # gridExtra::
  
}



#PLOT EACH ON OWN PAGE
for(s in seq_len(length(hab[["hab"]]))) {
  
  all_idx<- 1
  
  
  
  for(i in seq(1,52)){
  
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))

    
    
    temp_ <- reshape2::melt(move_cov_wk_spp, c("x", "y"), value.name = "Temperature") #temperature
 
   p <- ggplot() +
      geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
     scale_fill_gradientn(colours=c("yellow","red"),limits = range(0, zmax[[s]]))+ #set the color pallet and color limits
      theme_void()+ #remove x and y axis ticks and labels
      labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', i )) +
      theme(plot.title = element_text(hjust = 0.5),legend.position="none" ) #remove legend
     
   print(p)
    
    
  }
  

  
}




dev.off()









################################################################################
#2B) PLOTTING SPECIES-SPECIFIC TEMPERATURE OVER TIME VARRYING TEMP
#ie, applying each species temp preferences to temp gradient




#############################################
#OLD WAY USIN IMAGE.PLOT
#############################################


#load increasing temp gradient
#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata")

moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata_res2")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list





yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
zmax <- c(.25,.25,.26)
zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_IncrTemp','.pdf'))






for(s in seq_len(length(hab[["hab"]]))) {
  
  
  for(k in seq(12)){
    
    
    par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
    
    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      #col = grey(seq(1,0,l = 51)), 
      if(!i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate( move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F, zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]))
      }
      # col = grey(seq(1,0,l = 51)),
      if(i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate( move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]), col = c(rev(heat.colors(50))[5:50]) )
      }
      #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste(spp_names_short[s], month_nm[floor(i/(13/3))+1] , 'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
      
      
      
    }
    
    
  }
}


dev.off()








#############################################
#NEW WAY USIN GGPLOT
#############################################


#load increasing temp gradient
#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata")

moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata_res2")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list


yearscut <- 2

#trying same zlim as above, max values may need to be extended
zmax <- c(.25,.25,.26)
zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_IncrTemp','.pdf'))


surv_temp1 <- list()

#plot all on same page for comparison 

for(s in seq_len(length(hab[["hab"]]))) {
  
  all_idx <-1
  
  for(k in seq(12)){
    
    
  
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      
        temp_ <- reshape2::melt(move_cov_wk_spp, c("x", "y"), value.name = "Temperature") #temperature
        
        surv_temp1[[all_idx]] <- ggplot() +
          geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
          scale_fill_gradientn(colours=c("yellow","red"),limits = range(0, zmax[[s]]))+ #set the color pallet and color limits
          theme_void()+ #remove x and y axis ticks and labels
          #labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', i )) +
          theme(legend.position="none" ) #remove legend
        
         all_idx <- all_idx +1
      
    }
    
    
  }
  
  
  do.call("grid.arrange", c(surv_temp1, ncol=4, top=paste(spp_names_short[s],  'Month', i)))
  
}


#plot each on own page for videos

for(s in seq_len(length(hab[["hab"]]))) {
  

  
  for(k in seq(12)){
    
    
    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      
      temp_ <- reshape2::melt(move_cov_wk_spp, c("x", "y"), value.name = "Temperature") #temperature
      
      p <- ggplot() +
        geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Temperature)) + 
        scale_fill_distiller(palette = "Spectral",limits = range(0, zmax[[s]]))+ #set the color pallet and color limits
        theme_void()+ #remove x and y axis ticks and labels
        labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', i%%52,'Year', ceiling((i+month_shift)/52)) ) +
        theme(legend.position="none" ) #remove legend
      
      print(p)
      
    }
    
    
  }
  
}




dev.off()










######################################################################################


#3A) PLOTTING SPECIES-SPECIFIC HABITAT OVER TIME WITH CONSTANT TEMP GRADIENT
#ie, applying each species temp preferences to temp gradient and combining with habitat preference



#load increasing temp gradient
#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")


moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata_res2")


#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list





yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
#zmax <- c(.23,.22,.26)
#zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_HabTemp_ConstTemp','.pdf'))



maxtemp1 <-vector()
mintemp1<-vector()
maxtemp <- vector()
mintemp <- vector()


#PLOT EACH SPECIES in groups

for(s in seq_len(length(hab[["hab"]]))) {
  
  
  
  par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    
    
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]]^2 * move_cov_wk_spp)
      temp_rotate <- temp_rotate/sum(temp_rotate,na.rm=T)
      #print(sum(temp_rotate,na.rm=T))
      
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F , col = c("#4e83ed",rev(heat.colors(50))[5:50]))
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["spwn_hab"]][[paste0('spp',s)]]^2 * move_cov_wk_spp)
      temp_rotate <- temp_rotate/sum(temp_rotate,na.rm=T)
      #print(sum(temp_rotate))
      
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F , col = c("#4e83ed",rev(heat.colors(50))[5:50]))
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste(spp_names_short[s], month_nm[floor(i/(13/3))+1] , 'Week', (i)%%52), cex = 1)
    
    
    
  }
  
  
}



#PLOT EACH ON OWN PAGE
for(s in seq_len(length(hab[["hab"]]))) {
  
  
  # par(mfrow = c(10,6), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    par( mfrow = c(1,1), mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]]^2 *move_cov_wk_spp)
      fields::image.plot(temp_rotate/sum(temp_rotate,na.rm=T), cex.axis = 1.5, cex.main = 2, axes = F, col = c("#4e83ed",rev(heat.colors(50))[5:50]))
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["spwn_hab"]][[paste0('spp',s)]]^2 * move_cov_wk_spp)
      fields::image.plot(temp_rotate/sum(temp_rotate,na.rm=T), cex.axis = 1.5, cex.main = 1, axes = F , col = c("#4e83ed",rev(heat.colors(50))[5:50]))
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste(spp_names[s], 'Week', (i)%%52), cex = 1)
    
    maxtemp1[i]<- max(temp_rotate,na.rm=T)
    mintemp1[i]<- min(temp_rotate,na.rm=T)
    
  }
  
  maxtemp[s] <- max(maxtemp1)
  mintemp[s] <- min(mintemp1)
  
}




dev.off()





















######################################################################################


#3B) PLOTTING SPECIES-SPECIFIC HABITAT OVER TIME WITH INCREASING TEMP GRADIENT
#ie, applying each species temp preferences to temp gradient and combining with habitat preference



#load increasing temp gradient
#constant temp gradient
#moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata")

moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata_res2")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- tol_list




yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
#zmax <- c(.23,.22,.26)
#zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_HabTemp_IncrTemp','.pdf'))








for(s in seq_len(length(hab[["hab"]]))) {
  
  
  for(k in seq(12)){
    
   #uncomment below to have weeks 13 and 37 on own page (used this to create video)
     ifelse(((k==4)|(k==10)), par(mfrow = c(1,1), mar = c(1, 1, 1, 1)), par(mfrow = c(5,4), mar = c(1, 1, 1, 1)))
    
    #uncomment below to have all weeks in 5x4 grid on each page
    # par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
    
    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      #col = grey(seq(1,0,l = 51)), 
      if(!i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]]^2 * move_cov_wk_spp)
        fields::image.plot(temp_rotate/sum(temp_rotate,na.rm=T), cex.axis = 1.5, cex.main = 2, axes = F, col = c("#4e83ed",rev(heat.colors(50))[5:50]) )
      }
      # col = grey(seq(1,0,l = 51)),
      if(i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate(hab[["spwn_hab"]][[paste0('spp',s)]]^2 * move_cov_wk_spp)
        fields::image.plot(temp_rotate/sum(temp_rotate,na.rm=T), cex.axis = 1.5, cex.main = 1, axes = F, col =c("#4e83ed",rev(heat.colors(50))[5:50]) )
      }
      #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
      
      
      
    }
    
    
  }
}


dev.off()













######################################################################################


#4) PLOTTING ACTUAL SPECIES-SPECIFIC MODEL OUTPUT FROM A GIVEN SIMULATION WITH SURVEY SAMPLES ON TOP
# ALSO PLOTTING SURVEY VALUES ON TOP OF COVARIATES AS WELL
#ie, loading simulation output and copying above loops to plot them


spp_names <- c("Yellowtail Flounder","Cod","Haddock")

spp_names_short <- c("YT","Cod","Had")

month_nm <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#strata that each species occupies. Used to calculate stratified random mean of each
strata_species <- list()
strata_species[["YT"]] <-  c(13,14,15,16,17,18,19,20,21)
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
strata_species[["Had"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)


#load results from a given simulation
scenario <- "IncPop_ConTemp"

######################################################################################
#choose which simulation iteration to use (chosen in different file)
good_iter <- c(1,13,6) #ConPop_ConTemp
good_iter <- c(1,1,3) #ConPop_IncTemp
good_iter <- c(77,63,98) #IncPop_ConTemp
good_iter <- c(77,44,100) #IncPop_IncTemp
good_iter <- c(25,18,6) #DecPop_ConTemp
good_iter <- c(13,44,9) #DecPop_IncTemp
######################################################################################

######################################################################################
# #THESE FOR CONPOP_CONTEMP
color_max[["YT"]][[4]] <- 205  #spring survey pop max
color_max[["YT"]][[10]] <- 80 #fall survey
color_max[["Cod"]][[4]] <- 170  #spring survey pop max
color_max[["Cod"]][[10]] <- 335 #fall survey
color_max[["Had"]][[4]] <- 1525  #spring survey pop max
color_max[["Had"]][[10]] <-1273  #fall survey

#THESE FOR CONPOP_INCTEMP
color_max[["YT"]][[4]] <- 125  #spring survey pop max
color_max[["YT"]][[10]] <- 200 #fall survey
color_max[["Cod"]][[4]] <- 437  #spring survey pop max
color_max[["Cod"]][[10]] <- 3044 #fall survey
color_max[["Had"]][[4]] <- 3945  #spring survey pop max
color_max[["Had"]][[10]] <-3775  #fall survey

#THESE FOR INCPOP_CONTEMP
color_max[["YT"]][[4]] <- 405  #spring survey pop max
color_max[["YT"]][[10]] <- 343 #fall survey 
color_max[["Cod"]][[4]] <- 637  #spring survey pop max
color_max[["Cod"]][[10]] <- 1550 #fall survey 
color_max[["Had"]][[4]] <- 2645  #spring survey pop max
color_max[["Had"]][[10]] <-1980  #fall survey 

#THESE FOR INCPOP_INCTEMP
color_max[["YT"]][[4]] <- 957  #spring survey pop max
color_max[["YT"]][[10]] <- 1571 #fall survey 
color_max[["Cod"]][[4]] <- 922  #spring survey pop max
color_max[["Cod"]][[10]] <- 7398 #fall survey 
color_max[["Had"]][[4]] <- 9662  #spring survey pop max
color_max[["Had"]][[10]] <-9857  #fall survey 

#THESE FOR DECPOP_CONTEMP
color_max[["YT"]][[4]] <- 79  #spring survey pop max
color_max[["YT"]][[10]] <- 70 #fall survey 
color_max[["Cod"]][[4]] <- 62  #spring survey pop max
color_max[["Cod"]][[10]] <- 149 #fall survey 
color_max[["Had"]][[4]] <- 861  #spring survey pop max
color_max[["Had"]][[10]] <-593  #fall survey 

#THESE FOR DECPOP_INCTEMP
color_max[["YT"]][[4]] <- 96  #spring survey pop max
color_max[["YT"]][[10]] <- 86 #fall survey 
color_max[["Cod"]][[4]] <- 86  #spring survey pop max
color_max[["Cod"]][[10]] <- 486 #fall survey 
color_max[["Had"]][[4]] <- 1016  #spring survey pop max
color_max[["Had"]][[10]] <-1199  #fall survey 
######################################################################################


#survey results without noise
list_all_temp <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\list_all_",scenario,".RDS",sep=""))

list_all <- list()
list_all[["YT"]] <- list_all_temp[[good_iter[1]]]
list_all[["Cod"]] <- list_all_temp[[good_iter[2]]]
list_all[["Had"]] <- list_all_temp[[good_iter[3]]]

#simulation results (LOAD JUST ONE OF THE FOLLOWING)
#memory.limit(45000) #this one for all results
#result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\result_",scenario,".RDS",sep=""))
#load existing result_goodones, if it exists
result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\result_goodones_",scenario,".RDS",sep=""))

#load random survey locations used in this scenario
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))



#create matrix with survey values as points to add to plots later
survey_points <- list()
survey_points[["YT"]] <- vector("list",length(seq(3,22)))
survey_points[["Cod"]] <- vector("list",length(seq(3,22)))
survey_points[["Had"]] <- vector("list",length(seq(3,22)))


for(s in spp_names_short){
  for(yr in seq(3,22)){
    survey_points[[s]][[yr]] <- list()
    for(wk in c(13,37)){
  #grab survey results from first week in each season 
 #old way for image.plot
#test <- matrix(NA, nrow = length(result[[good_iter[[1]]]]$pop_bios[[1]][[1]][,1]),ncol = length(result[[good_iter[[1]]]]$pop_bios[[1]][[1]][1,]))

#new way for ggplot
survey_points[[s]][[yr]][[wk]] <- data.frame()
idx=1

for(i in seq(length(list_all[[s]][,1]))){

  if((as.numeric(list_all[[s]][i,"stratum"])%in%strata_species[[s]])&(as.numeric(list_all[[s]][i,"week"])==wk)&(as.numeric(list_all[[s]][i,"year"])==yr)){
  
    #old way using image.plot
#  test[as.numeric(list_all[[s]][i,2]),as.numeric(list_all[[s]][i,3])] <- 1#list_all[[s]][i,7] #col = 7 is recording the year
 
  survey_points[[s]][[yr]][[wk]][idx,"x"] <- as.numeric(list_all[[s]][i,"x"])
  survey_points[[s]][[yr]][[wk]][idx,"y"] <- as.numeric(list_all[[s]][i,"y"])
  survey_points[[s]][[yr]][[wk]][idx,"survey"] <- as.numeric(list_all[[s]][i,"stratum"])
  idx=idx+1
  }
  }
#old way
#survey_points[[s]][[yr]][[wk]] <- test
#new way

}
  }

}

yearscut <- 2

n_spp <- 3  #for loop length

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
#zmax <- c(.23,.22,.26)
#zmin <- c(0,0,0)


#for loop length and covariates
tmp <- substr(scenario,8,10)
temp_color_max <- vector()

if(tmp == "Con"){moveCov <- readRDS(paste("20 year moveCov matrices/GeorgesBank/GB_22yr_",tmp,"stTemp_HaddockStrata_res2",sep=""))
                temp_color_max[4] <- 10.1
                temp_color_max[10] <- 18.9} #for plotting

if(tmp == "Inc"){moveCov <- readRDS(paste("20 year moveCov matrices/GeorgesBank/GB_22yr_",tmp,"rTemp_HaddockStrata_res2",sep=""))
                temp_color_max[4] <- 15.4
                temp_color_max[10] <- 24.5} #for plotting

#temp tolerances
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("YT" = list("mu" = 9, "va" = 4),  #Yellowtail
                             "Cod" = list("mu" = 8.75, "va" = 4.25),  #Cod
                             "Had" = list("mu" = 9, "va" = 4) )    #Haddock


library(raster)
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

hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution
names(hab$hab) <- c("YT","Cod","Had")
fields::image.plot(rotate(hab$hab$Had))

library(ggplot2)
library(gridExtra)
library(plotly)

# 
# temp_ <- reshape2::melt(temp_rotate, c("x", "y"), value.name = "biomass")
# temp_2 <- survey_points[[s]][[yr]][[wk]]
# 
# ggplot(temp_,aes(x=y,y=rev(x))) +
#   geom_raster(aes(fill=biomass)) +
#   geom_point(data=temp_2,aes(color = survey), shape = 19, size = 3, color="red") +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)))


  #labs(x="letters", y="LETTERS", title="Matrix")

#obtain zlim bounds from maxtemp
# range(result[[good_iter[[s]]]]$pop_bios)
# zmax <- c(.25,.25,.26)
# zmin <- c(0,0,0)


pdf(file=paste0('testfolder/Survey_Months_Plots_',scenario,'.pdf'))


#record max population values to set below color limits. run below loop without plotting to get max values first
pop_max <- matrix(nrow=40,ncol=3)
color_min <- 0 


temp_max <- matrix(nrow=40,ncol=3)
temp_color_min <- 0 





for(s in seq_len(n_spp)) {
  
      surv_temp1 <- list() #FOR STORING PLOTS FOR SURVEYS POINTS OVER POPULATION VALUES
      surv_temp2 <- list() #FOR STORING PLOTS FOR SURVEYS POINTS OVER HABITAT COVARIATES
      surv_temp3 <- list()#FOR STORING PLOTS FOR SURVEYS POINTS OVER TEMPERATURE COVARIATE
      
      all_idx<- 1
      
  #only plot survey months
  for(k in c(4,10)){
    
    #uncomment below to have weeks 13 and 37 on own page (used this to create video)
    #ifelse(((k==4)|(k==10)), par(mfrow = c(1,1), mar = c(1, 1, 1, 1)), par(mfrow = c(5,4), mar = c(1, 1, 1, 1)))
    
    #uncomment below to have all weeks in 5x4 grid on each page
     #par(mfrow = c(5,4), mar = c(.5, .5, .5, .5))
    
    yr_idx<- 1

    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
 
      #col = grey(seq(1,0,l = 51)), 
        
        month_shift <- 4*(k-1) #puts us on end of previous month
      
        #FIRST WEEK in SURVEY MONTH
        
        #new way with ggplot
        #print((i+month_shift)%%52)  #PRINTS 13 THEN 37, THE WEEKS BEING SAMPLED
      #    
        
        
        #STORING PLOTS FOR SURVEYS POINTS OVER POPULATION VALUES   
        temp_rotate <- result[[good_iter[[s]]]]$pop_bios[[i+month_shift]][[s]]
        
        temp_ <- reshape2::melt(temp_rotate, c("x", "y"), value.name = "Biomass") #population biomass
        temp_2 <- survey_points[[s]][[ceiling(i/52)]][[month_shift+1]] #survey locations
        
        pop_max[all_idx,s] <- max(temp_[,3],na.rm=T)
        
    
     surv_temp1[[yr_idx]] <- ggplot() +
          geom_raster(data=temp_,aes(x=y,y=rev(x),fill=Biomass)) + #plot biomass
          geom_point(data=temp_2,aes(x=y,y=88-x), shape = 19, size = .25, color="black") + #add survey points
          scale_fill_distiller(palette = "Spectral",limits = range(0.000000000000000000000000000000000000000000000000000000001, color_max[[spp_names_short[[s]]]][[k]])) + #set the color pallet and color limits
          theme_void()+ #remove x and y axis ticks and labels
          #labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52))) +
         theme(legend.position="none" ) #remove legend
     
          
     #STORING PLOTS FOR SURVEYS POINTS OVER HABITAT COVARIATES 
     #set habitat
     if(s==1){hab_ras = YT_ras}
     if(s==2){hab_ras = Cod_ras}
     if(s==3){hab_ras = Had_ras}
     
     temp_ras <- reshape2::melt(as.matrix(hab_ras), c("x", "y"), value.name = "Habitat") #population biomass
    
     surv_temp2[[yr_idx]] <- ggplot() +
       geom_raster(data=temp_ras,aes(x=y,y=rev(x),fill=Habitat)) + #plot biomass
       geom_point(data=temp_2,aes(x=y,y=88-x), shape = 19, size = .25, color="black") + #add survey points
       scale_fill_distiller(palette = "Spectral") + #set the color pallet and color limits
       theme_void()+ #remove x and y axis ticks and labels
       #labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52))) +
       theme(legend.position="none" ) #remove legend
     
     
     
          
     #STORING PLOTS FOR SURVEYS POINTS OVER TEMPERATURE COVARIATES
     move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
     
     # move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
     #                           nr = nrow(move_cov_wk), 
     #                           sapply(move_cov_wk, norm_fun, 
     #                                  mu = moveCov[["spp_tol"]][[s]][["mu"]], 
     #                                  va = moveCov[["spp_tol"]][[s]][["va"]]))
     
     temp_temp <- reshape2::melt(move_cov_wk, c("x", "y"), value.name = "Temperature") #temperature
     
     temp_max[all_idx,s] <- max(temp_temp[,3],na.rm=T)

     surv_temp3[[yr_idx]] <- ggplot() +
       geom_raster(data=temp_temp,aes(x=y,y=rev(x),fill=Temperature)) + #plot biomass
       geom_point(data=temp_2,aes(x=y,y=88-x), shape = 19, size = .25, color="black") + #add survey points
       scale_fill_distiller(palette = "Spectral",limits = range(0, temp_color_max[k])) + #set the color pallet and color limits
       theme_void()+ #remove x and y axis ticks and labels
       #labs(x="lat", y="lon",title=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52))) +
       theme(legend.position="none" ) #remove legend
     
       
      #print(p)
      
      #SECOND WEEK in SURVEY MONTH
        
      
        all_idx<- all_idx+1
        yr_idx<- yr_idx+1
        #old way with image.plot
      #   temp_rotate <- rotate(result[[good_iter[[s]]]]$pop_bios[[i+month_shift]][[s]])
      #   fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F, col = c("#4e83ed",rev(heat.colors(50))[5:50]) )
      #   
      #   fields::image.plot(rotate(survey_points[[s]][[yr]][[wk]]),add=T,legend.shrink=0)
      # 
      # #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      # #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      # text(0.5, 0.98, labels = paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
      # 
      
      
    }

    do.call("grid.arrange", c(surv_temp1, ncol=4, top=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'All 20 Years Biomass')))
    do.call("grid.arrange", c(surv_temp2, ncol=4, top=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'All 20 Years Habitat')))
    do.call("grid.arrange", c(surv_temp3, ncol=4, top=paste(spp_names_short[s],  'Week', (i+month_shift)%%52,'All 20 Years Temperature')))

    # gridExtra::grid.arrange(Obsmodel_plot[[1]],Obsmodel_plot[[2]],Obsmodel_plot[[3]],nrow=3)
    # 
    
  }
}


dev.off()




max(pop_max[1:20,1])  #spring values
max(pop_max[21:40,1]) #fall values

max(pop_max[1:20,2])
max(pop_max[21:40,2])

max(pop_max[1:20,3])
max(pop_max[21:40,3])

max(temp_max[1:20,1])
max(temp_max[21:40,1])









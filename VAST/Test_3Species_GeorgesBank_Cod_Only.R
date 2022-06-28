#Using GBYT_VAST.R from Chris as template 

library(raster)
library(sp)
library(TMB)
library(VAST)
library(dplyr)
library(ggplot2)

#FIRST READ IN SURVEY VALUES AND ADD COLUMNS THAT CONVERY X,Y INTO LAT,LON


################################################################################
#read in sample survey
surv_random_sample <- readRDS(file="surv_random_sample.RDS")
surv_random_sample <- as.matrix(surv_random_sample,ncol= 12)

scenario <- "ConPop_IncTemp"
#survey results without noise
list_all <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\list_all_",scenario,".RDS",sep=""))
surv_random_sample <- list_all[[1]]
################################################################################

#read in habitat matrix
hab <- readRDS(file="hab_GB_3species.RDS") #courser resolution

#read in GB strata

#haddock contains all stratas used
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Had_ras)

#translate habitat matrix back into raster
hab_ras <-raster(hab$hab$spp3)
extent(hab_ras) <- extent(Had_ras)
plot(hab_ras)

#ADD COLUMNS TO SURVEY THAT CONTAIN LAT/LON INFORMATION

#longitude is NS. These are X values or rows
#obtained via longitude = yFromRow(raster,row = ) 

#latitude is EW, These are Y values or columns
#obtained via latitude = xFromCol(raster,col= )

lat <- vector()
lon <- vector()

for(i in seq(length(surv_random_sample[,1]))){
  
  rw <- as.numeric(surv_random_sample[i,"x"])  #x in col 2
  cl <- as.numeric(surv_random_sample[i,"y"]) #y in col 3
  
  lon[i] <- xFromCol(hab_ras, col = cl)
  lat[i] <- yFromRow(hab_ras, row = rw)
  
}


library(spatstat.geom)
#plot lat and lon coordinates to check that they look correct
all_points <- spatstat.geom::ppp(x=lon,y=lat, marks = surv_random_sample[,"stratum"] , window=owin(c(-70.99, -65) ,c(40,43)))
plot(all_points, use.marks=T) #here we see the stratas appear which makes me feel like it worked


#add columns to survey table
surv_random_sample <- cbind(surv_random_sample,lat,lon)
colnames(surv_random_sample) <- c("station_no","x","y","stratum","day","tow","year","YTF","Cod","Had","week","Season","Lat","Lon")


#following from Chris' file...

adios <- as.data.frame(surv_random_sample)

head(adios)

#remove samples from strata outside cod habitat
strata_species <- list()
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
adios <- adios[(adios$stratum %in% strata_species[["Cod"]]),]


#FIGURE OUT HOW BIG EACH CELL OF RASTER IS IN KM^2 TO SET AREASWEPT_KM2 SETTING BELOW

#get sizes of all cells in raster [km2]
cell_size<-raster::area(hab_ras, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]

#check range and mean value of cells
range(cell_size)
mean(cell_size)


#load gb polygon which has area info in it
#load stratas for clipping etc
strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
library(rgdal)
# get the shapefiles
strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 

#define georges bank
GB_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250", "01290", "01300")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_strata <- strata.areas[GB_strata_idx,]

#load random survey
scenario <- "ConPop_ConTemp"
#random survey locations
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))

#area info is in...
st_area <- GB_strata$A2
#corresponding to these strata...
st_num <- GB_strata$STR2
#number of cells in each strata...
cell_str <- surv_random$cells_per_strata[!is.na(surv_random$cells_per_strata)]
#area per cell...
area_per_cell <- st_area/cell_str


#PULLING OUT YELLOTWTAIL FLOUNDER IN THIS SCRIPT

# format for use in VAST
spring <- adios %>%
  filter(Season == "SPRING") %>%
 # filter(YEAR >= 2009) %>%
  mutate(mycatch = Cod_samp) %>%
  select(Year = year,
         Catch_KG = mycatch,
         Lat = Lat,
         Lon = Lon) %>%
  mutate(Vessel = "missing",
         AreaSwept_km2 = mean(cell_size)) #CORRECT AREA SWEPT?
summary(spring)
names(spring)


# reorder the data for use in VAST
#DOESNT SEEM TO BE USED BELOW...??
nrows <- length(spring[,1])
reorder <- sample(1:nrows, nrows, replace = FALSE)
spring_reorder <- spring
spring_reorder[1:nrows, ] <- spring[reorder, ]
head(spring)
head(spring_reorder)


# model with original data and default settings (Poisson link)
example <- list(spring)
example$Region <- "northwest_atlantic"
example$strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250)) #THESE ARE COD STRATA

#make_settings seems like the way to impliment most desired settings

settings <- make_settings(n_x = 1000,
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          bias.correct=TRUE)
#ABOVE SETTINGS PRODUCE ERRORS. CHECK_FIT SUGGESTS ADDITIONAL FIELDCONFIG SETTINGS

#WHEN ADDING ADDITIONAL FIELDCONFIG SETTINGS ALL 4 SETTINGS BELOW MUST BE INCLUDED
settings <- make_settings(n_x = 1000,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          bias.correct=TRUE,
                          FieldConfig= c("Omega1"=1, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0))
#' Specification of \code{FieldConfig} can be seen by calling \code{\link[FishStatsUtils]{make_settings}},
#'   which is the recommended way of generating this input for beginning users.
#dafault FieldConfig settings:
# if(missing(FieldConfig)) FieldConfig = c("Omega1"=0, "Epsilon1"=n_categories, "Omega2"=0, "Epsilon2"=0)

settings




#######################################################################################
# Try this first
#######################################################################################

fit <- fit_model(settings = settings,
                 "Lat_i"=as.numeric(spring[,'Lat']), 
                 "Lon_i"=as.numeric(spring[,'Lon']), 
                 "t_i"=as.numeric(spring[,'Year']), 
                 "c_i"=as.numeric(rep(0,nrow(spring))), 
                 "b_i"=as.numeric(spring[,'Catch_KG']), 
                 "a_i"=as.numeric(spring[,'AreaSwept_km2']), 
                 "v_i"=spring[,'Vessel'])

#silent = TRUE might stop output in console

# 
# #######################################################################################
# #Try second if issues with first run
# #######################################################################################
# 
# #logkappa1 keeps running into bounds so followed github suggestion here: https://github.com/James-Thorson-NOAA/VAST/issues/300
# 
# fit_orig = fit_model(settings = settings,
#                      "Lat_i"=as.numeric(spring[,'Lat']), 
#                      "Lon_i"=as.numeric(spring[,'Lon']), 
#                      "t_i"=as.numeric(spring[,'Year']), 
#                      "c_i"=as.numeric(rep(0,nrow(spring))), 
#                      "b_i"=as.numeric(spring[,'Catch_KG']), 
#                      "a_i"=as.numeric(spring[,'AreaSwept_km2']), 
#                      "v_i"=spring[,'Vessel'],
#                      run_model=FALSE)
# 
# Lower <- fit_orig$tmb_list$Lower
# # Change some bounds in Lower using grep(.) etc. (couldnt get this to work)
# #instead, i see that logkappa1 is in Lower[24] so change this value
# Lower[24] <- -10
# 
# fit = fit_model( settings = settings,
#                  "Lat_i"=as.numeric(spring[,'Lat']), 
#                  "Lon_i"=as.numeric(spring[,'Lon']), 
#                  "t_i"=as.numeric(spring[,'Year']), 
#                  "c_i"=as.numeric(rep(0,nrow(spring))), 
#                  "b_i"=as.numeric(spring[,'Catch_KG']), 
#                  "a_i"=as.numeric(spring[,'AreaSwept_km2']), 
#                  "v_i"=spring[,'Vessel'],
#                  lower=Lower )

#generates index csv and all plots including index plot (takes longer)
plot(fit)

#generates ONLY index csv and index plot (very quick)
plot_biomass_index(fit, PlotName = "index2") #DirName = paste(directory) PlotName = paste(name) 




#compare 



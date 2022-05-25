#this script reads in (x,y) survey coordinates from the habitat matrix and translates them back into latitude longitude coordinates


#read in sample survey
surv_random_sample <- readRDS(file="surv_random_sample.RDS")
surv_random_sample <- as.matrix(surv_random_sample,ncol= 12)
colnames(surv_random_sample) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","sd_spp1","sd_spp2")



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



#ADD COLUMNS TO SURVEY THAT CONTAIN LAT/LON INFORMATION

#longitude is NS. These are y values or rows
#obtained via longitude = yFromRow(raster,row = ) 

#latitude is EW, These are x values or columns
#obtained via latitude = xFromCol(raster,col= )

lat <- vector()
lon <- vector()

for(i in seq(length(surv_random_sample[,1]))){
  
  rw <- as.numeric(surv_random_sample[i,"x"])  #x in col 2
  cl <- as.numeric(surv_random_sample[i,"y"]) #y in col 3
  
  lat[i] <- xFromCol(hab_ras, col = cl)
  lon[i] <- yFromRow(hab_ras, row = rw)

}


#add columns to table
surv_random_sample <- cbind(surv_random_sample,lat,lon)

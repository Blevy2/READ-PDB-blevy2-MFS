# 
# x <- cbind(fit$extrapolation_list$Data_Extrap[,1],
#            fit$extrapolation_list$Data_Extrap[,2],
#            fit$Report$D_gct,
#            rep(seq(3,22),each=5000))
# 
# #x will have 100,000 rows. 5000 for each of the 20 years. The extrapolation gris has 5000 cells
# 
# colnames(x) <- c("Lon","Lat","Biomass","Year")
# 
# x<- as.data.frame(x, xy=TRUE)
# 
# #rasterFromXYZ(x[,1:3][x$Year==3,],res=res(hab_ras))
# xx<-raster::extract(GB_strata_Had,x[,2:1],layer="STR2")
# 
# 
# 
# 
# x<-sp::SpatialPoints(coords=cbind(fit$extrapolation_list$Data_Extrap[,1],fit$extrapolation_list$Data_Extrap[,2]))
# crs(x) <- crs(r1)
# crs(GB_strata_Had) <-crs(r1)
# xx[[ssn]]=sp::over(x,GB_strata_Had)
# biomass_est <- as.numeric(fit$Report$D_gct[1:5000]*fit$extrapolation_list$Area_km2_x)
# test <- cbind(xx[[ssn]],x,biomass_est)
# 
# colnames(test)[c(4,8,9,10)] <- c("stratum","Lon","Lat","Biomass_est")


#knots lat lon used:
#plot(sp::SpatialPoints(coords=cbind(fit$spatial_list$latlon_x[,2],fit$spatial_list$latlon_x[,1])))



vast_by_strat <- function(s,str_dir,cov_directory,noise, scenario1, r1, GB_strata_Had, strata_species_s ){

xx<-list()
xxx<-data.frame()
for(ssn in c("spring","fall")){
print(ssn)
  fit <- readRDS(
    paste("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-MFS2/VAST/ForPaper/",scenario1,"/",s,"/",str_dir,cov_directory,noise,"/obsmodel7/",ssn,"/fit_",ssn,".RDS",sep="")
  )


ssn1 = ifelse(ssn == "spring", "SPRING","FALL")

#Old way (uses estimates at samples instead of extrapolation points)
#x<-sp::SpatialPoints(coords=cbind(fit$data_frame$Lon_i,fit$data_frame$Lat_i))
#crs(x) <- crs(r1)
#crs(GB_strata_Had) <-crs(r1)
#xx[[ssn]]=sp::over(x,GB_strata_Had)
#biomass_xy <- fit$Report$D_i #biomass at xy
#area_swept_xy <- rep(1,length(biomass_xy)) #your_training_data$a_i #area swept at xy
#yr <- fit$data_frame$t_i  #are the indices here aligned with others??
#sn <- rep(ssn1,length(biomass_xy))
#tow <- rep(0,length(biomass_xy)) #needed for SRS file
#xx[[ssn]]<- cbind(xx[[ssn]],x,biomass_xy,area_swept_xy,yr,sn,tow)
#colnames(xx[[ssn]])[c(4,8,9,10,11,12,13,14)] <- c("stratum","Lon","Lat",paste("biomass_",s,sep=""),"area swept","year","Season","tow")


#second attempt (does not match VAST estimate)
x<-sp::SpatialPoints(coords=cbind(fit$extrapolation_list$Data_Extrap[,1],fit$extrapolation_list$Data_Extrap[,2]))
crs(x) <- crs(r1)
crs(GB_strata_Had) <-crs(r1)
ovrly=sp::over(x,GB_strata_Had)

for(yrr in seq(1,20)){
#old indexing method
#biomass_est <- as.numeric(fit$Report$D_gct[5000*(yrr-1)+1:5000*(yrr-1)+5000]*fit$extrapolation_list$Area_km2_x)
biomass_est <- as.numeric(fit$Report$D_gct[,1,yrr]*fit$extrapolation_list$Area_km2_x)
  
yr <- rep(yrr+2,5000)  
sn <- rep(ssn1,length(biomass_est))

test <- cbind(ovrly,x,biomass_est,yr,sn)

colnames(test)[c(4,8,9,10,11,12)] <- c("stratum","Lon","Lat","Biomass_est","year","season")

test <- na.omit(test)



test <- test[(test$stratum %in% strata_species_s),]
print(sum(test$Biomass_est))
 
xx[[ssn]] <- rbind(xx[[ssn]],test)
}

}
xxx <- rbind(xx[[1]],xx[[2]])



#now outside here
#ttt <- srs_survey(df=xxx, sa=sv.area, str=NULL, ta=1, sppname = paste0("biomass_",s, sep=""))


return(xxx)


}
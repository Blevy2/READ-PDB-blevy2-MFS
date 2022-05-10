



#READY TO COMPARE VALUES

#this script can be used to compare spatial model results with observation data

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x)
  t(apply(x, 2, rev))


#load survey data
cod_tows <-
  as.data.frame(
    read.csv(file = "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_164712_GBK_NONE_survey_dist_map_fixed.csv", header =
               T)
  )
had_tows <-
  as.data.frame(
    read.csv(file = "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_164744_GBK_NONE_survey_dist_map_fixed.csv", header =
               T)
  )
YT_tows <-
  as.data.frame(
    read.csv(file = "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv", header =
               T)
  )

#ONLY LOOK AT RECENT YEARS
cod_tows <- cod_tows[cod_tows$YEAR >= 2011,]
had_tows <- had_tows[had_tows$YEAR >= 2011,]
YT_tows <- YT_tows[YT_tows$YEAR >= 2011,]


#replace NA values with 0s
cod_tows[is.na(cod_tows)]<-0
YT_tows[is.na(YT_tows)]<-0
had_tows[is.na(had_tows)]<-0

#CHECK HOW MANY MONTHS ARE CONTAINED IN EACH SURVEY
#cod has sampled in 7 months
cod_months <-
  unique(substr(cod_tows$TOWDATE, 3, 5)) #3 letter month abbreviation in locations 3-5 of TOWDATE
#had has sampled in 7 months
had_months <-
  unique(substr(had_tows$TOWDATE, 3, 5)) #3 letter month abbreviation in locations 3-5 of TOWDATE
#YT has sampled in 6 months
YT_months <-
  unique(substr(YT_tows$TOWDATE, 3, 5)) #3 letter month abbreviation in locations 3-5 of TOWDATE

#add month column to data
cod_tows$month <- substr(cod_tows$TOWDATE, 3, 5)
had_tows$month <- substr(had_tows$TOWDATE, 3, 5)
YT_tows$month <- substr(YT_tows$TOWDATE, 3, 5)


#load previously created haddock rasters (its the largest one that contains others)
library(raster)
library(spatstat.geom)

#haddock
Had_ras <-
  readRDS(file = "TestScripts/Habitat_plots/Haddock/Had_Weighted_AdaptFalse_RASTER_res2.RDS")
plot(Had_ras)






# 
# 
# #load matrix and practice making it into raster
# hab <- readRDS(file = "hab_GB_3species.RDS") #courser resolution
# 
# test <- raster(hab$hab$spp1)
# extent(test) <- extent(Had_ras)
# plot(test)
# 
# 
# 
# 
# #pull out survey values and plot on top of habitat
# 
# #YT
# plot(test)
# 
# 
# # YT_points <- ppp(YT_tows$LONGITUDE, YT_tows$LATITUDE, marks = YT_tows$CATCH_WT_CAL , owin(c(-69.98877, -65.56877) ,c(40.07482,42.79482) ))
# # plot(YT_points)
# 
# 
# #rasterizing the tows to match the resolution of the matrices and plotting.
# YT_points_rasterize <-
#   rasterize(
#     data.frame(cbind(YT_tows$LONGITUDE, YT_tows$LATITUDE)),
#     Had_ras,
#     field = (YT_tows$CATCH_WT_CAL / sum(YT_tows$CATCH_WT_CAL, na.rm = T)),
#     fun = 'sum'
#   )
# plot(YT_points_rasterize)









#aggregate both model output and tow data into monthly values

#TOWS
#YT
YT_monthly <- list()
for (m in YT_months) {
  YT_monthly[[m]] <- YT_tows[YT_tows$month == m,]
}

#cod
cod_monthly <- list()
for (m in cod_months) {
  cod_monthly[[m]] <- cod_tows[cod_tows$month == m,]
}

#had
had_monthly <- list()
for (m in had_months) {
  had_monthly[[m]] <- had_tows[had_tows$month == m,]
}




#MODEL OUTPUT

#first average weekly values
temp_YT <- vector("list", length = 22)#22 years
temp_cod <-  vector("list", length = 22)
temp_had <-  vector("list", length = 22)

YT_wk <- vector("list", length = 52)
cod_wk <-  vector("list", length = 52)
had_wk <-  vector("list", length = 52)



for (w1 in seq(52)) {
  idx <- 1
  for (w2 in seq(w1, length(res$pop_bios), 52)) {
    temp_YT[[idx]] <- res$pop_bios[[w2]][["spp1"]]
    temp_cod[[idx]] <- res$pop_bios[[w2]][["spp2"]]
    temp_had[[idx]] <- res$pop_bios[[w2]][["spp3"]]
    
    idx <- idx + 1
    
  }
  YT_wk[[w1]] <- Reduce('+', temp_YT) / length(temp_YT)
  cod_wk[[w1]] <- Reduce('+', temp_cod) / length(temp_cod)
  had_wk[[w1]] <- Reduce('+', temp_had) / length(temp_had)
}


#second create monthly average values


#spring sample data is from mar, apr, may, june
#march 1 is day 60 This is 4/7 through 9th week (ie, weight week 9 by 3/7).
#march: wks (3/7)9, 10, 11, 12, 13
#april: wks     14, 15, 16, 17, (2/7)18
#may: wks  (5/7)18, 19, 20, 21, (5/7)22 (weight would be (5/7)/((5/7)+(5/7)) and (5/7)/((5/7)+(5/7)))
#june: wks (2/7)22, 23, 24, 25, 26


spring_weeks <-
  matrix(c(9,14,18,22,10,15,19,23,11,16,20,24,12,17,21,25,13,18,22,26),
         nc = 5) #repeating entries for first two month but will make weight 0

spring_week_weights <-  matrix(c( (3/7)/(4+(3/7)), (1)/(4+(2/7)), (5/7)/(3+2*(5/7)), (2/7)/(4+(2/7)), (1)/(4+(3/7)), (1)/(4+(2/7)), (1)/(3+2*(5/7)) , (1)/(4+(2/7)), (1)/(4+(3/7)), (1)/(4+(2/7)),(1)/(3+2*(5/7)) , (1)/(4+(2/7)),(1)/(4+(3/7)), (1)/(4+(2/7)), (1)/(3+2*(5/7)), (1)/(4+(2/7)), (1)/(4+(3/7)), (2/7)/(4+(2/7)), (5/7)/(3+2*(5/7)) , (1)/(4+(2/7))  ), nc = 5)


spring_sample_mnts <- c("Mar", "Apr", "May", "Jun")



YT_spring_mnth <-  list() #7 months with survey observations
cod_spring_mnth <- list() #7 months
had_spring_mnth <- list() #7 months

m_idx <- 1

for (m in spring_sample_mnts) {
  wk1 <- spring_weeks[m_idx, 1]
  
  YT_spring_mnth[[m]] <-  (  spring_week_weights[m_idx, 1] * YT_wk[[wk1]] + spring_week_weights[m_idx, 2] * YT_wk[[wk1 + 1]] + spring_week_weights[m_idx, 3] * YT_wk[[wk1 + 2]] + 
                               spring_week_weights[m_idx, 4] *YT_wk[[wk1 + 3]] + spring_week_weights[m_idx, 5] * YT_wk[[wk1 + 4]] 
  )
  
  
  cod_spring_mnth[[m]] <-  (  spring_week_weights[m_idx, 1] * cod_wk[[wk1]] + spring_week_weights[m_idx, 2] *
                                                       cod_wk[[wk1 + 1]] + spring_week_weights[m_idx, 3] * cod_wk[[wk1 + 2]] + spring_week_weights[m_idx, 4] *
                                                       cod_wk[[wk1 + 3]] + spring_week_weights[m_idx, 5] * cod_wk[[wk1 + 4]] 
  )
  
  
  had_spring_mnth[[m]] <- (  spring_week_weights[m_idx, 1] * had_wk[[wk1]] + spring_week_weights[m_idx, 2] *
                                                       had_wk[[wk1 + 1]] + spring_week_weights[m_idx, 3] * had_wk[[wk1 + 2]] + spring_week_weights[m_idx, 4] *
                                                       had_wk[[wk1 + 3]] + spring_week_weights[m_idx, 5] * had_wk[[wk1 + 4]]
  )
  
  
  m_idx <- m_idx+1
  # 
  # YT_mnth[[idx]] <-
  #   (YT_wk[[w]] + YT_wk[[w + 1]] + YT_wk[[w + 2]] + YT_wk[[w + 3]]) / 4
  # cod_mnth[[idx]] <-
  #   (cod_wk[[w]] + cod_wk[[w + 1]] + cod_wk[[w + 2]] + cod_wk[[w + 3]]) / 4
  # had_mnth[[idx]] <-
  #   (had_wk[[w]] + had_wk[[w + 1]] + had_wk[[w + 2]] + had_wk[[w + 3]]) / 4
  # idx <- idx + 1
}





#fall sample data is from sep, oct, nov
#september 1 is day 244. This is 6/7 through 35th week (ie, weight week 35 by 1/7).
#sept: wks (1/7)*35, 36, 37, 38, 39, (1/7)40
#oct: wks   (6/7)40, 41, 42, 43, (4/7)44
#nov: wks  (3/7)44, 45, 46, 47, (6/7)*48  (weight would be (6/7)/((3/7)+(6/7)) and (3/7)/((3/7)+(6/7)))

fall_weeks <-
  matrix(c(35, 40, 44, 36, 41, 45, 37, 42, 46, 38, 43, 47, 39, 44, 48, 40, 44, 48),
         nc = 6) #repeating entries for first two month but will make weight 0

fall_week_weights <-
  matrix(c(
    .5,
    (6 / 7) / ((4 / 7) + (6 / 7)),
    (3 / 7) / ((3 / 7) + (6 / 7)),
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    .5,
    (4 / 7) / ((4 / 7) + (6 / 7)),
    1,
    0,
    0,
    (6 / 7) / ((3 / 7) + (6 / 7))
  ), nc = 6)

fall_weeks_n <- c(5, 4, 4)

fall_sample_mnts <- c("Sep", "Oct", "Nov")



YT_fall_mnth <-  list() #7 months with survey observations
cod_fall_mnth <- list() #7 months
had_fall_mnth <- list() #7 months

m_idx <- 1

for (m in fall_sample_mnts) {
  wk1 <- fall_weeks[m_idx, 1]
  
  YT_fall_mnth[[m]] <- (1/fall_weeks_n[m_idx]) * (  fall_week_weights[m_idx, 1] * YT_wk[[wk1]] + fall_week_weights[m_idx, 2] *
        YT_wk[[wk1 + 1]] + fall_week_weights[m_idx, 3] * YT_wk[[wk1 + 2]] + fall_week_weights[m_idx, 4] *
        YT_wk[[wk1 + 3]] + fall_week_weights[m_idx, 5] * YT_wk[[wk1 + 4]] + fall_week_weights[m_idx, 6] *
        YT_wk[[wk1 + 5]]
    )
  
  
  cod_fall_mnth[[m]] <- (1/fall_weeks_n[m_idx]) * (  fall_week_weights[m_idx, 1] * cod_wk[[wk1]] + fall_week_weights[m_idx, 2] *
                                                 cod_wk[[wk1 + 1]] + fall_week_weights[m_idx, 3] * cod_wk[[wk1 + 2]] + fall_week_weights[m_idx, 4] *
                                                 cod_wk[[wk1 + 3]] + fall_week_weights[m_idx, 5] * cod_wk[[wk1 + 4]] + fall_week_weights[m_idx, 6] *
                                                 cod_wk[[wk1 + 5]]
  )
  
  
  had_fall_mnth[[m]] <- (1/fall_weeks_n[m_idx]) * (  fall_week_weights[m_idx, 1] * had_wk[[wk1]] + fall_week_weights[m_idx, 2] *
                                                 had_wk[[wk1 + 1]] + fall_week_weights[m_idx, 3] * had_wk[[wk1 + 2]] + fall_week_weights[m_idx, 4] *
                                                 had_wk[[wk1 + 3]] + fall_week_weights[m_idx, 5] * had_wk[[wk1 + 4]] + fall_week_weights[m_idx, 6] *
                                                 had_wk[[wk1 + 5]]
  )
  
  
  m_idx <- m_idx+1
  # 
  # YT_mnth[[idx]] <-
  #   (YT_wk[[w]] + YT_wk[[w + 1]] + YT_wk[[w + 2]] + YT_wk[[w + 3]]) / 4
  # cod_mnth[[idx]] <-
  #   (cod_wk[[w]] + cod_wk[[w + 1]] + cod_wk[[w + 2]] + cod_wk[[w + 3]]) / 4
  # had_mnth[[idx]] <-
  #   (had_wk[[w]] + had_wk[[w + 1]] + had_wk[[w + 2]] + had_wk[[w + 3]]) / 4
  # idx <- idx + 1
}



#combine fall and spring surveys
YT_model_month <- do.call(c,list(YT_spring_mnth,YT_fall_mnth))
cod_model_month <- do.call(c,list(cod_spring_mnth,cod_fall_mnth))
had_model_month <- do.call(c,list(had_spring_mnth,had_fall_mnth))






#READY TO COMPARE VALUES


library(gstat)
library(rgdal)


#go through each month and plot survey values to compare to model values.
#must extrapolate survey into surface


#initialize pdf
pdf(file=paste0('testfolder/model_vs_survey_movement','.pdf'))


#YELLOWTAIL PLOTS

YT_months <-c("Mar","Apr","May","Sep", "Oct", "Nov")

for(m in YT_months){
  
  par(mfrow = c(2,1), mar = c(1.5, 1.5, 1.5, 1.5))

  #plotting model values
  temp <- raster(YT_model_month[[m]])
  extent(temp) <- extent(Had_ras)
  plot(temp, main =paste("YTF Model, Month:",m))
  
  
  
  #plotting survey values
  temp2 <- YT_tows[toupper(YT_tows$month)==toupper(m),]
  
  nsamps <- length(temp2[,1])
  
  temp3 <- SpatialPointsDataFrame(cbind(temp2$LONGITUDE,temp2$LATITUDE), data = as.data.frame(temp2$CATCH_WT_CAL))
  
  #setup raster to use
  grid <- as(Had_ras,"SpatialPixels")
  #proj4string(grid) = proj4string(YT_oct_pts)
  
  crs(grid)<-crs(Had_ras) #need to have same CRS
  crs(temp3)<-crs(grid)
  
  idw = gstat::idw(formula=temp3$`temp2$CATCH_WT_CAL`~1, locations = temp3, newdata= grid)
  
  temp4 <- raster(idw)
  plot(temp4, main =paste("YTF Survey, Month:",m, "N_samp:",nsamps)) 
  
  
  
}



#COD PLOTS

cod_months <-  c("Mar","Apr","May","Jun","Sep", "Oct", "Nov")

for(m in cod_months){
  
  par(mfrow = c(2,1), mar = c(1.5, 1.5, 1.5, 1.5))
  
  #plotting model values
  temp <- raster(cod_model_month[[m]])
  extent(temp) <- extent(Had_ras)
  plot(temp, main =paste("Cod Model, Month:",m))
  
  
  
  #plotting survey values
  temp2 <- cod_tows[toupper(cod_tows$month)==toupper(m),]
  
  nsamps <- length(temp2[,1])
  
  temp3 <- SpatialPointsDataFrame(cbind(temp2$LONGITUDE,temp2$LATITUDE), data = as.data.frame(temp2$CATCH_WT_CAL))
  
  #setup raster to use
  grid <- as(Had_ras,"SpatialPixels")
  #proj4string(grid) = proj4string(YT_oct_pts)
  
  crs(grid)<-crs(Had_ras) #need to have same CRS
  crs(temp3)<-crs(grid)
  
  idw = gstat::idw(formula=temp3$`temp2$CATCH_WT_CAL`~1, locations = temp3, newdata= grid)
  
  temp4 <- raster(idw)
  plot(temp4, main =paste("Cod Survey, Month:",m, "N_samp:",nsamps))  
  
  
  
}



#HADDOCK PLOTS

had_months <- surv_mnths <- c("Mar","Apr","May","Jun","Sep", "Oct", "Nov")

for(m in had_months){
  
  par(mfrow = c(2,1), mar = c(1.5, 1.5, 1.5, 1.5))
  
  #plotting model values
  temp <- raster(had_model_month[[m]])
  extent(temp) <- extent(Had_ras)
  plot(temp, main =paste("Had Model, Month:",m))
  
  
  #plotting survey values
  temp2 <- had_tows[toupper(had_tows$month)==toupper(m),]
  
  nsamps <- length(temp2[,1])
  
  temp3 <- SpatialPointsDataFrame(cbind(temp2$LONGITUDE,temp2$LATITUDE), data = as.data.frame(temp2$CATCH_WT_CAL))
  
  #setup raster to use
  grid <- as(Had_ras,"SpatialPixels")
  #proj4string(grid) = proj4string(YT_oct_pts)
  
  crs(grid)<-crs(Had_ras) #need to have same CRS
  crs(temp3)<-crs(grid)
  
  idw = gstat::idw(formula=temp3$`temp2$CATCH_WT_CAL`~1, locations = temp3, newdata= grid)
  
  temp4 <- raster(idw)
  plot(temp4, main =paste("Had Survey, Month:",m, "N_samp:",nsamps)) 

  
  
}



dev.off()



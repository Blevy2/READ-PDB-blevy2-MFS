

#read in estimates provided by David Chevrier (DC) and Liz Brooks (LB)
areas_DC <- read_csv("C:/Users/benjamin.levy/Desktop/NOAA/GIS_Stuff/From_Liz/equalArea_strata_DC.csv")
areas_LB <- read_csv("C:/Users/benjamin.levy/Desktop/NOAA/GIS_Stuff/From_Liz/strata_area_LB.csv")


#Remove non GB strata
strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300)) #THESE ARE HAD STRATA

areas_DC <- areas_DC[(as.numeric(areas_DC$STRATUMA) %in% strata.limits$Georges_Bank),]
areas_LB <- areas_LB[(as.numeric(areas_LB$STRATUM) %in% strata.limits$Georges_Bank),]

#to shift from square meters to square kilometer, divide square meter values by 1000000
areas_DC$KM2 <- areas_DC$`AREA SQ Meters` / 1000000

#to shift from square nautical miles to square kilometer, multiply mile values by 3.4299
areas_LB$KM2 <- areas_LB$STRATUM_AREA*3.4299


#measure cell size in each case
scen <- "ConPop_ConTemp"
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scen,"\\surv_random_",scen,".RDS",sep=""))
cells_per_strata <- surv_random$cells_per_strata[!is.na(surv_random$cells_per_strata)]

#Compare results
diff <- areas_DC$KM2 - areas_LB$KM2
CellSzEst_DC <- areas_DC$KM2/cells_per_strata
CellSzEst_LB <- areas_LB$KM2/cells_per_strata
GB_areas <- cbind(areas_DC$STRATUMA,
                  areas_DC$KM2,
                  areas_LB$KM2,
                  diff,
                  CellSzEst_DC,
                  CellSzEst_LB)

colnames(GB_areas) <- c("Stratum","DC_areas_KM2","LB_areas_KM2", "Difference", "CellSzEst_DC","CellSzEst_LB")

write.csv(GB_areas, file="Georges_Bank_Areas_and_CellSizes.csv")

var(CellSzEst_DC) #0.159
mean(CellSzEst_DC) #8.98

var(CellSzEst_LB) #0.052
mean(CellSzEst_LB) #8.74




# GBYT_VAST.R
# my example of how to use ADIOS data in VAST

# set working directory to source file location to start

library(TMB)
library(VAST)
library(dplyr)
library(ggplot2)

# for reproducability
set.seed <- 14159623

# get data
adios <- read.csv("ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv",
                  header = TRUE, stringsAsFactors = FALSE)
head(adios)

# format for use in VAST
spring <- adios %>%
  filter(SEASON == "SPRING") %>%
  filter(YEAR >= 2009) %>%
  mutate(mycatch = ifelse(is.na(CATCH_WT_CAL), 0, CATCH_WT_CAL)) %>%
  select(Year = YEAR,
         Catch_KG = mycatch,
         Lat = LATITUDE,
         Lon = LONGITUDE) %>%
  mutate(Vessel = "missing",
         AreaSwept_km2 = 0.03841492)
summary(spring)
names(spring)

# reorder the data for use in VAST
nrows <- length(spring[,1])
reorder <- sample(1:nrows, nrows, replace = FALSE)
spring_reorder <- spring
spring_reorder[1:nrows, ] <- spring[reorder, ]
head(spring)
head(spring_reorder)

# model with original data and default settings (Poisson link)
example <- list(spring)
example$Region <- "northwest_atlantic"
example$strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210))

settings <- make_settings(n_x = 50,
                          Region=example$Region, 
                          purpose="index", 
                          strata.limits=example$strata.limits, 
                          bias.correct=TRUE)
settings

fit <- fit_model(settings = settings,
                 "Lat_i"=spring[,'Lat'], 
                 "Lon_i"=spring[,'Lon'], 
                 "t_i"=spring[,'Year'], 
                 "c_i"=rep(0,nrow(spring)), 
                 "b_i"=spring[,'Catch_KG'], 
                 "a_i"=spring[,'AreaSwept_km2'], 
                 "v_i"=spring[,'Vessel'])
#names(fit)
plot(fit)
#names(fit$Report)
#names(fit$data_list)
#fit$parameter_estimates$AIC
# to see the mesh
#windows(record=TRUE) 
#plot(fit$spatial_list$MeshList$anisotropic_mesh)
#dev.off()

### what happens if reorder data
# have to make new directory cuz kmeans not overwritten otherwise
setwd(paste0(getwd(), "\\reordered"))

example <- list(spring_reorder)
example$Region <- "northwest_atlantic"
example$strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210))

settings2 <- make_settings(n_x = 50,
                           Region=example$Region, 
                           purpose="index", 
                           strata.limits=example$strata.limits, 
                           bias.correct=TRUE)
settings2

fit2 <- fit_model(settings = settings2,
                  "Lat_i"=spring_reorder[,'Lat'], 
                  "Lon_i"=spring_reorder[,'Lon'], 
                  "t_i"=spring_reorder[,'Year'], 
                  "c_i"=rep(0,nrow(spring_reorder)), 
                  "b_i"=spring_reorder[,'Catch_KG'], 
                  "a_i"=spring_reorder[,'AreaSwept_km2'], 
                  "v_i"=spring_reorder[,'Vessel'])

plot(fit2)

# now change the model for the reordered data to use logit-link instead of Poisson-link
setwd("..\\reorderedPoisson")

example <- list(spring_reorder)
example$Region <- "northwest_atlantic"
example$strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210))

settings3 <- make_settings(n_x = 50,
                           Region=example$Region, 
                           purpose="index", 
                           strata.limits=example$strata.limits, 
                           bias.correct=TRUE)

settings3$ObsModel[2] <- 0 # this changes to logit-link

fit3 <- fit_model(settings = settings3,
                  "Lat_i"=spring_reorder[,'Lat'], 
                  "Lon_i"=spring_reorder[,'Lon'], 
                  "t_i"=spring_reorder[,'Year'], 
                  "c_i"=rep(0,nrow(spring_reorder)), 
                  "b_i"=spring_reorder[,'Catch_KG'], 
                  "a_i"=spring_reorder[,'AreaSwept_km2'], 
                  "v_i"=spring_reorder[,'Vessel'])

plot(fit3)

# and now go back to original data but use logit-link for both models
setwd("..\\Poisson")

example <- list(spring)
example$Region <- "northwest_atlantic"
example$strata.limits <- data.frame(Georges_Bank = c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210))

settings1 <- make_settings(n_x = 50,
                           Region=example$Region, 
                           purpose="index", 
                           strata.limits=example$strata.limits, 
                           bias.correct=TRUE)

settings1$ObsModel[2] <- 0 # this changes to logit-link

fit1 <- fit_model(settings = settings1,
                  "Lat_i"=spring[,'Lat'], 
                  "Lon_i"=spring[,'Lon'], 
                  "t_i"=spring[,'Year'], 
                  "c_i"=rep(0,nrow(spring)), 
                  "b_i"=spring[,'Catch_KG'], 
                  "a_i"=spring[,'AreaSwept_km2'], 
                  "v_i"=spring[,'Vessel'])

plot(fit1)

# compare biomass estimates
setwd("..\\")
est <- read.csv("Table_for_SS3.csv", header = TRUE)
est1 <- read.csv("Poisson\\Table_for_SS3.csv", header = TRUE)
est2 <- read.csv("reordered\\Table_for_SS3.csv", header = TRUE)
est3 <- read.csv("reorderedPoisson\\Table_for_SS3.csv", header = TRUE)
year.plot.range <- 2016:2019
est$run <- 0
est1$run <- 1
est2$run <- 2
est3$run <- 3
df <- rbind(est, est1, est2, est3) %>%
  mutate(lo = Estimate_metric_tons - 1.96 * SD_mt,
         hi = Estimate_metric_tons + 1.96 * SD_mt)

ggplot(filter(df, Year %in% year.plot.range), 
       aes(x = Year, y=Estimate_metric_tons, color=as.factor(run))) +
  geom_ribbon(aes(ymin=lo, ymax=hi, fill=as.factor(run)), alpha=0.2) +
  geom_point() +
  geom_line() +
  expand_limits(y = 0) +
  theme_bw()

diffdf <- data.frame(Year = est$Year,
                     reldiff = 100 * (est2$Estimate_metric_tons - est$Estimate_metric_tons) / est$Estimate_metric_tons)

ggplot(diffdf, aes(x=Year, y=reldiff)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, col="red") +
  ylab("Relative Diff %") +
  theme_bw()

# to see the mesh with original tow locations
# toggle back and forth to see how the triangles change
windows(record=TRUE) 
plot(fit$spatial_list$MeshList$anisotropic_mesh)
points(fit$spatial_list$loc_i[,1], 
       fit$spatial_list$loc_i[,2])

plot(fit1$spatial_list$MeshList$anisotropic_mesh)
points(fit1$spatial_list$loc_i[,1], 
       fit1$spatial_list$loc_i[,2])

plot(fit2$spatial_list$MeshList$anisotropic_mesh)
points(fit2$spatial_list$loc_i[,1], 
       fit2$spatial_list$loc_i[,2])

plot(fit3$spatial_list$MeshList$anisotropic_mesh)
points(fit3$spatial_list$loc_i[,1], 
       fit3$spatial_list$loc_i[,2])


#dev.off()

# compare AIC
cbind(c(fit$parameter_estimates$AIC, fit2$parameter_estimates$AIC),
      c(fit1$parameter_estimates$AIC, fit3$parameter_estimates$AIC))


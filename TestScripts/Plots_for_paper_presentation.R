

#RUN BELOW AFTER RUNNING VAST_STRATMEAN_ERROR_COMPARISON_ALL_FORPAPER FIRST TO GET THE CORRECT VARIABLES


#YELLOWTAIL ONLY CONSTNATPOPULATION INCREASING TEMPERATURE PLOT

model_line_size = 1.5
model_point_size = 3.5
estimate_line_size = 1.25
estimate_point_size = 3

for(season_ in c(1,2)){

 ssnn = ifelse(season_==1, "Spring Estimate", "Fall Estimate")
  
print(ggplot() +
  
  
  #this way plots data by season
  geom_point(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=model_point_size) +
  geom_line(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=model_line_size) +
  
  
  
  #plot VAST estimate without covariates with NO noise
  geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov No Noise"),width=.3) +
  #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==2),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"), size=estimate_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"),size=estimate_line_size)+
  # 
  # #plot VAST estimate without covariates with WITH noise
  # geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==2),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov W Noise"),width=.3) +
  # #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==2),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  # geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==2),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"),size=2)+
  # geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==2),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"))+
  
  #plot VAST estimate with covariates with NO noise
  geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov No Noise"),width=.3) +
  #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"), size=estimate_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"),size=estimate_line_size)+
  
  # #plot VAST estimate with covariates WITH noise
  # geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov w Noise"),width=.3) +
  # #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  # geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
  # geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
  # 
  
  #plot stratified calculation data with NO noise
  geom_errorbar(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean No Noise"),width=.3) +
  # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
  geom_point(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean No Noise"), size=estimate_point_size)+
  geom_line(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season,color = "Strat Mean No Noise"),size=estimate_line_size)+
  
  

  
  # #plot stratified calculation data with NO noise
  # geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean W Noise"),width=.3) +
  # # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
  # geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
  # geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
  # 
  # 
  #facet_wrap(~ season, ncol =2) +
  # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FC=", toString(FC_spring), 
  #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
  #                                          "  FC=", toString(FC_fall),
  #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
  
  labs(x="year",y="Biomass", title =paste("Yellowtail Flounder ",ssnn,sep=""), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=14))
)

}





#







































#HADDOCK INCPOP CONTEMP ONLY CONSTNATPOPULATION INCREASING TEMPERATURE PLOT


model_line_size = 1.5
model_point_size = 3.5
estimate_line_size = 1.25
estimate_point_size = 3

season_ = 2

ggplot() +
  
  #plot VAST estimate without covariates with NO noise
  geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov No Noise"),width=.3) +
  #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"), size=estimate_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov No Noise"),size=estimate_line_size)+
  # 
  # #plot VAST estimate without covariates with WITH noise
  # geom_errorbar(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov W Noise"),width=.3) +
  # #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  # geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"),size=season_)+
  # geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST No Cov W Noise"))+
  
  #plot VAST estimate with covariates with NO noise
  geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov No Noise"),width=.3) +
  #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"), size=estimate_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov No Noise"),size=estimate_line_size)+
  
  # #plot VAST estimate with covariates WITH noise
  # geom_errorbar(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST w Cov w Noise"),width=.3) +
  # #geom_linerange(data=subset(VAST_data[[s]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  # geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
  # geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=Estimate,group=season, color = "VAST w Cov w Noise"))+
  # 
  
  #plot stratified calculation data with NO noise
  geom_errorbar(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean No Noise"),width=.3) +
  # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
  geom_point(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean No Noise"), size=estimate_point_size)+
  geom_line(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]]),season==season_),aes(x=year,y=mean.yr.absolute,group=season,color = "Strat Mean No Noise"),size=estimate_line_size)+
  
  
  
  #this way plots data by season
  geom_point(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=biomass, group = season, color = "Model"),size=model_point_size) +
  geom_line(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=biomass, group =season, color = "Model"),size=model_line_size) +
  
  
  
  # #plot stratified calculation data with NO noise
  # geom_errorbar(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean W Noise"),width=.3) +
  # # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
  # geom_point(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
  # geom_line(data=as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Strat Mean W Noise"))+
  # 
  # 
  #facet_wrap(~ season, ncol =2) +
# labs(x="year",y="Biomass", title = paste(paste(s," ",folder,sep=""), "\n", #new line
#                                          paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
#                                                "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
#                                                " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
#                                                "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),
#                                                "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["spring"]],digits=2),
#                                                "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["spring"]],digits=2),sep=""), "\n", #new line
#                                          paste(" V.NC.NN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
#                                                "   V.NC.YN=",round(VAST_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
#                                                " V.YC.NN=",round(VAST_Model_error[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
#                                                "   V.YC.YN=",round(VAST_Model_error[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),
#                                                "  SM.NN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]][["fall"]],digits=2),
#                                                "  SM.YN=",round(SRS_Model_error[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]][["fall"]],digits=2),sep=""),sep=""), color ="" )+
#   

labs(x="year",y="Biomass", title ="Haddock Fall Estimate", color ="" )+
  
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16))








# ESTIMATE RATIO PLOTS
#Yellowtail estimates are for the constant population, constant temperature, all strata
#COD DECPOP_CONTEMP
#Haddock are IncPop_IncTemp_all strata_

model_line_size = 1.5
model_point_size = 3.5
estimate_line_size = 1.25
estimate_point_size = 3

for(season_ in c(1,2)){
  
  ssnn = ifelse(season_==1, "Spring", "Fall")

print(
ggplot() +
  
  
  #this way plots data by season divided by itself so it equals 1
  geom_point(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=log(Est_ratio), group = season, color = "Model"),size=estimate_point_size) +
  geom_line(data = subset(as.data.frame(pop_by_season[[s]]),season==season_), aes(x=as.numeric(year),y=log(Est_ratio), group =season, color = "Model"),size=estimate_line_size ) +
  
  #plot VAST estimate without covariates with NO noise
  geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov No Noise"),size=model_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov No Noise"),size=model_line_size)+
  
  # #plot VAST estimate without covariates with WITH noise
  # geom_point(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov W Noise"))+
  # geom_line(data=subset(VAST_data[[s]][["_NoCovs_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST No Cov W Noise"))+
  # 
  #plot VAST estimate with covariates with NO noise
  geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov No Noise"),size=model_point_size)+
  geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov No Noise"),size=model_line_size)+
  
  # #plot VAST estimate with covariates WITH noise
  # geom_point(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov w Noise"))+
  # geom_line(data=subset(VAST_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]],season==season_),aes(x=Year,y=log(Est_ratio),group=season, color = "VAST w Cov w Noise"))+
  # 
  
  #plot stratified calculation data with NO noise
  geom_point(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),season==season_),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean No Noise"),size=model_point_size)+
  geom_line(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["NoNoise_"]][[folder]][,1:5]),season==season_),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean No Noise"),size=model_line_size)+
  # 
  # #plot stratified calculation data with NO noise
  # geom_point(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),season==season_),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean W Noise"),size=)+
  # geom_line(data=subset(as.data.frame(SRS_data[[s]][["_WithCov_"]][["WithNoise_"]][[folder]][,1:5]),season==season_),aes(x=year,y=log(Est_ratio),group=season, color = "Strat Mean W Noise"),size=)+
  # 
  # 
  #facet_wrap(~ season, ncol =1) +
  # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FC=", toString(FC_spring), 
  #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
  #                                          "  FC=", toString(FC_fall),
  #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
  
  labs(x="year",y="log(Estimate / Model)", title = paste("Haddock Ratio Trend- ", ssnn,sep=""), color ="" )+
  
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=16),
        title=element_text(size=16))
)
}



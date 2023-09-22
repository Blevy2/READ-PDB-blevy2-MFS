

# for(scenario1 in c("ConPop_ConTemp","ConPop_IncTemp",
#                    "DecPop_ConTemp","DecPop_IncTemp",
#                    "IncPop_ConTemp","IncPop_IncTemp")){




plot_estimates_by_strat <- function(){



pdf(file=paste(orig.dir,"/testfolder/est_by_strata_",scenario1,"_",str_dir,".pdf",sep=""))


for(s in short_names){

for(ssn in c("SPRING","FALL")){
 year_min = 3

  ssn1 = ifelse(ssn=="SPRING",1,2)

  #model values
  P_est = readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Simulation_Results\\",scenario1,"\\pop_by_strata_",scenario1,".RDS",sep=""))
  P_est = P_est[[s]]
  P_est = P_est[P_est$season==ssn1,] #by season
  #sometimes stratum is labeled statum
  if(names(P_est)[[3]]=="statum"){P_est$stratum = P_est$statum}
  
  #sum by stratum to compare with pop_by_season
  #P_est_total <- P_est %>% group_by(year,season) %>% summarise(biomass2=sum(biomass))

  #Stratified mean estimates (Yes Noise(YN) and No Noise (NN))
  SM_est_NN = strat_mean_by_strat[[s]][["NoNoise_"]]
  SM_est_YN = strat_mean_by_strat[[s]][["WithNoise_"]]

  SM_est_NN = SM_est_NN[SM_est_NN$Season==ssn,]
  SM_est_YN = SM_est_YN[SM_est_YN$Season==ssn,]#by season

  #Vast estimates (Y noise, Y covs and N versions)
  V_est_NC_NN = vast_by_str[[s]][["_NoCovs_"]][["NoNoise_"]]
  V_est_NC_YN = vast_by_str[[s]][["_NoCovs_"]][["WithNoise_"]]
  V_est_YC_NN = vast_by_str[[s]][["_WithCov_"]][["NoNoise_"]]
  V_est_YC_YN = vast_by_str[[s]][["_WithCov_"]][["WithNoise_"]]
  
  V_est_NC_NN = V_est_NC_NN[V_est_NC_NN$season==ssn,] #by season
  V_est_NC_YN = V_est_NC_YN[V_est_NC_YN$season==ssn,]
  V_est_YC_NN = V_est_YC_NN[V_est_YC_NN$season==ssn,]
  V_est_YC_YN = V_est_YC_YN[V_est_YC_YN$season==ssn,]

  #sum biomass by stratum
  V_est_NC_NN <- V_est_NC_NN %>%
  group_by(year,stratum) %>%
  summarise(biomass = sum(Biomass_est)) %>% as.data.frame()
  
  V_est_NC_YN <- V_est_NC_YN %>%
    group_by(year,stratum) %>%
    summarise(biomass = sum(Biomass_est)) %>% as.data.frame()
  
  V_est_YC_NN <- V_est_YC_NN %>%
    group_by(year,stratum) %>%
    summarise(biomass = sum(Biomass_est)) %>% as.data.frame()
  
  V_est_YC_YN <- V_est_YC_YN %>%
    group_by(year,stratum) %>%
    summarise(biomass = sum(Biomass_est)) %>% as.data.frame()


 
  
ppp<- ggplot() +
  
  #this way plots data by season
  geom_point(data = subset(as.data.frame(P_est),year>=year_min), aes(x=as.numeric(year),y=biomass, group = stratum, color = "Model"),size=3) +
  geom_line(data = subset(as.data.frame(P_est),year>=year_min), aes(x=as.numeric(year),y=biomass, group =stratum, color = "Model"),size=1) +

  #plot VAST estimate without covariates with NO noise
 # geom_errorbar(data=subset(V_est,Year>=year_min),aes(x=Year,y=mean.str,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov No Noise"),width=.3) +
  #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
  geom_point(data=V_est_NC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov No Noise"),size=2)+
  geom_line(data=V_est_NC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov No Noise"))+
  
  geom_point(data=V_est_NC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov Yes Noise"),size=2)+
  geom_line(data=V_est_NC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov Yes Noise"))+
  
  geom_point(data=V_est_YC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov No Noise"),size=2)+
  geom_line(data=V_est_YC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov No Noise"))+
  
  geom_point(data=V_est_YC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov Yes Noise"),size=2)+
  geom_line(data=V_est_YC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov Yes Noise"))+
  
  #plot stratified calculation data
  #geom_errorbar(data=SM_est,aes(x=year,y=mean.yr.str,group=statrum,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean No Noise"),width=.3) +
  # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
  geom_point(data=SM_est_NN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean No Noise"))+
  geom_line(data=SM_est_NN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean No Noise"))+
  
  geom_point(data=SM_est_YN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean Yes Noise"))+
  geom_line(data=SM_est_YN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean Yes Noise"))+
  
  ggforce::facet_wrap_paginate(~ stratum, scales="free", ncol = 1, nrow = 4) +
 # facet_wrap(~ stratum, scales = "free", ncol =2) +
  
labs(x="year",y="Biomass", title = paste(s," By strata ",scenario1," ",ssn,sep=""), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=8))


#RECORD NUMBER OF PAGES THEN LOOP THROUGH AND PRINT THEM
npgs = ggforce::n_pages(ppp)

for(i in seq(npgs)){
  ppp<- ggplot() +
    
    #this way plots data by season
    geom_point(data = subset(as.data.frame(P_est),year>=year_min), aes(x=as.numeric(year),y=biomass, group = stratum, color = "Model"),size=3) +
    geom_line(data = subset(as.data.frame(P_est),year>=year_min), aes(x=as.numeric(year),y=biomass, group =stratum, color = "Model"),size=1) +
    
    #plot VAST estimate without covariates with NO noise
    # geom_errorbar(data=subset(V_est,Year>=year_min),aes(x=Year,y=mean.str,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST No Cov No Noise"),width=.3) +
    #geom_linerange(data=subset(VAST_data[[s]][[folder]],Year>=year_min),aes(x=Year,y=Estimate,group=season,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST Estimate")) +
    geom_point(data=V_est_NC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov No Noise"),size=2)+
    geom_line(data=V_est_NC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov No Noise"))+
    
    # geom_point(data=V_est_NC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov Yes Noise"),size=2)+
    # geom_line(data=V_est_NC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST No Cov Yes Noise"))+
    
    geom_point(data=V_est_YC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov No Noise"),size=2)+
    geom_line(data=V_est_YC_NN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov No Noise"))+
    
    # geom_point(data=V_est_YC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov Yes Noise"),size=2)+
    # geom_line(data=V_est_YC_YN,aes(x=year,y=biomass,group=stratum, color = "VAST Yes Cov Yes Noise"))+
    
    #plot stratified calculation data
    #geom_errorbar(data=SM_est,aes(x=year,y=mean.yr.str,group=statrum,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean No Noise"),width=.3) +
    # geom_linerange(data=as.data.frame(SRS_data[[s]][[folder]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean")) +
    geom_point(data=SM_est_NN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean No Noise"))+
    geom_line(data=SM_est_NN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean No Noise"))+
    
    # geom_point(data=SM_est_YN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean Yes Noise"))+
    # geom_line(data=SM_est_YN,aes(x=year,y=mean.yr.strr,group=stratum, color = "Strat Mean Yes Noise"))+
    
    ggforce::facet_wrap_paginate(~ stratum, scales="free", ncol = 1, nrow = 4,page=i) +
    # facet_wrap(~ stratum, scales = "free", ncol =2) +
    
    labs(x="year",y="Biomass", title = paste(s," By strata ",scenario1," ",str_dir," ",ssn,sep=""), color ="" )+
    
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          title=element_text(size=8))
  
  print(ppp)
}

}
}


dev.off()

}

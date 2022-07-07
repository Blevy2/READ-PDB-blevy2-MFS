#THIS SCRIPT IS A PIECE OF RUN_SURVEY_BENS_NEW WHERE THE STRATIFIED MEAN CALCULATION IS MADE
#THIS SCRIPT ALLOWS US TO CALULATE THE STRATIFIED MEAN FOR A SMALLER NUMER OF NOISELESS SURVEYS



##################################################################################################
#THINGS WE NEED
##################################################################################################
scenario <- "ConPop_IncTemp"

n_spp <- 3

#survey results without noise
list_all <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\list_all_",scenario,".RDS",sep=""))

#simulation results
result <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\result_",scenario,".RDS",sep=""))

#random survey locations
surv_random <- readRDS(paste("E:\\READ-PDB-blevy2-MFS2\\GB_Results\\",scenario,"\\surv_random_",scenario,".RDS",sep=""))

#spp1 spp2 spp3
short_names <- c("YT","Cod","Had") 

#strata that each species occupies. Used to calculate stratified random mean of each
strata_species <- list()
strata_species[["YT"]] <-  c(13,14,15,16,17,18,19,20,21)
strata_species[["Cod"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25)
strata_species[["Had"]] <- c(13,14,15,16,17,18,19,20,21,22,23,24,25,29,30)

##################################################################################################



#ADD TRUE MODEL POPULATION VALUES TO SURVEY DATA TABLES
for(iter in seq(length(list_all))){
  
  temp <- matrix(data=0,nrow=length(list_all[[iter]][,1]),ncol=n_spp)
  
  for(samp in seq(length(list_all[[iter]][,1]))){
    
    x = as.numeric(list_all[[iter]][samp,2]) #x in second column
    y = as.numeric(list_all[[iter]][samp,3]) #y in third column
    wk = as.numeric(list_all[[iter]][samp,11]) #week in 11th column
    yr = as.numeric(list_all[[iter]][samp,7]) #year in 7th column
    
    temp[samp,1] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp1"]],na.rm=T) #YT is spp1
    temp[samp,2] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp2"]],na.rm=T) #Cod is spp2
    temp[samp,3] <- sum(result[[iter]]$pop_bios[[(wk+(52*(yr-1)))]][["spp3"]],na.rm=T) #Had is spp3
    
  }
  colnames(temp) <- c("YT","Cod","Had") 
  list_all[[iter]] <- cbind(list_all[[iter]],temp)
  
}

#FIND MEAN VALUE BY SEASON USING ABOVE INFORMATION. USE MEAN OF TWO SURVEY WEEKS FOR EACH SEASON
season_wks <- list(c(13,14),c(37,38))
pop_by_season <- list()

for(iter in seq(length(list_all))){
  for(s in short_names){ 
    temp <- data.frame()
    idx <- 1
    for(yr in seq(3,22)){
      
      for(season in seq(2)){
        
        
        
        temp[idx,1] <- yr
        temp[idx,2] <- season
        
        #use values in given year for weeks in specified season. only use single strata because entire population summariezed in each strata in above loop
        temp[idx,3] <- mean(as.numeric(list_all[[iter]][((as.numeric(list_all[[iter]][,"year"]==yr)) & (as.numeric(list_all[[iter]][,"week"]) %in% season_wks[[season]]) & (as.numeric(list_all[[iter]][,"stratum"]==29)) ),s]))
        
        
        idx <- idx + 1    
      }  
      
    }
    colnames(temp) <- c("year","season","biomass")
    pop_by_season[[s]][[iter]] <- temp
  }
}

#BELOW WILL TAKE A MINUTE

#choose some strata to exclude, if desired
exclude_strata <- FALSE

ifelse(exclude_strata, 
       #George's Bank Setup by species
       #YT            Cod          Haddock
       exclude <- list(c(13,14,15,17,18), c(23,24,25), c(23,24,25,29,30)),
       
       #exclude none
       exclude <- list(c(0),c(0),c(0)) #3 species
)

#NOW WE NEED TO CREATE A STRATIFIED MEAN FROM EACH OF THESE SAMPLES
#there are #strata * #iterations * #samp_per_iter total samples

#I AM COPYING FROM CALC_SRS_INDEX_SURVEY_BENS, which was adapted from Liz's code to create below
library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)

#stop output from below using this option
options(dplyr.summarise.inform = FALSE)

#load file to calculate the stratified mean
source("TestScripts/Calc_strat_mean/fn_srs_survey_BENS.R")



#setup dimensions for each species- 1 for each strata
strat_mean_all <- vector("list",length(seq(n_spp)))

for(s in seq(n_spp)){
  
  strat_mean_all[[s]] <- vector("list",length(list_all)) 
}



#go through each strata survey, iteration, sample


for(iter in seq(length(list_all))){
  print(iter)
  
  #if any NA columns make them zero
  list_all[[iter]][is.na(list_all[[iter]])]=0
  
  
  
  # calculate SRS estimates ====
  
  for(s in seq(n_spp)){
    
    
    #DEFINE INDIVIDUAL STRATUM AREAS 
    stratum <- sort(unique(surv_random$log.mat[,4]))
    
    STRATUM_AREA <- na.omit(surv_random$cells_per_strata) # old way: rep(10000/nstrata,nstrata) #100x100 grid so each corner has area 2500
    
    sv.area <- as_tibble(data.frame(stratum,STRATUM_AREA))
    
    #remove stratum that species does not occupy
    sv.area <- sv.area[(sv.area$stratum %in% strata_species[[short_names[s]]]),]#sv.area %>% slice(-exclude)
    
    #remove strata to exclude from stratified mean calculation
    sv.area <- sv.area[!(sv.area$stratum %in% exclude[[s]]),]#sv.area %>% slice(-exclude)
    
    spp <- as_tibble(list_all[[iter]],header=T) #pull out entire survey matrix
    
    ##remove strata to exclude from stratified mean calculation
    spp <- spp[(spp$stratum %in% strata_species[[short_names[s]]]),]
    spp <- spp[!(spp$stratum %in% exclude[[s]]),]
    
    spp$year <- as.numeric(spp$year)
    
    
    # get total area of stock ====
    spp.strata <- unique(spp$stratum)
    spp.strata <- as.numeric(spp.strata)
    
    spp.area <- sum(sv.area$STRATUM_AREA[sv.area$stratum %in% spp.strata]) #TOTAL AREA OF ALL STRATA
    
    
    
    temp <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=1, sppname = paste0("spp", s, sep="")  )   # if strata=NULL, the function will use the unique strata set found in df
    # View(temp)
    strat_mean_all[[s]][[iter]] <- temp %>%
      mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
             CV.absolute=sd.mean.yr.absolute/mean.yr.absolute) # if strata=NULL, the function will use the unique strata set found in df
    
    strat_mean_all[[s]][[iter]] <- data.matrix(strat_mean_all[[s]][[iter]])
    
    
    
  }
  
  colnames(strat_mean_all[[s]][[iter]]) <- c("year","mean.yr","var.mean.yr","sd.mean.yr","CV","season","mean.yr.absolute","sd.mean.yr.absolute","CV.absolute")
  
  
  
  
}


names(strat_mean_all) <- short_names


if(exclude_strata == FALSE){
  
  strat_mean_all_allstrata <- strat_mean_all
  
}

if(exclude_strata == TRUE){
  
  strat_mean_all_exclude <- strat_mean_all
  
}


##########################################################################################
# Preparing things to plot
##########################################################################################


#copying plot_pop_summary to summarize yearly population estimates
#assumes we have summarized version of simulations called res
results_df <- list()

for(iter in seq(length(list_all))){
  
  res <- result[[iter]]
  
  n_spp <- length(res[["pop_summary"]]) 
  res_df <- lapply(seq_len(n_spp), function(x) {
    res_spp <- lapply(names(res[["pop_summary"]][[x]]), function(x1) {
      x1_res <- tidyr::gather(as.data.frame(t(res[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
      if(x1 == "Bio.mat" | x1 == "Bio.mat.sd") {	res_out <- data.frame("pop" = rep(short_names[[x]], length.out = nrow(x1_res)), 
                                                                       "metric" = x1, 
                                                                       "year" = as.numeric(x1_res$year), 
                                                                       "day" = rep(1:358, length.out = nrow(x1_res)),#changed 362 to 358
                                                                       "julien_day" = seq_len(nrow(x1_res)),
                                                                       "data" = x1_res$value) 
      
      return(res_out)
      
      }
      if(x1 == "Rec.mat" | x1 == "Rec.mat.sd") { res_out <- data.frame("pop" = rep(short_names[[x]], length.out = nrow(x1_res)), 
                                                                       "metric" = x1 , 
                                                                       "year" = as.numeric(seq_len(nrow(x1_res))), 
                                                                       "day" = rep(1, length.out = nrow(x1_res)),
                                                                       "julien_day" = rep(1, length.out = nrow(x1_res)),
                                                                       "data" = x1_res$value) 
      
      return(res_out)
      
      }
      
    })
    return(do.call(rbind, res_spp))
  })
  
  results_df[[iter]] <- do.call(rbind, res_df)
  
}





# #ANNUAL POP BY SPECIES

annual_species <- list()

for(iter in seq(length(list_all))){
  
  for(s in seq(length(short_names))){
    annual_species[[short_names[s]]][[iter]] <- results_df[[iter]] %>% filter(metric == "Bio.mat", day == 1, pop == short_names[s]) %>% 
      group_by(pop,year) %>% summarise(data = sum(data))
    
  }
}




pdf(file=paste("Results/GB_error_plots/Individual_SRS_",scenario,".pdf",sep=""))


nyears <- 20

#for error calculation
SRS_error_spring_exclude <- list()
SRS_error_fall_exclude <- list()
SRS_spring_exclude <- list()
SRS_fall_exclude <- list()

SRS_error_spring_allstrata <- list()
SRS_error_fall_allstrata <- list()
SRS_spring_allstrata <- list()
SRS_fall_allstrata <- list()

#plot stratified calculation and population estimate on same plot

#first make model output have 2 seasons to match the stratified mean calcs

for(iter in seq(length(list_all))){
  
  print(iter)
  
  for(s in seq(length(annual_species))){
    annual_species[[short_names[s]]][[iter]] <- rbind(annual_species[[short_names[s]]][[iter]][3:22,],annual_species[[short_names[s]]][[iter]][3:22,])
    annual_species[[short_names[s]]][[iter]]$season <- c(rep(1,nyears),rep(2,nyears)) #spring = season 1, fall = season 2
    annual_species[[short_names[s]]][[iter]]$year <- as.numeric(rep(seq(3,22),2))
  }
  
  
  
  SRS_data <- list()
  
  
  for(s in short_names){ 
    
    #pull out strat mean calc
   ifelse(exclude_strata == FALSE, SRS_data[[s]] <-  strat_mean_all_allstrata[[s]][[iter]],   SRS_data[[s]] <-  strat_mean_all_exclude[[s]][[iter]])
    
    #model value (old way)
    #model[[s]][[iter]] <-  annual_species[[s]][[iter]][annual_species[[s]][[iter]]$season==1,"data"]
    # 
    #  #SRS spring estimate
    # SRS_spring[[s]][[iter]] <- strat_mean_all[[s]][[iter]][strat_mean_all[[s]][[iter]][,"season"]==1,"mean.yr.absolute"]
    # 
    # #SRS fall estimate
    # SRS_fall[[s]][[iter]] <- strat_mean_all[[s]][[iter]][strat_mean_all[[s]][[iter]][,"season"]==2,"mean.yr.absolute"]
    # 
    # #calculate SPRING error from each iteration
    # SRS_error_spring[[s]][[iter]] <- norm(model[[s]][[iter]] - SRS_spring[[s]][[iter]] , type="2") / norm(model[[s]][[iter]] , type ="2")
    # 
    # #calculate FALL error from each iteration
    # SRS_error_fall[[s]][[iter]] <- norm(model[[s]][[iter]] - SRS_fall[[s]][[iter]] , type="2") / norm(model[[s]][[iter]] , type ="2")
    # 
    model_spring = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==1,"biomass"]
    
    model_fall = pop_by_season[[s]][[iter]][pop_by_season[[s]][[iter]]$season==2,"biomass"]
    
    if(exclude_strata == FALSE){
      
      #SRS spring estimate
      SRS_spring_allstrata[[s]][[iter]] <- strat_mean_all_allstrata[[s]][[iter]][strat_mean_all_allstrata[[s]][[iter]][,"season"]==1,"mean.yr.absolute"]
      
      #SRS fall estimate
      SRS_fall_allstrata[[s]][[iter]] <- strat_mean_all_allstrata[[s]][[iter]][strat_mean_all_allstrata[[s]][[iter]][,"season"]==2,"mean.yr.absolute"]
      
      #calculate SPRING error from each iteration
      SRS_error_spring_allstrata[[s]][[iter]] <- norm(model_spring- SRS_spring_allstrata[[s]][[iter]] , type="2") / norm(model_spring , type ="2")
      
      #calculate FALL error from each iteration
      SRS_error_fall_allstrata[[s]][[iter]] <- norm(model_fall - SRS_fall_allstrata[[s]][[iter]] , type="2") / norm(model_fall , type ="2")
    }
    
    if(exclude_strata == TRUE){
      
      #SRS spring estimate
      SRS_spring_exclude[[s]][[iter]] <- strat_mean_all_exclude[[s]][[iter]][strat_mean_all_exclude[[s]][[iter]][,"season"]==1,"mean.yr.absolute"]
      
      #SRS fall estimate
      SRS_fall_exclude[[s]][[iter]] <- strat_mean_all_exclude[[s]][[iter]][strat_mean_all_exclude[[s]][[iter]][,"season"]==2,"mean.yr.absolute"]
      
      #calculate SPRING error from each iteration
      SRS_error_spring_exclude[[s]][[iter]] <- norm(model_spring- SRS_spring_exclude[[s]][[iter]] , type="2") / norm(model_spring , type ="2")
      
      #calculate FALL error from each iteration
      SRS_error_fall_exclude[[s]][[iter]] <- norm(model_fall - SRS_fall_exclude[[s]][[iter]] , type="2") / norm(model_fall , type ="2")
    }
    
  }
  
  
  long_names <- c("Yellowtail Flounder","Cod","Haddock")
  
  
  #par(mfrow = c(1,3), mar = c(1, 1, 1, 1))
  
  #for(s in short_names){
  # 
  # #OLD WAY
  # #initiate ggplot
  # p<- ggplot() +
  #   #plot stratified calculation data
  #   geom_errorbar(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
  #   geom_point(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
  #   geom_line(data=as.data.frame(SRS_data[[s]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
  #   #plot model data
  #   geom_point(data = as.data.frame(annual_species[[s]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
  #   geom_line(data = as.data.frame(annual_species[[s]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
  #   
  #   facet_wrap(~ season) +
  #   labs(x="year",y="Biomass", title = long_names[idx], color ="" ) 
  # idx<-idx+1
  # 
  # print(p)
  #}
  
  #NEW WAY PLOTTING 3 TOGETHER ON SAME PAGE
  
  #YTF
  p1<- ggplot() +
    #plot stratified calculation data
    geom_errorbar(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
    geom_point(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    geom_line(data=as.data.frame(SRS_data[[1]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    
    #plot model data
    #this way plots annual data
    #geom_point(data = as.data.frame(annual_species[[1]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    #geom_line(data = as.data.frame(annual_species[[1]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    
    #this way plots data by season
    geom_point(data = as.data.frame(pop_by_season[[1]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model")) +
    geom_line(data = as.data.frame(pop_by_season[[1]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model")) +
    
    
    facet_wrap(~ season) +
    labs(x="year",y="Biomass", title = long_names[1], color ="" ) 
  
  #COD
  p2<- ggplot() +
    #plot stratified calculation data
    geom_errorbar(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
    geom_point(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    geom_line(data=as.data.frame(SRS_data[[2]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    
    #plot model data
    #this way plots annual data
    #geom_point(data = as.data.frame(annual_species[[2]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    #geom_line(data = as.data.frame(annual_species[[2]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    
    #this way plots data by season
    geom_point(data = as.data.frame(pop_by_season[[2]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model")) +
    geom_line(data = as.data.frame(pop_by_season[[2]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model")) +
    
    facet_wrap(~ season) +
    labs(x="year",y="Biomass", title = long_names[2], color ="" )
  
  #HAD
  p3<- ggplot() +
    #plot stratified calculation data
    geom_errorbar(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Stratified Mean"),width=.3) +
    geom_point(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    geom_line(data=as.data.frame(SRS_data[[3]]),aes(x=year,y=mean.yr.absolute,group=season, color = "Stratified Mean"))+
    
    #plot model data
    #this way plots annual data
    #geom_point(data = as.data.frame(annual_species[[3]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    #geom_line(data = as.data.frame(annual_species[[3]][[iter]]), aes(x=as.numeric(year),y=data, group =season, color = "Model")) +
    
    #this way plots data by season
    geom_point(data = as.data.frame(pop_by_season[[3]][[iter]]), aes(x=as.numeric(year),y=biomass, group = season, color = "Model")) +
    geom_line(data = as.data.frame(pop_by_season[[3]][[iter]]), aes(x=as.numeric(year),y=biomass, group =season, color = "Model")) +
    
    facet_wrap(~ season) +
    labs(x="year",y="Biomass", title = long_names[3], color ="" )
  
  gridExtra::grid.arrange(p1,p2,p3,nrow=3)
  
  
}

dev.off()



if(exclude_strata == FALSE){

df_spring_allstrata <- tibble(iter = rep(1:length(list_all),n_spp),
                    error = c(SRS_error_spring_allstrata[[1]], SRS_error_spring_allstrata[[2]], SRS_error_spring_allstrata[[3]]),
                    species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                    
                    season = rep(rep("spring",length(list_all)),n_spp),
)

df_fall_allstrata <- tibble(iter = rep(1:length(list_all),n_spp),
                  error = c(SRS_error_fall_allstrata[[1]], SRS_error_fall_allstrata[[2]], SRS_error_fall_allstrata[[3]]),
                  species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                  
                  season = rep(rep("fall",length(list_all)),n_spp),
)

}

if(exclude_strata == TRUE){
  
  df_spring_exclude <- tibble(iter = rep(1:length(list_all),n_spp),
                      error = c(SRS_error_spring_exclude[[1]], SRS_error_spring_exclude[[2]], SRS_error_spring_exclude[[3]]),
                      species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                      
                      season = rep(rep("spring",length(list_all)),n_spp),
  )
  
  df_fall_exclude <- tibble(iter = rep(1:length(list_all),n_spp),
                    error = c(SRS_error_fall_exclude[[1]], SRS_error_fall_exclude[[2]], SRS_error_fall_exclude[[3]]),
                    species = c(rep("YTF",length(list_all)),rep("Cod",length(list_all)),rep("Had",length(list_all))),
                    
                    season = rep(rep("fall",length(list_all)),n_spp),
  )
  
}


ifelse(exclude_strata == FALSE, df <- rbind(df_fall_allstrata,df_spring_allstrata), df <- rbind(df_fall_exclude,df_spring_exclude))


#create data frame containing mean values for each group
ifelse(exclude_strata == FALSE,
       means_sd_allstrata <- ddply(df, .(species,season), summarise, mean = mean(as.numeric(error)), std_dev = sd(as.numeric(error))),
       means_sd_exclude <- ddply(df, .(species,season), summarise, mean = mean(as.numeric(error)), std_dev = sd(as.numeric(error)))
)

ifelse(exclude_strata == FALSE,
       means_sd <- means_sd_allstrata ,
       means_sd <- means_sd_exclude
)


#Error scatterplots

#1) to plot a single scenario, run just cc below and print(cc)
#2) to plot two scenarios on top of each other, store the first as cc and then run code below doing cc +

library(ggplot2)
cc<-ggplot(data=df,
           aes(x=iter,y=as.numeric(error))) +
  geom_point(color="blue")+
  ylim(0,1)+
  facet_grid(season ~ species)+
  geom_hline(aes(yintercept = mean), data = means_sd, color = "blue") 

print(cc)

# 
# ggsave(filename = paste("Results/GB_error_plots/Individussssal_SRS_",scenario,".pdf",sep=""),
#        plot = last_plot())
# 



#run this second to plot on top of each other
cc+  geom_point(data=df,color="red",
                aes(x=iter,y=as.numeric(error)))+
  facet_grid(season ~ species) +
  geom_hline(aes(yintercept = mean), data = means_sd, color = "red")





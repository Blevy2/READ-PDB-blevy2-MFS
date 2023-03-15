#THIS SCRIPT READS IN CSV RELATED TO RATIO PLOTS AND CALCULATED SUMMARY INFORMATION


ratio_data <- read.csv(file = paste(getwd(),"/VAST/ForPaper/All_Ratio_summary_info_new.csv",sep=""))

short_names <- c("YTF","Cod","Had")

summary_info <- list()


#THESE DONT MAKE SENSE BECAUSE THEY MIX ALL STRATA WITH REDUCED STRATA
# #mean and std dev of all vast
# summary_info[["mean_std_all_VAST"]] <- c( mean(ratio_data[ratio_data[,"Estimate"]=="VAST","Mean_overall_finite"]) , sd(ratio_data[ratio_data[,"Estimate"]=="VAST","Mean_overall_finite"]))
# #mean and std dev of all sm
# summary_info[["mean_std_all_SM"]] <-c( mean(ratio_data[ratio_data[,"Estimate"]=="Strat Mean","Mean_overall_finite"]) , sd(ratio_data[ratio_data[,"Estimate"]=="Strat Mean","Mean_overall_finite"]))
# 





#USING ALL STRATA...
#MEAN AND STD OF ALL VAST AND SM BY SEASON

  for(E in c("VAST","Strat Mean")){
    d <- ratio_data %>%
      filter(Strata == "All", Estimate == E)
    
    ifelse(E=="Strat Mean", EE <- "SM", EE <- "VAST")
    
    summary_info[[paste("mean_std_ALL_spring_",EE,sep="")]] <- c(mean(d[,"Mean_Spring"]),sd(d[,"Mean_Spring"]))
    summary_info[[paste("mean_std_ALL_fall_",EE,sep="")]] <- c(mean(d[,"Mean_Fall"]),sd(d[,"Mean_Fall"]))
    
  }


#USING ALL STRATA...
#MEAN AND STD OF ALL VAST AND SM BY SEASON AND SPECIES
for(s in short_names){
  for(E in c("VAST","Strat Mean")){
d <- ratio_data %>%
    filter(Species == s, Strata == "All", Estimate == E)

ifelse(E=="Strat Mean", EE <- "SM", EE <- "VAST")

summary_info[[paste("mean_std_",s,"_spring_",EE,sep="")]] <- c(mean(d[,"Mean_Spring"]),sd(d[,"Mean_Spring"]))
summary_info[[paste("mean_std_",s,"_fall_",EE,sep="")]] <- c(mean(d[,"Mean_Fall"]),sd(d[,"Mean_Fall"]))

  }
}




#percent of stratified mean estimates with all strata that were too low (<1) compare this to mean value

#USING ALL STRATA...
#ALL VAST AND SM >1

for(E in c("VAST","Strat Mean")){
  d <- ratio_data %>%
    filter(Strata == "All", Estimate == E)
  
  ifelse(E=="Strat Mean", EE <- "SM", EE <- "VAST")
  
  summary_info[[paste("Pct_>1_",EE,sep="")]] <- sum(d$Total.1)/(40*length(d$Total.1))
  
}





#MEAN AND STD OF ALL VAST AND SM

#USING ALL STRATA...

for(E in c("VAST","Strat Mean")){
  d <- ratio_data %>%
    filter(Strata == "All", Estimate == E)
  
  ifelse(E=="Strat Mean", EE <- "SM", EE <- "VAST")
  
  summary_info[[paste("mean_std_ALL",EE,sep="")]] <- c(mean(d[,"Mean_overall_finite"]),sd(d[,"Mean_overall_finite"]))

  
}



#mean and std of VAST with and without covariates

#USING ALL STRATA...

for(C in c("NoCovs","WithCo")){
  d <- ratio_data %>%
    filter(Estimate == "VAST",Strata == "All", Covariate == C)

  summary_info[[paste("mean_std_ALL_",C,sep="")]] <- c(mean(d[,"Mean_overall_finite"]),sd(d[,"Mean_overall_finite"]))
  
  
}






#mean and std dev excluding strata vs including all
#both with and without covariates


#USING ALL STRATA...

for(S in c("All","Reduced")){
for(E in c("VAST","Strat Mean")){
for(C in c("NoCovs","WithCo")){
  d <- ratio_data %>%
    filter(Estimate == E , Covariate == C, Strata == S)
  
  summary_info[[paste("mean_std_ALL_",C,"_",E,"_",S,sep="")]] <- c(mean(d[,"Mean_overall_finite"]),sd(d[,"Mean_overall_finite"]))
  
}
}
}


write.csv(summary_info, file = paste(getwd(),"/Manuscript/CSVs/Ratio_summary_new.csv",sep=""))



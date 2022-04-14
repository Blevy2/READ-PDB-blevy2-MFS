#This script calculates various parameters for mixfishsim input

##################################################################
#first calculate weighted average of prerecruits and adults
#reads in weight info (avg weight by age and avg number by age) for each species to calculate weighted mean of adults
#This value is used to calculate phi0 and in delay_diff for MFS
##################################################################


#1- YELLOWTAIL FLOUNDER

YT_all <- read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\Stock Assessment Review Committee (SARC) Docs\\YT WEight Info.csv",header=TRUE,stringsAsFactors=FALSE)

YT_row <- nrow(YT_all) #total number of observations (top half for weight bottom half for corresponding numbers)

YT_PreRecAge <- 1 #age before they are recruited (1)
YT_AdultAge <-  2 #age when they become adults (2)

YT_W <- YT_all[1:(YT_row/2),] #weight by age
YT_N <- YT_all[(YT_row/2+1):YT_row,] #number by age

#Pre recruit weight. Add +1 bc col 1 has year
YT_PRW <- as.numeric(YT_W[,(YT_PreRecAge+1)])

#Pre recruit numbers. Add +1 bc col 1 has year
YT_PRN <- YT_N[,(YT_PreRecAge+1)]
#remove commas
YT_PRN <- as.numeric(gsub(",", "", YT_PRN))

#calculate mean 
YT_PRN_mean <- mean(YT_PRN)
YT_PRW_mean <- mean(YT_PRW)

#Fully recruited (ie adult) weight. Add +1 bc col 1 has year
YT_FRW <- YT_W[,(YT_AdultAge+1):ncol(YT_W)]
#Pre recruit numbers. Add +1 bc col 1 has year
YT_FRN <- YT_N[,(YT_AdultAge+1):ncol(YT_W)]
#change each of above from characters to numeric
temp1 <- matrix(0,ncol=ncol(YT_FRW),nrow=nrow(YT_FRW))
temp2 <- matrix(0,ncol=ncol(YT_FRW),nrow=nrow(YT_FRW))
for(i in seq(ncol(YT_FRW))){
  
  temp1[,i] <- as.numeric(YT_FRW[,i])
  
  temp3 <- YT_FRN[,i]
  temp2[,i] <- as.numeric(gsub(",","",temp3))
  
}
YT_FRW <- temp1
YT_FRN <- temp2

#calculate mean for each year
YT_FRW_mean <- colMeans(YT_FRW)
YT_FRN_mean <- colMeans(YT_FRN)


#final adult weighted mean is sum(weight_i*x_i)/(sum(weights))
YT_adult_weightmean <- sum(YT_FRW_mean*YT_FRN_mean)/sum(YT_FRN_mean)

YT_prerec_weightmean <- sum(YT_PRN_mean*YT_PRW_mean)/sum(YT_PRN_mean)




#2- COD

Cod_all <- read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\Stock Assessment Review Committee (SARC) Docs\\Cod WEight Info.csv",header=TRUE,stringsAsFactors=FALSE)

Cod_row <- nrow(Cod_all) #total number of observations (top half for weight bottom half for corresponding numbers)

Cod_PreRecAge <- 1 #age before they are recruited (1)
Cod_AdultAge <-  2 #age when they become adults (2)

Cod_W <- Cod_all[1:(Cod_row/2),] #weight by age
Cod_N <- Cod_all[(Cod_row/2+1):Cod_row,] #number by age

#Pre recruit weight. Add +1 bc col 1 has year
Cod_PRW <- as.numeric(Cod_W[,(Cod_PreRecAge+1)])

#Pre recruit numbers. Add +1 bc col 1 has year
Cod_PRN <- Cod_N[,(Cod_PreRecAge+1)]
#remove commas
Cod_PRN <- as.numeric(gsub(",", "", Cod_PRN))

#calculate mean 
Cod_PRN_mean <- mean(Cod_PRN)
Cod_PRW_mean <- mean(Cod_PRW)

#Fully recruited (ie adult) weight. Add +1 bc col 1 has year
Cod_FRW <- Cod_W[,(Cod_AdultAge+1):ncol(Cod_W)]
#Pre recruit numbers. Add +1 bc col 1 has year
Cod_FRN <- Cod_N[,(Cod_AdultAge+1):ncol(Cod_W)]
#change each of above from characters to numeric
temp1 <- matrix(0,ncol=ncol(Cod_FRW),nrow=nrow(Cod_FRW))
temp2 <- matrix(0,ncol=ncol(Cod_FRW),nrow=nrow(Cod_FRW))
for(i in seq(ncol(Cod_FRW))){
  
  temp1[,i] <- as.numeric(Cod_FRW[,i])
  
  temp3 <- Cod_FRN[,i]
  temp2[,i] <- as.numeric(gsub(",","",temp3))
  
}
Cod_FRW <- temp1
Cod_FRN <- temp2

#calculate mean for each year
Cod_FRW_mean <- colMeans(Cod_FRW)
Cod_FRN_mean <- colMeans(Cod_FRN)


#final adult weighted mean is sum(weight_i*x_i)/(sum(weights))
Cod_adult_weightmean <- sum(Cod_FRW_mean*Cod_FRN_mean)/sum(Cod_FRN_mean)

Cod_prerec_weightmean <- sum(Cod_PRN_mean*Cod_PRW_mean)/sum(Cod_PRN_mean)





# HADDOCK

#mean weight by age obtained from digitizing Figure B138 in Haddock assessment from Liz Brooks
#Age
# 1 0.19
# 2 0.46
# 3 0.74
# 4 0.94
# 5 1.14
# 6 1.28
# 7 1.44
# 8 1.60
# 9 1.85

Had_all <- read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\Stock Assessment Review Committee (SARC) Docs\\Haddock Weight Info.csv",header=TRUE,stringsAsFactors=FALSE)

Had_row <- nrow(Had_all) #total number of observations (top half for weight bottom half for corresponding numbers)

Had_PreRecAge <- 1 #age before they are recruited (1)
Had_AdultAge <-  2 #age when they become adults (2)

Had_W <- Had_all[Had_row,] #weight by age
Had_N <- Had_all[(1:12),] #number by age

#Pre recruit weight. Add +1 bc col 1 has year
Had_PRW <- as.numeric(Had_W[,(Had_PreRecAge+1)])

#Pre recruit numbers. Add +1 bc col 1 has year
Had_PRN <- Had_N[,(Had_PreRecAge+1)]
#remove commas
Had_PRN <- as.numeric(gsub(",", "", Had_PRN))

#calculate mean 
Had_PRN_mean <- mean(Had_PRN)
Had_PRW_mean <- mean(Had_PRW)

#Fully recruited (ie adult) weight. Add +1 bc col 1 has year
Had_FRW <- Had_W[,(Had_AdultAge+1):ncol(Had_W)]
#Pre recruit numbers. Add +1 bc col 1 has year
Had_FRN <- Had_N[,(Had_AdultAge+1):ncol(Had_W)]
#change each of above from characters to numeric
temp1 <- matrix(0,ncol=ncol(Had_FRW),nrow=nrow(Had_FRW))
temp2 <- matrix(0,ncol=ncol(Had_FRN),nrow=nrow(Had_FRN))
for(i in seq(ncol(Had_FRW))){
  
  temp1[,i] <- as.numeric(Had_FRW[,i])
  
  temp3 <- Had_FRN[,i]
  temp2[,i] <- as.numeric(gsub(",","",temp3))
  
}
Had_FRW <- temp1
Had_FRN <- temp2

#calculate mean for each year
Had_FRW_mean <- colMeans(Had_FRW)
Had_FRN_mean <- colMeans(Had_FRN)


#final adult weighted mean is sum(weight_i*x_i)/(sum(weights))
Had_adult_weightmean <- sum(Had_FRW_mean*Had_FRN_mean)/sum(Had_FRN_mean)

Had_prerec_weightmean <- sum(Had_PRN_mean*Had_PRW_mean)/sum(Had_PRN_mean)







################################################################################
# calculate weighted fishing mortality F for adults and prerecruits
################################################################################



#1- YELLOWTAIL FLOUNDER

YT_all_F <- read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\Stock Assessment Review Committee (SARC) Docs\\YT Fishing Mort Info.csv",header=TRUE,stringsAsFactors=FALSE)

YT_row_F <- nrow(YT_all_F) #total number of observations (top half for weight bottom half for corresponding numbers)

YT_PreRecAge <- 1 #age before they are recruited (1)
YT_AdultAge <-  2 #age when they become adults (2)

YT_F <- YT_all_F[1:(YT_row/2),] #weight by age
YT_N <- YT_all_F[(YT_row/2+1):YT_row,] #number by age

#Pre recruit weight. Add +1 bc col 1 has year
YT_PRF <- as.numeric(YT_F[,(YT_PreRecAge+1)])

#Pre recruit numbers. Add +1 bc col 1 has year
YT_PRN <- YT_N[,(YT_PreRecAge+1)]
#remove commas
YT_PRN <- as.numeric(gsub(",", "", YT_PRN))

#calculate mean 
YT_PRN_mean <- mean(YT_PRN)
YT_PRF_mean <- mean(YT_PRF)

#Fully recruited (ie adult) F. Add +1 bc col 1 has year
YT_FRF <- YT_F[,(YT_AdultAge+1):ncol(YT_F)]
#Pre recruit numbers. Add +1 bc col 1 has year
YT_FRN <- YT_N[,(YT_AdultAge+1):ncol(YT_F)]
#change each of above from characters to numeric
temp1 <- matrix(0,ncol=ncol(YT_FRF),nrow=nrow(YT_FRF))
temp2 <- matrix(0,ncol=ncol(YT_FRF),nrow=nrow(YT_FRF))
for(i in seq(ncol(YT_FRF))){
  
  temp1[,i] <- as.numeric(YT_FRF[,i])
  
  temp3 <- YT_FRN[,i]
  temp2[,i] <- as.numeric(gsub(",","",temp3))
  
}
YT_FRF <- temp1
YT_FRN <- temp2

#calculate mean for each year
YT_FRF_mean <- colMeans(YT_FRF)
YT_FRN_mean <- colMeans(YT_FRN)


#final adult weighted mean is sum(weight_i*x_i)/(sum(weights))
YT_adult_Fmean <- sum(YT_FRF_mean*YT_FRN_mean)/sum(YT_FRN_mean)

YT_prerec_Fmean <- sum(YT_PRN_mean*YT_PRF_mean)/sum(YT_PRN_mean)



#2- Cod

Cod_all_F <- read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\Stock Assessment Review Committee (SARC) Docs\\Cod Fishing Mort Info.csv",header=TRUE,stringsAsFactors=FALSE)

Cod_row_F <- nrow(Cod_all_F) #total number of observations (top half for weight bottom half for corresponding numbers)

Cod_PreRecAge <- 1 #age before they are recruited (1)
Cod_AdultAge <-  2 #age when they become adults (2)

Cod_F <- Cod_all_F[1:(Cod_row/2),] #weight by age
Cod_N <- Cod_all_F[(Cod_row/2+1):Cod_row,] #number by age

#Pre recruit weight. Add +1 bc col 1 has year
Cod_PRF <- as.numeric(Cod_F[,(Cod_PreRecAge+1)])

#Pre recruit numbers. Add +1 bc col 1 has year
Cod_PRN <- Cod_N[,(Cod_PreRecAge+1)]
#remove commas
Cod_PRN <- as.numeric(gsub(",", "", Cod_PRN))

#calculate mean 
Cod_PRN_mean <- mean(Cod_PRN)
Cod_PRF_mean <- mean(Cod_PRF)

#Fully recruited (ie adult) F. Add +1 bc col 1 has year
Cod_FRF <- Cod_F[,(Cod_AdultAge+1):ncol(Cod_F)]
#Pre recruit numbers. Add +1 bc col 1 has year
Cod_FRN <- Cod_N[,(Cod_AdultAge+1):ncol(Cod_F)]
#change each of above from characters to numeric
temp1 <- matrix(0,ncol=ncol(Cod_FRF),nrow=nrow(Cod_FRF))
temp2 <- matrix(0,ncol=ncol(Cod_FRF),nrow=nrow(Cod_FRF))
for(i in seq(ncol(Cod_FRF))){
  
  temp1[,i] <- as.numeric(Cod_FRF[,i])
  
  temp3 <- Cod_FRN[,i]
  temp2[,i] <- as.numeric(gsub(",","",temp3))
  
}
Cod_FRF <- temp1
Cod_FRN <- temp2

#calculate mean for each year
Cod_FRF_mean <- colMeans(Cod_FRF)
Cod_FRN_mean <- colMeans(Cod_FRN)


#final adult weighted mean is sum(weight_i*x_i)/(sum(weights))
Cod_adult_Fmean <- sum(Cod_FRF_mean*Cod_FRN_mean)/sum(Cod_FRN_mean)

Cod_prerec_Fmean <- sum(Cod_PRN_mean*Cod_PRF_mean)/sum(Cod_PRN_mean)





#3- Haddock was estimated from a plot
#Figure B116 shows FullF ranging from ~0.15-.75 since 2000. 
#I think this is after exponentiation so lets say e^F = .45 so that F = ln(.45) = -.7985







################################################################################
# calculate phi0 and define R0 for each species
################################################################################

phi0 <- function(M = NULL, Adult_weight = NULL, PreRecruit_weight = NULL){
  
  p0 <- exp(M)*(Adult_weight/PreRecruit_weight)
  return(p0)
}

YT_phi0 <- phi0(M = -1.478, Adult_weight = .39, PreRecruit_weight = .13)

Cod_phi0 <- phi0(M = -1.299, Adult_weight = 2.95, PreRecruit_weight = .39)

Had_phi0 <- phi0(M = -1.097, Adult_weight = 1.12, PreRecruit_weight = .19)


#define R0 for each species (see notes document for details on values)
#R0 = S0 / phi0

#SARC doc shows max yellowtail biomass values of 40,000-60,000 mt in early 70s through late 80s. Let S0 = 70000
YT_R0 <- 70000/YT_phi0

#Figure A.6.9 in SARC doc shows highest biomass of about 35,000 mt in the late 80s. Let S0 = 50,000 
Cod_R0 <- 150000/Cod_phi0

#Figure b147 in assessment doc shows total biomass has been highest in recent years around 250,000 with estimate catch of about 20,000. Let S0 = 270,000. 
Had_R0 <- 270000/Had_phi0





################################################################################
# estimate alpha and beta for beverton holt stock recruitment for each species
################################################################################

alpha <- function(h = NULL, R0 = NULL){
  
  a <-  (4 * h * R0) / (5 * h - 1)
  
  return(a)
}

beta <- function(h = NULL, R0 = NULL, phi0 = NULL){
  
  b <- (phi0 * R0 * (1 - h)) / (5 * h - 1)
  return(b)
}


#Yellowtail
YT_h <-.812

alpha(h= YT_h, R0 = YT_R0)
beta(h= YT_h, R0= YT_R0, phi0= YT_phi0)

#Cod
Cod_h<- .793
  
alpha(h= Cod_h, R0 = Cod_R0)
beta(h= Cod_h, R0= Cod_R0, phi0= Cod_phi0)

#Haddock
Had_h <- .657
  
alpha(h= Had_h, R0 = Had_R0)
beta(h= Had_h, R0= Had_R0 , phi0= Had_phi0)




#Plot BH parameters to see if they seem ok

#yellowtail biomass range
YT_range <- seq(0,90000)
YT_params <- list("a"=108582,"b"=4301)  #alph,beta

Cod_range <- seq(0,200000)
Cod_params <- list("a"=77766,"b"=10472)  #alph,beta


Had_range <- seq(0,300000)
Had_params <- list("a"=157783,"b"=40530)  #alph,beta


#change range, params and abline input
range <- YT_range
params_ <- YT_params
rec <- vector()
for(i in range){
  rec[i] <- Recr(model = "BH", params = params_, B = i, cv = 0)
}

plot(rec)

#add intersection line
abline(0,1/YT_phi0)














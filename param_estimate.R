#This script is used to fit recruitment parameters for desired scenario (increasing, decreasing, constant populations)



####################################################################################################################

#FIRST SETUP SIMULATION INPUTS




source("R/init_sim_Bens_nofish.R")
sim <- init_sim_Bens_nofish(nrows = 1, ncols = 1, n_years = 22,
                            n_tows_day = 1,n_days_wk_fished = 1, n_fleets = 1, n_vessels = 1, n_species = 1,   #RUN 1 SPECIES AT A TIME
                            move_freq = 1)

#load rcpp exports
Rcpp::sourceCpp(file= "src/Movement.cpp")
Rcpp::sourceCpp(file= "src/RcppExports.cpp")
Rcpp::compileAttributes() #this updates RcppExports.R file, which contains function definitions





####################################################################################################################

 source("R/init_pop_Bens_param_estimate.R")


#YELLOWTAIL, COD, HADDOCK

#YT Population input. RUN JUST ONE

#ORIGINAL YT
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = 3194), 
              start_cell = list("spp1" = c(46,112)),
              lambda = c("spp1" = .1), 
              rec_params = list("spp1" = c("model" = "BH", "a" = 30445, "b" = 4301, "cv" = 0)), 
              rec_wk = list("spp1" = 9:12),
              spwn_wk = list("spp1" = 9:12), 
              M = c("spp1" = .2064+.358), 
              K = c("spp1" = -log((1+exp(-.2295))^(1/52)-1)),
              Weight_PreRecruit = (c("spp1" = .13/.39)),
              Weight_Adult = (c("spp1" = 1)))

#DECREASING YT
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = 50000), #DIFFERENT
                                    start_cell = list("spp1" = c(46,112)),
                                    lambda = c("spp1" = .1), 
                                    rec_params = list("spp1" = c("model" = "BH", "a" = 30445, "b" = 4301, "cv" = 0)), 
                                    rec_wk = list("spp1" = 9:12),
                                    spwn_wk = list("spp1" = 9:12), 
                                    M = c("spp1" = .2064+.358+.2), #DIFFERENT
                                    K = c("spp1" = -log((1+exp(-.2295))^(1/52)-1)),
                                    Weight_PreRecruit = (c("spp1" = .13/.39)),
                                    Weight_Adult = (c("spp1" = 1)))

#Cod population input. RUN JUST ONE

#Original Cod
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = (13000+30000)/2), 
                                    start_cell = list("spp1"  = c(30,134)),
                     lambda = c("spp1" = .1), 
                     rec_params = list("spp1" = c("model" = "BH", "a" = 27868 , "b" = 10472, "cv" = 0)), 
                     rec_wk = list("spp1" = 8:13),
                     spwn_wk = list("spp1" = 8:13), 
                     M = c("spp1" = .2728+.511), 
                     K = c("spp1" = -log((1+exp(-.16))^(1/52)-1)),
                     Weight_PreRecruit = (c("spp1" = .39/2.95)),
                     Weight_Adult = (c("spp1" = 1)))

#decreasing Cod
#Cod population input. RUN JUST ONE
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = 40000), #DIFFERENT 
                                    start_cell = list("spp1"  = c(30,134)),
                                    lambda = c("spp1" = .1), 
                                    rec_params = list("spp1" = c("model" = "BH", "a" = 27868 , "b" = 10472, "cv" = 0)), 
                                    rec_wk = list("spp1" = 8:13),
                                    spwn_wk = list("spp1" = 8:13), 
                                    M = c("spp1" = .2728+.35), #DIFFERENT
                                    K = c("spp1" = -log((1+exp(-.16))^(1/52)-1)),
                                    Weight_PreRecruit = (c("spp1" = .39/2.95)),
                                    Weight_Adult = (c("spp1" = 1)))



#haddock population input. RUN JUST ONE

#Original Haddock
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = 150000*1.2), 
                     start_cell = list("spp1" = c(24,91)),
                     lambda = c("spp1" = .1), 
                     rec_params = list("spp1" = c("model" = "BH", "a" = 73568, "b" = 40530, "cv" = 0)), 
                     rec_wk = list("spp1" = 11:14),
                     spwn_wk = list("spp1" = 11:14  ), 
                     M = c("spp1" = .334+.45), 
                     K = c("spp1" = -log((1+exp(-.2465))^(1/52)-1)),
                     Weight_PreRecruit = (c("spp1" = .19/1.12)),
                     Weight_Adult = (c("spp1" = 1)))

#Decreasing Haddock
Pop <- init_pop_Bens_param_estimate(sim_init = sim, Bio = c("spp1" = 150000*1.2), 
                                    start_cell = list("spp1" = c(24,91)),
                                    lambda = c("spp1" = .1), 
                                    rec_params = list("spp1" = c("model" = "BH", "a" = 73568, "b" = 40530, "cv" = 0)), 
                                    rec_wk = list("spp1" = 11:14),
                                    spwn_wk = list("spp1" = 11:14  ), 
                                    M = c("spp1" = .334), #DIFFERENT 
                                    K = c("spp1" = -log((1+exp(-.2465))^(1/52)-1)),
                                    Weight_PreRecruit = (c("spp1" = .19/1.12)),
                                    Weight_Adult = (c("spp1" = 1)))

####################################################################################################################




####################################################################################################################
#set temperature preferences CHOOSE JUST ONE 
#The following assumes moveCov has been created and already has an empty spp_tol sublist

#YT TEMPERATURE PREFERENCES
moveCov <- list()
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 9, "va" = 4))    

#Cod TEMPERATURE PREFERENCES
moveCov <- list()
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.75, "va" = 4.25) )   

#Haddock TEMPERATURE PREFERENCES
moveCov <- list()
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 9, "va" = 4) )

####################################################################################################################


####################################################################################################################

library(MixFishSim)

#no fishing
fleets <- init_fleet(sim_init = sim, VPT = list("spp1" = 0), #VPT = value per ton
                     Qs = list("fleet 1" = c("spp1" = 0)   #Q = catchability
                     ),
                     fuelC = list("fleet1" = 3),
                     step_params = list("fleet 1" = c("rate" = 3, "B1" = 1, "B2" = 2, "B3" = 3)
                     ),				
                     past_knowledge = FALSE,  #dont use past knowledge
                     past_year_month = TRUE,
                     past_trip = TRUE,
                     threshold = 0.7
)


####################################################################################################################




## Run simulation

 
  
  
  #to source a new go_fish where I edited to skips most things:
  #1: load file
  source("R/go_fish_Bens.R") #my edited version that skips most things
  #2: allow the function to call other hidden functions from mixfishsim 
  environment(go_fish_Bens) <- asNamespace('MixFishSim')
  #3: replace go_fish with go_fish_Bens in the MixFishSim package
  assignInNamespace("go_fish", go_fish_Bens, ns = "MixFishSim")

  
  
  
  #CREATE DESIRED HYPOTHETICAL DATA TO FIT TO. 
  #individual functions obtained by fitting to start,end population values using matlabs curveFitter toolbox
  
  #for example, for YT we want to start at 50000 population and end around 5000 over 22 years
  #i therefore input [0,21] for the x data and [50000,5000] for the y data to obtain the function
  #f(x)=a*exp(bx) for a = 5e4 and b = -0.1096
  
  
 exp_func <- function(a,b,t){return(a*exp(b*t))}
  
 #CHOOSE JUST 1 
  
  #YT 
  YT_dec_data <- exp_func(50000,-.1096,seq(0,21)) #decreasing population
  H_DATA <- YT_dec_data
  #plot(YT_dec_data)
  start_params <- c( 30445,  4301)
  
  #Cod 
  Cod_dec_data <- exp_func(40000, -.09902, seq(0,21)) #decreasing population
  H_DATA <- Cod_dec_data
  start_params <- c( 27868 , 10472)
  
  #Haddock
  Had_dec_data <- exp_func(180000, -0.05231, seq(0,21)) #decreasing population
  H_DATA <- Had_dec_data  
  start_params <- c(73568, 40530)
 
  source("R/run_sim_param_estimate.R")
  
  #use optim function to find optimal value
  
  #to estimate parameters then return the error, if running output return normal 
  run_type <- "estimate"
  
params <-  optim(par = start_params,
  fn = run_sim_param_estimate,
  lower = c(0,0))


####################################################################################################################





## Summary plots

#first run simulation with estimated values to record output
run_type <- "test"
res<-run_sim_param_estimate(x=params$par)


## Biological
source("R/plot_pop_summary.R")
p1 <- plot_pop_summary(results = res,
                       timestep = "annual",
                       save = FALSE, 
                       save.location = NULL
)

plot_pop_summary(results = res, timestep = "daily", save = FALSE, save.location = "C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/testfolder")



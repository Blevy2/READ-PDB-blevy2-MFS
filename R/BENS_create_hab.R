#' @title Create habitat distribution fields
#' 
#' @description \code{create_hab} parametrises and returns the
#' spatial fields used for the distribution of suitable habitat for the
#' populations in the simulation. 
#'
#' The spatial fields are generated using
#' \code{\link[RandomFields]{RFsimulate}} function from the \emph{RandomFields}
#' package.
#'
#' @param sim is the parameter settings for the simulation, made by
#' \code{init_sim} function.
#' @param spp.ctrl List of controls to generate suitable habitat for each
#' species. Must be of the form spp.ctrl = list(spp.1 = c(var = 20, ...),
#' spp.2 = c(var = 10, ..),..) and contain the following:
#' \itemize{
#'	\item \strong{nu} (\emph{>=0})
#' 	\item \strong{var} (\emph{>=0}) Controls the range in a matern covariance
#' 	\item \strong{scale} (\emph{>=0})
#'	\item \strong{Aniso} (\emph{matrix, dim = c(2,2)})
#'}
#' @param plot.dist Boolean, whether to plot the distributions to file
#' @param plot.file path to save the plots of the species distributions

#' @return Silently returns a list of spatial distributions of suitable habitat
#' with first level of the list being the population (1 -> n.spp). If
#' \code{plot.dist = TRUE} it produces an image of the spatial distributions at
#' each time step for each of the populations saved to the working directory
#' (unless specified otherwise in \code{plot.file})

#' @examples
#' hab <- create_hab(sim.init = sim.init, spp.ctrl = list(
#'	      'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
#'	      matrix(nc=2, c(1.5, 3, -3, 4)))), spawn_areas = list("spp1" =
#'	      list("area1" = c(2,4,6,8))), list("spp2" = list("area1" =
#'	      c(0,10,23,35))), spwn_mult = 10, plot.dist = TRUE, plot.file =   getwd())

#' @export

BENS_create_hab <- function (sim_init = sim, stratas = NULL, strata = NULL, seed = 123, spp.ctrl = NULL, spawn_areas = NULL, spwn_mult = 10, plot.dist = FALSE, plot.file = getwd()) {

  
print(spwn_mult)
  
	# Extract indices
	idx <- sim_init[["idx"]]
	n.spp <- idx[["n.spp"]]
	ncols <- idx[["ncols"]]
	nrows <- idx[["nrows"]]

	RandomFields::RFoptions(spConform = FALSE) # faster and only returns the matrix of values
	set.seed(seed)

	# Checks
	if(is.null(n.spp)) stop('must specify the number of species to simulate')
	if(is.null(spp.ctrl)) stop('must specify the control parameters for the species simulations')

hab <- 	lapply(seq_len(n.spp), function(i) {
	par  <- spp.ctrl[[paste0('spp.',i)]]

	# Check
	for (params in c('nu', 'var', 'scale', 'Aniso')) {
	if(!(params %in% names(par))) stop(paste0('spp.',i,' does not contain ',params))
	}

	if(!is.matrix(par$Aniso)) stop(paste0('Aniso must be a matrix of dim c(2,2)'))

	# Create the sim object
	hab_mod <- RandomFields::RMmatern(nu = par$nu, var = par$var, scale =
			    par$scale, Aniso = par$Aniso)
	assign(paste0('hab.','spp.',i), RandomFields::RFsimulate(model = hab_mod, x = seq(nrows), y = seq(ncols)))


	# Normalise from 0 to 1 per time period
	x<- get(paste0('hab.','spp.',i))
	x[x < 0] <- 0 # Any negative to zeros
	assign(paste0('spp',i), x / sum(x)) # sum to 1
	return(get(paste0('spp',i)))

	})

	names(hab) <- paste0("spp", seq_len(n.spp))

	# Plot
	if(spp.ctrl$plot.dist == TRUE ) {
	  print("plottingggggg hab")
	png(filename = paste0(spp.ctrl$plot.file,'/','habitat','.png'), width = 800, height = 800)
	plot_habitat(hab)
	dev.off()
	}

	#change line 87 to plot_habitat(hab[["hab"]])?

# Now the spawning habitat
# Define the spawning habitat preferences

	if(!is.null(spawn_areas)) {

	spwn_hab <- lapply(paste0("spp",seq_len(n.spp)), function(x) {
		create_spawn_hab(hab = hab[[x]], spwnareas = spawn_areas[[x]], mult = spwn_mult)
	})
	names(spwn_hab) <- paste0("spp", seq_len(n.spp))

# create a matrix of 0.5s with right dims
spwn <- matrix(rep(0.5, nrows * ncols), nc = ncols)

	spwn_loc <- lapply(names(spwn_hab), function(x) {
	res <- define_spawn(coord = spawn_areas[[x]], spwn = spwn, mult = 2)
	res[res==0.5] <- 0 # zeros for non-spawning areas
	return(res)
	
	})

	names(spwn_loc) <- paste0("spp", seq_len(n.spp))

	}
	
	
	
	# Now the strata (altered above spawning habitat)
	
	#spawn version of above, incase it is needed. I dont think it is at the moment
	#  spwn_hab <- lapply(paste0("strata",1:length(hab$strata)), function(x) {
	

	
	if(!is.null(strata)) {
	 
	  # create a matrix of 0s with right dims
	  stratas <- matrix(rep(0, nrows * ncols), nc = ncols)
	  
    for(k in seq(length(strata))){

        #define coordinate boundaries
      
      
      x1 <- strata[[k]][1]; x2 <- strata[[k]][2]
      y1 <- strata[[k]][3]; y2 <- strata[[k]][4]

  
      stratas[x1:x2, y1:y2] <- k
	  

	  
	 # names(stratas) <- paste0("Strata", k)
	  
	}
	  
	}
	
	
	

	if(is.null(spawn_areas)) {
	spwn_hab <- NULL
	spwn_loc <- NULL
	}

	# Plot
	if(plot.dist == TRUE & !is.null(spawn_areas)) {
	  print("plottingggggg spawn hab")
	png(filename = paste0(spawn_areas$plot.file,'/','habitat_spwn','.png'), width = 800, height = 800)
	plot_habitat(spwn_hab)
	dev.off()
	}

	habitat_lst <- list(hab = hab, spwn_hab = spwn_hab, spwn_loc = spwn_loc, spawn_areas = spawn_areas, strata = strata, stratas = stratas)
	# Return the list invisibly
	return(habitat_lst)
		
}

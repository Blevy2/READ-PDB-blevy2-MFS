# test_haddock.R

# set working directory to source file location to begin

library(TMB)
library(VAST)

# use ?load_example and look in Details to see currently available data sets
example = load_example( data_set="GB_spring_haddock" )

settings = make_settings(n_x=100, Region=example$Region, purpose="index", 
                         strata.limits=example$strata.limits, bias.correct=FALSE )

fit = fit_model( "settings"=settings, 
                 "Lat_i"=example$sampling_data[,'Lat'], 
                 "Lon_i"=example$sampling_data[,'Lon'], 
                 "t_i"=example$sampling_data[,'Year'], 
                 "c_i"=rep(0,nrow(example$sampling_data)), 
                 "b_i"=example$sampling_data[,'Catch_KG'], 
                 "a_i"=example$sampling_data[,'AreaSwept_km2'], 
                 "v_i"=example$sampling_data[,'Vessel'] )

names(fit)

plot( fit )

# get error message the figure margins too large if don't do this
windows(record=TRUE) 
# to see the mesh
plot(fit$spatial_list$MeshList$anisotropic_mesh)

?plot_results

plot_results(fit, settings, plot_set = 1)
plot_results(fit, settings, plot_set = 2)
plot_results(fit, settings, plot_set = 6)

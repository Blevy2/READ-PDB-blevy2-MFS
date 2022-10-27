#FOR VISUALIZING COVARIATE RESPONSES


#####################
# Effects package
#####################
library(VAST)
library(effects)  # Used to visualize covariate effects

fittt <- fit_spring


#CHANGE X1 OR X2 FORMULA IN FITTT IF IT INCLUDES AS.NUMERIC()
fittt$X1_formula <- ~1
fittt$X2_formula <- ~poly(Temp, degree = 2)


# Must add data-frames to global environment (hope to fix in future)
covariate_data_full = fittt$effects$covariate_data_full
catchability_data_full = fittt$effects$catchability_data_full

# Plot 1st linear predictor, but could use `transformation` to apply link function
pred = Effect.fit_model( fittt,
                         focal.predictors = c("Temp"),
                         which_formula = "X1",
                         xlevels = 100,
                         transformation = list(link=identity, inverse=identity) )
plot(pred)


pred2 = Effect.fit_model( fittt,
                         focal.predictors = c("Temp"),
                         which_formula = "X2",
                         xlevels = 100,
                         transformation = list(link=identity, inverse=identity) )
plot(pred2)

pred3 = Effect.fit_model( fittt,
                          focal.predictors = c("Habitat"),
                          which_formula = "X2",
                          xlevels = 100,
                          transformation = list(link=identity, inverse=identity) )
plot(pred3)


#####################
# pdp package
#####################

library(pdp)

# Make function to interface with pdp
pred.fun = function( object, newdata ){
  predict( x=object,
           Lat_i = object$data_frame$Lat_i,
           Lon_i = object$data_frame$Lon_i,
           t_i = object$data_frame$t_i,
           a_i = object$data_frame$a_i,
           what = "P1_iz",
           new_covariate_data = newdata,
           do_checks = FALSE )
}

# Run partial
Partial = partial( object = fittt,
                   pred.var = "Temp",
                   pred.fun = pred.fun,
                   train = fittt$covariate_data )

# Make plot using ggplot2
library(ggplot2)
autoplot(Partial)

#using FIshLife to extract values for GB species

library(FishLife)
library(rfishbase)
#library(fishbase)

#Yellowtail flounder, Cod, Haddock


################################################################
#FIRST SET NAMES BEFORE RUNNING BELOW
################################################################

#yellowtail setup

Tax_class <- "Actinopterygii"

Order <- "Pleuronectiformes"

Family <- "Pleuronectidae"

Genus <- "Limanda"

Species <- "ferruginea"


# Cod setup

Tax_class <- "Actinopterygii"

Order <- "Gadiformes"

Family <- "Gadidae" 

Genus <- "Gadus"

Species <- "morhua"


# Haddock setup

Tax_class <- "Actinopterygii"

Order <- "Gadiformes"

Family <- "Gadidae" 

Genus <- "Melanogrammus"

Species <- "aeglefinus"



################################################################
#SECOND RUN HERE TO PRINT OUT PARAMETERS
################################################################

Search_species(Class = Tax_class, Order = Order,
               Family = Family, Genus = Genus,
               Species = Species, add_ancestors = TRUE,
               Database = FishLife::FishBase_and_RAM)
              # ParentChild_gz = Database$ParentChild_gz)



params = matrix( c("K","M", "G","ln_MASPS"), ncol=2, byrow=TRUE)


#find values for class, order, family, genus AND species
#species value will be in the [[1]] locations
Predict_YT2 <- Plot_taxa(Search_species(Class = Tax_class, Order = Order,
                         Family = Family, Genus = Genus,
                         Species = Species, add_ancestors = TRUE,
                         Database = FishLife::FishBase_and_RAM)$match_taxonomy, mfrow=c(1,2), params=params)



#find values for JUST species
Predict_YT <- Plot_taxa(Search_species(Class = Tax_class, Order = Order,
                                       Family = Family, Genus = Genus,
                                       Species = Species, add_ancestors = FALSE,
                                       Database = FishLife::FishBase_and_RAM)$match_taxonomy, mfrow=c(1,2), params=params)
#from above,
#M = -1.4782162  -> exp(-1.4782162) = 0.2280441
#K = -1.4171570  -> exp(-1.4171570) = 0.2424022



Predict_YT[[1]]


#We then show updated values for the predictive mean using Predict[[1]]$Mean_pred...
#THIS WILL PRINT OUT WHAT WE WANT
knitr::kable(Predict_YT[[1]]$Mean_pred, digits=3)

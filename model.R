##########################################################################################
#                                                                                        #            
#  Code for Chapter 10 (Price Model)                                                     #
#   of Real Estate Analysis in the Information Age                                       #   
#                                                                                        #                  
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

## Load Libraries

library(sf)
library(sp)
library(tidyverse)
library(RSQLite)
library(RODBC)

## Set data and code directory

data.dir <- 'c:/temp/'
code.dir <- 'c:/code/REAIA_book/'

## Load custom source files

source(paste0(code.dir, 'custom_functions.R'))  

## Set the database path and name  

sales.db <- file.path(data.dir, 'assessorData.db')

### Load data ----------------------------------------------------------------------------  

# Read in Sales File
sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
sales.data <- dbReadTable(sales.conn, 'cleanSales')

sales.data$sales.date <- as.Date(sales.data$sales.date)


# Basic model

base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + tot.sf + 
                bldg.grade + baths + eff.age + traffic.noise + deck.sf + gar.att.sf + 
                view.best + is.waterfront + bsmt.sf + sales.date + topo + restr.szshp +
                condition + gar.bsmt.sf + beds,
              data=sales.data)

library(MASS)

step.lm <- stepAIC(base.lm, direction="both")

library(BMA)

bma.lm <- bicreg(y=log(sales.data$sale.price),
                 x=sales.data[,c('present.use', 'lot.size', 'tot.sf', 
                                 'bldg.grade', 'baths', 'eff.age', 'traffic.noise', 
                                 'deck.sf', 'gar.att.sf', 'view.best', 'is.waterfront',
                                 'bsmt.sf', 'sales.date', 'topo', 'restr.szshp',
                                 'condition', 'gar.bsmt.sf' , 'beds')])

  # Stepwise
  # BMA

sales.data$condition[sales.data$condition == 1] <- 2
view.combine <- which(sales.data$view.best %in% c('view.other', 'view.smwater',
                                                  'view.city'))
sales.data$view.best[view.combine] <- 'view.other'

bma.lm <- bicreg(y=log(sales.data$sale.price),
                 x=sales.data[,c('present.use', 'lot.size', 'tot.sf', 
                                 'bldg.grade', 'baths', 'eff.age', 'traffic.noise', 
                                 'deck.sf', 'gar.att.sf', 'view.best', 'is.waterfront',
                                 'bsmt.sf', 'sales.date', 'condition', 'beds')])


base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + tot.sf + 
                bldg.grade + baths + eff.age + traffic.noise + deck.sf + gar.att.sf + 
                view.best + is.waterfront + bsmt.sf + sales.date + condition + beds,
              data=sales.data)

# Diagnostics

# Multicollinearity
library(car)
vif(base.lm)
sqrt(vif(base.lm)) > 2

# Heteroskedasticity
# Nonlinearities
crPlots(base.lm)

# log TotSF, lotSF, factorize bldggrade, chop up sales date

ncvTest(base.lm)
spreadLevelPlot(base.lm)

# New model
sales.data$month <- as.numeric(as.factor(substr(sales.data$sales.date, 1, 7)))
base.adj.lm <- lm(log(sale.price) ~ as.factor(present.use) + log(lot.size) + log(tot.sf) + 
                   as.factor(bldg.grade) + baths + eff.age + traffic.noise + 
                    deck.sf + gar.att.sf+ 
                   view.best + is.waterfront + bsmt.sf + condition + beds + 
                    as.factor(month),
                  data=sales.data)

ncvTest(base.adj.lm)
spreadLevelPlot(base.adj.lm)
crPlots(base.adj.lm)

  # Spatial Autocorrelation


source('c:/code/tools/spatialTools/spatEconTools.R')
sp.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                        sales.data$latitude),
                                  sales.data)

 
# # Develop a spatial weights matrix
 
  library(spdep)  
  dwf <- function(x) {1 / ((x + .00025) ^ 2)}

  nbList <- knn2nb(knearneigh(sp.data, 10))

## Create Distances
  nbDists <- nbdists(nbList, sp.data)    

 ## Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                        , style="W",zero.policy=T))
  
  mi.test <- moran.test(base.adj.lm$resid, swm, zero.policy=TRUE)
 
 # Determine form of spatial dependence
  lm.test <- lm.LMtests(base.adj.lm, swm,
                       test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
 
 # Specify spatial error Model
   mod.se <- errorsarlm(as.formula(base.adj.lm),data=sp.data,
                        swm, method="spam", zero.policy=TRUE)
# 
   mi.test.se <- moran.test(mod.se$resid, swm, zero.policy=TRUE)
   
   
   qqq <- base.adj.lm
   qqq$residuals <- mod.se$resid
   crPlots(qqq)

# Spatial Models

  # LM Test
  # Error or lag model
  # GWR Model as a test

# Sensitivity Test

  # Specification
  # Discordant Values
  # Sample (cross-validation)







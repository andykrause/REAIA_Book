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

# Diagnostics

  # Multicolliearity
  # Heteroskedasticity
  # Spatial Autocorrelation

# Spatial Models

  # LM Test
  # Error or lag model
  # GWR Model as a test

# Sensitivity Test

  # Specification
  # Discordant Values
  # Sample (cross-validation)







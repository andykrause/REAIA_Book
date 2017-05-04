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







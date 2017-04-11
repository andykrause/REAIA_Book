##########################################################################################
#                                                                                        #            
#  Code for Chapter 8 (Explore) and 9 (Clean)                                            #
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
  code.dir <- 'c:/dropbox/research/bigdatabook/code/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  sales.db <- file.path(data.dir, 'assessorData.db')

### Load data ----------------------------------------------------------------------------  
  
  # Read in Sales File
  sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
  sales.data <- dbReadTable(sales.conn, 'prepSales')

  # Load City Police Beats Data
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))

### Univariate Exploration ---------------------------------------------------------------
  
  
  
  
  
  


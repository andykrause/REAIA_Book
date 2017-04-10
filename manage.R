##########################################################################################
#                                                                                        #            
#  Code for Chapter 5 (Manage) of Real Estate Analysis in the Information Age            #   
#                                                                                        #                  
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(RSQLite)
  library(RODBC)
  library(tidyverse)
  library(maptools)
  library(sf)
  library(sp)

 ## Set data and code directory

  data.dir <- 'c:/temp/' 
  code.dir <- 'c:/code/REAIA_Book/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  sales.db <- file.path(data.dir, 'assessorData.db')

### Arrange assessor data into single db file --------------------------------------------

 ## Convert CSVs to SQLite

 if (!file.exists(sales.db)){
  
   convertCSVtoSQLite(dataPathCurrent=file.path(data.dir, 'assessor'),
                      dataPathNew=data.dir,
                      newFileName='assessorData.db',
                      fileNames=c('EXTR_RPSale.csv',
                                  'EXTR_Parcel.csv',
                                  'EXTR_Resbldg.csv'),
                      tableNames=c('Sales',
                                     'Parcel',
                                     'ResBldg'),
                      overWrite=TRUE,
                      verbose=TRUE,
                      writeRowNames=FALSE)
  }

## Fix Unique IDS for the Assessor Data --------------------------------------------------
  
 ## Open up connection to database
  
  sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
  
 ## Sales
  
  # Read in data

  sales.data <- dbReadTable(sales.conn, 'Sales')

  # Make new UID from ExciseTaxNbr
  sales.data <- dplyr::mutate(sales.data, UID=ExciseTaxNbr)
  
  # Place Excise Tax Number on far left
  sales.data <- dplyr::select(sales.data, UID, everything())

  # Order by that field
  sales.data <- dplyr::arrange(sales.data, UID)

  # Create fully unique field
  
  # Get a data.frame of those UID that are duplicated
  uid.mult <- sales.data %>% 
    dplyr::group_by(UID) %>%
      dplyr::summarize(rec.count=n()) %>%
       dplyr::filter(rec.count > 1)
  
  # Develop a list full of proper suffixes
  uid.suffix <- lapply(split(uid.mult, uid.mult$UID), 
                       function(x) 1:(x$rec.count))
  names(uid.suffix) <- NULL
  
  # Apply suffix to sales.data
  sales.data$uid.suffix <- 0
  sales.data$uid.suffix[sales.data$UID %in% uid.mult$UID] <- unlist(uid.suffix)
  
  # Concatenate to make an unique ID
  sales.data$UID <- ifelse(sales.data$uid.suffix==0,
                           sales.data$UID,
                           paste0(sales.data$UID, '..', sales.data$uid.suffix))
  
  # Remove uid.suffix field
  sales.data$uid.suffix <- NULL

  # Write out
  dbRemoveTable(sales.conn, 'Sales')
  dbWriteTable(sales.conn, 'Sales', sales.data, row.names=FALSE)
  
 ## Parcel Data
  
  # Read in data
  parcel.data <- dbReadTable(sales.conn, 'Parcel')
  
  # Add unique pinx field
  parcel.data <- buildPinx(parcel.data)
  
  # Write out
  dbRemoveTable(sales.conn, 'Parcel')
  dbWriteTable(sales.conn, 'Parcel', parcel.data, row.names=FALSE)
  
## ResBldg Data
  
  # Read in data
  resbldg.data <- dbReadTable(sales.conn, 'ResBldg')
  
  # Add unique pinx field
  resbldg.data <- buildPinx(resbldg.data)
  
  # Add the bldgnbr to complete unique id
  resbldg.data$pinx <- paste0(resbldg.data$pinx, '.', resbldg.data$BldgNbr)
  names(resbldg.data)[1] <- 'BldgID'
  
  # Write out
  dbRemoveTable(sales.conn, 'ResBldg')
  dbWriteTable(sales.conn, 'ResBldg', resbldg.data, row.names=FALSE)
  
 ## Clean up
  
  rm(resbldg.data); rm(parcel.data); rm(sales.data)
  gc()
  
 ## Close connection to database
  
  dbDisconnect(sales.conn)
  
### Convert the Parcel Cadastral data into R objects for faster loading, etc. later ------
  
  ## Convert the parcel file to centroids
  
  # Load in parcel file
  parcels <- st_read(file.path(data.dir, 'geographic/parcel/parcel.shp'), quiet=TRUE)

  # Convert to appropriate CRS
  parcels <- st_transform(parcels, 4326)
  
  # Extract centroid Lat longs
  parcel.centroids <- st_centroid(parcels)
  #parcel.centroids <- st_sf(parcels[ ,c(1:3)], parcel.centroids$geometry)
  #names(parcel.centroids)[4] <- 'centroid'
  parcel.centroids$Shape_area <- NULL
  parcel.centroids$Shape_len <- NULL
  
  # Save as an R object for loading later
  save(parcel.centroids, file= file.path(data.dir, 'geographic/parcelcentroids.Rdata'))

### Convert the Beats data into an R object ----------------------------------------------  
  
  # Read in the Police Beats Data
  beats <- st_read(file.path(data.dir, 'beats/SPD_BEATS_WGS84.shp'), quiet=TRUE)
  
  # Transform the Coordinate Reference System  
  beats <- st_transform(beats, 4326)
  
  # Save as an R object for loading later
  save(beats, file= file.path(data.dir, 'geographic/beats.Rdata'))
  
##########################################################################################
##########################################################################################
  
  
  
  
  
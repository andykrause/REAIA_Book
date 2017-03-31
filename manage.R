
### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(RSQLite)
  library(RODBC)
  library(maptools)
  library(sf)
  library(sp)

 ## Set data and code directory

  data.dir <- 'c:/temp/' #data.dir <- 'c:/dropbox/research/bigdatabook/data/'
  code.dir <- 'c:/dropbox/research/bigdatabook/code/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  sales.db <- file.path(data.dir, 'assessorData.db')

### Arrange assessor data into single db file --------------------------------------------

 ## Convert CSVs to SQLite

 if(!file.exists(sales.db)){
  
   convertCSVtoSQLite(dataPathCurrent=file.path(data.dir, 'assessor'),
                      dataPathNew = data.dir,
                      newFileName = 'assessorData.db',
                      fileNames=c('EXTR_RPSale.csv',
                                  'EXTR_Parcel.csv',
                                  'EXTR_Resbldg.csv'),
                      tableNames = c('Sales',
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
  
  # Make RecordingNbr the Unique ID
  sales.data$RecID <- sales.data$RecordingNbr
  
  # Place RecID first
  s.names <- names(sales.data)
  s.names <- s.names[!s.names %in% c('RecID', 'RecordingNbr')]
  sales.data <- sales.data[ ,c('RecID', s.names)]
  
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
  parcel.centroids <- st_sf(parcels[ ,c(1:3)], parcel.centroids)
  names(parcel.centroids)[4] <- 'centroid'
  
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
  
  
  
  
  

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(sf)
  library(stringr)
  library(sp)
  library(plyr)
  library(RSQLite)
  library(RODBC)
  
 ## Set data and code directory
 
  #data.dir <- 'c:/dropbox/research/bigdatabook/data/'
  data.dir <- 'c:/temp/'
  code.dir <- 'c:/dropbox/research/bigdatabook/code/'
  
 ## Load custom source files
  
  source(paste0(code.dir, 'custom_functions.R'))  
  
 ## Set the database path and name  
  
  sales.db <- file.path(data.dir, 'assessorData.db')
  
### Integrate assessor data into single db file ------------------------------------------

 ## Convert CSVs to SQLite
  
  if(!file.exists(sales.db)){
  
    convertCSVtoSQLite(dataPathCurrent=file.path(data.dir, 'assessor'),
                     dataPathNew = data.dir,
                     newFileName = 'assessorData.db',
                     fileNames=c('EXTR_RPSale.csv',
                                 'EXTR_Parcel.csv',
                                 'EXTR_Resbldg.csv'),
                     tableNames = c('AllSales',
                                    'Parcel',
                                    'ResBldg'),
                     overWrite=TRUE,
                     verbose=TRUE,
                     writeRowNames=FALSE)
  }
  
### Initial clean of sales to eliminate non-relevant observations ------------------------ 

  # Read in Sales File
  sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
  raw.sales <- dbReadTable(sales.conn, 'AllSales')
  
  # Base clean
  clean.sales <- raw.sales[raw.sales$Major > 0, ]
  clean.sales <- clean.sales[clean.sales$SalePrice > 0, ]
  
  # Build sales data
  clean.sales$docDate <- paste(substr(clean.sales$DocumentDate, 4, 5)
                              ,substr(clean.sales$DocumentDate, 1, 2),
                              substr(clean.sales$DocumentDate, 7, 10), sep="")
  clean.sales$salesDate <- as.POSIXct(strptime(clean.sales$docDate, "%d%m%Y"))
  clean.sales$salesYear <- as.numeric(format(clean.sales$salesDate, "%Y"))
  clean.sales <- clean.sales[!is.na(clean.sales$salesDate), ]
  
  # Eliminate Transactions prior to Sales Year Limit
  clean.sales <- clean.sales[clean.sales$salesYear >= 2011 & 
                             clean.sales$salesYear <= 2016, ]
  
  # Add PINX
  clean.sales <- buildPinx(clean.sales)
  
  # Add trans count and limit by paramter
  clean.sales <- buildTransCount(clean.sales, transLimit=5)
  
  # Add MultiParcel sale designation
  clean.sales <- idDup(clean.sales, 'ExciseTaxNbr', newField = 'multiParcel',
                      iddType='labelNonUnique', binNonUq=TRUE)
  
  # Remove those with multiparcel
  clean.sales <- clean.sales[clean.sales$multiParcel == 0, ]
  
  # Add unique IDs
  clean.sales <- buildSaleUIDs(clean.sales)
  
  # Trim sales by Insturment, reason and warning
  # Fix the "Warning" Field.  Add a leading/trailing space for the grep()
  clean.sales$SaleWarning <- paste(" ", clean.sales$SaleWarning, " ", sep="")
  
  trim.list <- list(SaleReason=2:19,  
                    SaleInstrument=c(0, 1, 4:28),
                    SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 25, 27,
                                              31:33, 37, 39, 43, 46, 48, 49,
                                              50:53, 59, 61, 63, 64, 66), " "))
  
  for(tL in 1:length(trim.list)){
    clean.sales <- trimByField(clean.sales, names(trim.list)[tL],
                             trimList = unlist(trim.list[tL]))
  }
  
  # Write out
  tExists <- dbExistsTable(sales.conn, 'trimmedSales')
  
  if(tExists) {
    dbRemoveTable(sales.conn, 'trimmedSales')
  }
  dbWriteTable(sales.conn, 'trimmedSales', clean.sales, row.names=FALSE)
  
### Label Sales by Recond and use --------------------------------------------------------
  
  # Read in Data
  readData(dbName=sales.db,
           dataYear=2016,
           verbose=TRUE)
    
  # Add Present Uses
  trim.sales$present.use <- parcel.data$PresentUse[match(trim.sales$pinx,
                                                  parcel.data$pinx)]

  # Add the record type
  trim.sales$res.record <- resbldg.data$BldgNbr[match(trim.sales$pinx,
                                              resbldg.data$pinx)]
  
  trim.sales$res.record <- ifelse(is.na(trim.sales$res.record), 0, trim.sales$res.record)
  
  # Remove those with non-residential record type or with more than one dwelling on it  
  trim.sales <- trim.sales[trim.sales$res.record == 1, ]
    
  # Remove those not with SFR or Townhome use category
  trim.sales <- trim.sales[trim.sales$present.use == 2 |
                           trim.sales$present.use == 29, ]
  
  # Write out
  tExists <- dbExistsTable(sales.conn, 'labeledSales')
  
  if(tExists) {
    dbRemoveTable(sales.conn, 'labeledSales')
  }
  
  dbWriteTable(sales.conn, 'labeledSales', trim.sales, row.names=FALSE)
  
  # Clean up
  for(delX in c('parcel.data','resbldg.data')){
    rm(list=ls(pattern=glob2rx(paste0(delX,"*"))))
  }
  gc()  
 
### Integrate Sales data with assessor data ----------------------------------------------  
  
 ## Read in full data  
  
  # Parcel Data
  parcel.data <- buildPinx(dbReadTable(sales.conn, 'parcel'))
 
  # Res Building data
  resbldg.data <- buildPinx(dbReadTable(sales.conn, 'resbldg'))
  
 ## Clean up assessor data
  
  # Limit columns in parcel data
  
  parcel.data <- parcel.data[,c('pinx', 'Area', 'SubArea', 'CurrentZoning',
                                'HBUAsIfVacant', 'PresentUse', 'SqFtLot', 
                                'Topography', 'RestrictiveSzShape', 
                                'MtRainier', 'Olympics', 'Cascades', 
                                'Territorial', 'SeattleSkyline', 'PugetSound',
                                'LakeWashington', 'LakeSammamish', 
                                'SmallLakeRiverCreek', 'OtherView',
                                'WfntLocation', 'WfntFootage', 'WfntBank',
                                'TrafficNoise', 'Contamination')]
 
  # Limit Columns in Res Bldg 
 
  resbldg.data <- resbldg.data[,c('pinx', 'BldgNbr', 'NbrLivingUnits',
                                  'Stories', 'BldgGrade', 'SqFtTotLiving',
                                  'SqFtTotBasement', 'SqFtFinBasement',
                                  'SqFtGarageBasement', 'SqFtGarageAttached',
                                  'SqFtDeck', 'Bedrooms', 'BathHalfCount', 
                                  'Bath3qtrCount', 'BathFullCount', 'YrBuilt',
                                  'YrRenovated', 'Condition')]
  
  resbldg.data <- resbldg.data[resbldg.data$BldgNbr == 1, ]
  resbldg.data <- resbldg.data[!duplicated(resbldg.data$pinx), ]

 ## Integrate
  
  # Add parcel data to sales
  trim.sales <- merge(trim.sales, parcel.data, by='pinx')
 
  # Add res bldg data to sales
  trim.sales <- merge(trim.sales, resbldg.data, by='pinx')
 
### Integrate the geospatial data (parcel and beat) with the sales -----------------------  
  
 ## Convert the parcel file to centroids

  # Load in parcel file
  parcels <- st_read(file.path(data.dir, 'geographic/parcel/parcel.shp'),
                     quiet=TRUE)
  
  # Tranform to appropriate CRS
  parcels <- st_transform(parcels, 4326)
  
  # Extract centroid Lat longs
  parcel.centroids <- st_centroid(parcels)
  longs <- unlist(lapply(parcel.centroids, function(x) x[1]))
  lats <- unlist(lapply(parcel.centroids, function(x) x[2]))
  
  # Build a new data.frame
  parcel.xy <- data.frame(pinx=paste0('..', parcels$PIN),
                          longitude=longs,
                          latitude=lats)
  
  # Limit to those parcels in the sale dataset
  parcel.xy <- parcel.xy[parcel.xy$pinx %in% trim.sales$pinx, ]

  # Conver to a spdf
  parcel.sp <- SpatialPointsDataFrame(coords=cbind(parcel.xy$longitude,
                                                   parcel.xy$latitude),
                                      data=parcel.xy, 
                                      proj4string=CRS("+init=epsg:4326"))

  # Convert to a simple feature object
  parcel.sf <- st_as_sf(parcel.sp)
  
  # Convert the CRS to lat/long
  parcel.sf <- transform(parcel.sf, 4326)
      
 ## Prepare the Police Beats file

 # Read in the Police Beats Data
  beats <- st_read(file.path(data.dir, 'beats/SPD_BEATS_WGS84.shp'),
                   quiet=TRUE)

 # Transform the Coordinate Reference System  
  beats <- st_transform(beats, 4326)
  
 # Remove the beats in the water precincts  
  beats <- beats[beats$first_prec != '', ]
  
## Add the Beat Identification to the sales
  
  # Set null values
  parcel.sf$beat <- 'NONE'
  
  # Peform intersection
  beats.overlay <- st_intersects(beats, parcel.sf)
  
  # Extract intersection and add to parcel sf
  for(i in 1:length(beats.overlay)){
   ov.id <- beats.overlay[[i]]
   parcel.sf$beat[ov.id] <- as.character(beats$beat[i])
  }
  
  # Remove sales not in Beat Precincts
  parcel.sf <- parcel.sf[parcel.sf$beat != "NONE", ]  

 ## Add location data to sales  
  
  # Join data
  final.sales <- merge(trim.sales, 
                       parcel.sf[ , c('pinx', 'beat', 'longitude', 'latitude')],
                       by='pinx')
    
  # Write out
  tExists <- dbExistsTable(sales.conn, 'finalSales')
  
  if(tExists) {
    dbRemoveTable(sales.conn, 'finalSales')
  }
  dbWriteTable(sales.conn, 'finalSales', trim.sales, row.names=FALSE)

  # Close
  dbDisconnect(sales.conn)

  
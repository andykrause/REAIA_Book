
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

### Initial clean of sales to eliminate non-relevant observations ------------------------ 

  # Read in Sales File
  sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
  sales.data <- dbReadTable(sales.conn, 'Sales')
  
 ## Transform and Filtering Activities for Sales (Interspersed) 
  
  # Filter those missing PIN numbers or Price
  sales.data <- sales.data[sales.data$Major > 0, ]
  sales.data <- sales.data[sales.data$SalePrice > 0, ]
  
  # Transform new date fields 
  sales.data$doc.date <- paste(substr(sales.data$DocumentDate, 4, 5)
                              ,substr(sales.data$DocumentDate, 1, 2),
                              substr(sales.data$DocumentDate, 7, 10), sep="")
  sales.data$sales.date <- as.POSIXct(strptime(sales.data$doc.date, "%d%m%Y"))
  sales.data$sales.year <- as.numeric(format(sales.data$sales.date, "%Y"))
  
  # Filter those missing a sales date
  sales.data <- sales.data[!is.na(sales.data$sales.date), ]
  
  
  # Filter sales based on study relevancy (time period)
  sales.data <- sales.data[sales.data$sales.year >= 2011 & 
                             sales.data$sales.year <= 2016, ]
  
  # Transform Major and Minor into a PIN identifier
  sales.data <- buildPinx(sales.data)
  
  # Transform & Filter: Add trans count and limit by paramter
  sales.data <- buildTransCount(sales.data, trans.limit=5)
  
  # Transform: Add MultiParcel sale designation
  sales.data <- idDup(x.data=sales.data, 
                      x.field='ExciseTaxNbr', 
                      new.field = 'multi.parcel',
                      idd.type='labelNonUnique', 
                      bin.nonuq=TRUE)
  
  # Filter those with multiparcel
  sales.data <- sales.data[sales.data$multi.parcel == 0, ]
  
  # Add unique IDs
  sales.data <- buildSaleUIDs(sales.data)
  
  # Filter by Sale Insturment, Reason and Warning
  
  # Transform the "Warning" Field.  
  #Add a leading/trailing space for the grep()
  
  sales.data$SaleWarning <- paste(" ", sales.data$SaleWarning, 
                                  " ", sep="")

  # Create a list of factors to eliminate  
  trim.list <- list(SaleReason=2:19,  
                    SaleInstrument=c(0, 1, 4:28),
                    SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 25, 27,
                                              31:33, 37, 39, 43, 46, 48, 49,
                                              50:53, 59, 61, 63, 64, 66), " "))
  
  # Loop through each factor type and filter accordingly
  for(tL in 1:length(trim.list)){
    sales.data <- trimByField(x.data=sales.data, 
                              x.field=names(trim.list)[tL],
                              trim.list = unlist(trim.list[tL]))
  }
  
  # Transform:  Limit field names
  sales.fields <- c('pinx', 'rec.ID', 'sale.ID', 'SalePrice', 'doc.date',
                    'sales.date', 'sales.year')
  sales.data <- sales.data[ ,sales.fields]
  names(sales.data)[names(sales.data)=='SalePrice'] <- 'sale.price'

### Integrate Sale Record and Use Type------------------------------------------------------
  
 ## Read in Data
  
  # Parcel Data
  parcel.data <- dbReadTable(sales.conn, 'parcel')
  parcel.data <- buildPinx(parcel.data)
  
  # Res Building data
  resbldg.data <- dbReadTable(sales.conn, 'resbldg')
  resbldg.data <- buildPinx(resbldg.data)
  
  # Integrate the record type (Whether or not Residential) (Labeling)
  sales.data$res.record <- resbldg.data$BldgNbr[match(sales.data$pinx,
                                                      resbldg.data$pinx)]
  
  # Transform the integrated field to binary
  sales.data$res.record <- ifelse(is.na(sales.data$res.record), 0, 
                                  sales.data$res.record)
  
  # Filter those with non-residential record type or with more than one dwelling on it  
  sales.data <- sales.data[sales.data$res.record == 1, ]
  
  # Transform:  Remove Res.record field
  sales.data$res.record <- NULL
  
  # Integrate property use field
  sales.data$present.use <- parcel.data$PresentUse[match(sales.data$pinx,
                                                   parcel.data$pinx)]
  
  # Filter those with no present use
  sales.data <- sales.data[!is.na(sales.data$present.use), ]
  
  # Filter those not with SFR or Townhome use category
  sales.data <- sales.data[sales.data$present.use == 2 |
                           sales.data$present.use == 29, ]
  
### Integrate Sales data with assessor data ----------------------------------------------  

 ## Transform the assessor data
  
  # Parcel Data
  
  # Transform:  Remove unneccessary fields
  parcel.data <- parcel.data[,c('pinx', 'Area', 'SubArea', 'CurrentZoning',
                                'HBUAsIfVacant', 'SqFtLot', 
                                'Topography', 'RestrictiveSzShape', 
                                'MtRainier', 'Olympics', 'Cascades', 
                                'Territorial', 'SeattleSkyline', 'PugetSound',
                                'LakeWashington', 'LakeSammamish', 
                                'SmallLakeRiverCreek', 'OtherView',
                                'WfntLocation', 'WfntFootage', 'WfntBank',
                                'TrafficNoise', 'Contamination')]
  
  # Transform: Change field names
  names(parcel.data) <- c('pinx', 'area', 'sub.area', 'zoning', 
                          'hbu.vacant', 'lot.size', 'topo', 'restr.szshp',
                          'view.rainier', 'view.olympics', 'view.cascades',
                          'view.terr', 'view.city', 'view.puget', 
                          'view.lkwash', 'view.lksamm', 'view.smwater',
                          'view.other', 'wfnt', 'wfnt.ftg', 'wfnt.bank',
                          'traffic.noise', 'contam')
  
  # Res Building Data
  
  # Transform: Remove unnecessary fields 
  resbldg.data <- resbldg.data[,c('pinx', 'BldgNbr', 'NbrLivingUnits',
                                  'Stories', 'BldgGrade', 'SqFtTotLiving',
                                  'SqFtFinBasement',
                                  'SqFtGarageBasement', 'SqFtGarageAttached',
                                  'SqFtDeck', 'Bedrooms', 'BathHalfCount', 
                                  'Bath3qtrCount', 'BathFullCount', 
                                  'YrBuilt',
                                  'YrRenovated', 'Condition')]
  
  # Transform: Change field names
  names(resbldg.data) <- c('pinx', 'bldg.nbr', 'nbr.lu', 'stories',
                           'bldg.grade', 'tot.sf', 'bsmt.sf',
                           'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                           'beds', 'bath.half', 'bath.75', 'bath.full',
                           'yr.built', 'yr.ren', 'condition')
  
  # Filter: remove multi-structure sites from resbldg
  
  resbldg.data <- resbldg.data[order(resbldg.data$bldg.nbr), ]
  resbldg.data <- resbldg.data[!duplicated(resbldg.data$pinx), ]
  resbldg.data <- resbldg.data[resbldg.data$bldg.nbr == 1, ]
  
 ## Integrate Assessor data and sales data
  
  # Join parcel data to sales (inner join)
  sales.data <- merge(sales.data, parcel.data, by='pinx')
 
  # Join res bldg data to sales (inner join)
  sales.data <- merge(sales.data, resbldg.data, by='pinx')
 
### Integrate the geospatial data (parcel and beat) with the sales -----------------------  
  
 ## Prepare Centroids Data  
  
  # Load Data
  load(file= file.path(data.dir, 'geographic/parcelcentroids.Rdata'))
  
  # Transform:  Add pinx field for joining
  parcel.centroids$pinx <- paste0('..', parcel.centroids$PIN)
  
  # Transform: Remove unnecessary fields
  parcel.centroids$MAJOR <- NULL
  parcel.centroids$MINOR <- NULL
  parcel.centroids$PIN <- NULL
  
  # Transform:  Change CRS
  parcel.centroids$centroid <- st_transform(parcel.centroids$centroid, 4326)
  
  # Filter: Limit to those parcels in the sale dataset
  parcel.centroids <- dplyr::filter(parcel.centroids, pinx %in% sales.data$pinx)

 ## Prepare the Police Beats file

 # Load Data
  load(file= file.path(data.dir, 'geographic/beats.Rdata'))
  
 # Filter: Remove water precincts  
  beats <- dplyr::filter(beats, first_prec != '')
  
 # Save beats
  save(beats, file=file.path(data.dir, 'geographic/beats.Rdata'))
  
## Integrate the Beat Identification to the sales
  
  # Transform:  Set up null field
  parcel.centroids$beat <- 'NONE'
  
  # Integrate: Perform geospatial intersection (spatial join)
  beats.overlay <- st_intersects(beats, parcel.centroids)
  
  # Transform: Extract intersection and add to parcel centroids
  for(i in 1:length(beats.overlay)){
    
    ov.id <- beats.overlay[[i]]
    parcel.centroids$beat[ov.id] <- as.character(beats$beat[i])

  }
  
  # Filter: Remove sales not in Beat Precincts
  parcel.centroids <- dplyr::filter(parcel.centroids, beat != "NONE")
  
 ## Add location data to sales  
  
  # Transform: Create Separate Lat/long Columns 
  parcel.centroids$longitude <- unlist(lapply(parcel.centroids$centroid, function(x) x[1]))
  parcel.centroids$latitude <- unlist(lapply(parcel.centroids$centroid, function(x) x[2]))
  
  # Save Data
  save(parcel.centroids, 
       file= file.path(data.dir, 'geographic/parcelcentroids_sales.Rdata'))
  
  
  # Integrate: Join data
  sales.data <- merge(sales.data, 
                      parcel.centroids[ , c('pinx', 'beat', 'longitude', 'latitude')],
                      by='pinx')
  
  # Write to the database
  dbWriteTable(sales.conn, 'prepSales', sales.data, row.names=FALSE, overwrite=TRUE)

  # Close
  dbDisconnect(sales.conn)

  

## Load libraries

  require(RODBC)
  require(RSQLite)
  require(stringr)
  require(sf)
  
## Read in Sales Data
  
  # Set DB
  salesDB <- 'C:/Data/USA/wa/King/Assessor/kingsales.db'

  # Open Connection
  salesConn <- dbConnect(dbDriver('SQLite'), salesDB)
  condoSales <- dbReadTable(salesConn, 'condoSales')
  
  # Read in Data
  resSales <- dbReadTable(salesConn, 'resSales')

  # Close connection
  dbDisconnect(salesConn)

## Read in Geographic Data
  
  parcel.points <- st_read('c:/data/usa/wa/king/geographic/parcelpoints2016.shp')
  
## Limit Sales to 2016
 
  rsales.2016 <- resSales[resSales$salesYear == 2016, ]
  ksales.2016 <- condoSales[condoSales$salesYear == 2016, ]

## Extract lat/long  
  
  parcel.points$long <- unlist(lapply(parcel.points$geometry, 
                                      function(x) x[1]))
  parcel.points$lat <- unlist(lapply(parcel.points$geometry, 
                                     function(x) x[2]))

## Add to Res Sales
  
  # Add
  rsales.2016$long <- parcel.points$long[match(substr(rsales.2016$pinx, 3,15), 
                                          parcel.points$PIN)]
  rsales.2016$lat <- parcel.points$lat[match(substr(rsales.2016$pinx, 3,15), 
                                          parcel.points$PIN)]

  ## Remove those with missing
  rsales.2016 <- rsales.2016[!is.na(rsales.2016$lat), ]

## Add to condo sales  
  
  # Add
  ksales.2016$long <- parcel.points$long[match(substr(ksales.2016$pinc, 3,15), 
                                          parcel.points$PIN)]
  ksales.2016$lat <- parcel.points$lat[match(substr(ksales.2016$pinc, 3,15), 
                                        parcel.points$PIN)]

  # Remove those missing
  ksales.2016 <- ksales.2016[!is.na(ksales.2016$lat), ]

## Write out data  
  write.csv(rsales.2016, 
            'C:/Dropbox/Research/bigDataBook/data/ressales2016.csv', 
            row.names=F)
  write.csv(ksales.2016, 
            'C:/Dropbox/Research/bigDataBook/data/condosales2016.csv', 
            row.names=F)
  
  
  
  #####
  
  
  download.file(url=paste0('https://data.seattle.gov/views/nnxn-434b/files/',
                           '96d998d4-ae20-4ea8-b912-436e68982a0d.zip'), 
                destfile='c:/temp/beatsxx.zip' )
  
  
  
  # Unzip
  unzip('c:/temp/beatsxx.zip',
        exdir='c:/temp/beats')

  beats <- st_read('c:/temp/beats/SPD_BEATS_WGS84.shp')
  beats <- st_transform(beats, 4326)
  beats <- beats[beats$first_prec != '', ]
  
  
  library(sp)
  sales <- SpatialPointsDataFrame(coords=cbind(rsales.2016$long,
                                               rsales.2016$lat),
                                  data=rsales.2016,
                                  proj4string=CRS("+init=epsg:4283"))
  ss_sf = st_as_sf(sales, relation_to_geometry = "field")
  ss_sf <- st_transform(ss_sf, 4326)
  
  ss_sf$beat <- 'None'
  q <- st_intersects(beats,ss_sf)
  for(ii in 1:length(q)){
   hh <- q[[ii]]
   ss_sf$beat[hh] <- beats$beat[ii]
  }
  
  tt <- st_centroid(beats)
  
  

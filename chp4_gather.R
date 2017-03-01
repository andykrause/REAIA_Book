
 ## Set your data directory

  #data.dir <- 'c:/dropbox/research/bigdatabook/data/'
  data.dir <- 'c:/temp/'

## Set up a capture location for downloaded files
  
  # Create a directory if one doesn't exist
  if(!dir.exists(file.path(data.dir, 'raw_zip_files'))){
    dir.create(file.path(data.dir, 'raw_zip_files'))
  }
  
 ## Download and unzip the Seattle Police Beat Shapefile

  # Check if file exists, download and unzip if it doesn't
  if(!file.exists(file.path(data.dir, 'raw_zip_files', 'beats.zip'))){
  
    # Download
    download.file(url=paste0('https://data.seattle.gov/views/nnxn-434b/files/',
                         '96d998d4-ae20-4ea8-b912-436e68982a0d.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'beats.zip'))
  }  

  # Create a directory if one doesn't exist
  if(!dir.exists(file.path(data.dir, 'beats'))){
    dir.create(file.path(data.dir, 'beats'))
  }
  
  # Unzip the files
  unzip(file.path(data.dir, 'raw_zip_files', 'beats.zip'),
        exdir=file.path(data.dir, 'beats'))
        
 ## Download and unzip the King County parcel shapefile
  
  # Create a directory if one doesn't exist
  if(!dir.exists(file.path(data.dir, 'geographic'))){
    dir.create(file.path(data.dir, 'geographic'))
  }
  
  # Check if file exists, download and unzip if it doesn't
  if(!file.exists(file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'))){
    
    # Download
    download.file(url=paste0('ftp://ftp.kingcounty.gov/gis-web/web/',
                             'GISData/parcel_address_SHP.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'))
  }
  
  # Unzip
  unzip(file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'), 
        exdir=file.path(data.dir, 'geographic'))
  
## Download and unzip the King County Assessor's Data Files

  # Create a directory if one doesn't exist
  if(!dir.exists(file.path(data.dir, 'assessor'))){
    dir.create(file.path(data.dir, 'assessor'))
  }

  # Check if file exists, download and unzip if it doesn't
  if(!file.exists(file.path(data.dir, 'raw_zip_files', 'sales.zip'))){
    
  ## Sales File  
    # Download
    download.file(url=paste0('http://your.kingcounty.gov/extranet/assessor/',
                             'Real Property Sales.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'sales.zip'))
  }
  
  # Unzip
  unzip(file.path(data.dir, 'raw_zip_files', 'sales.zip'), 
        exdir=file.path(data.dir, 'assessor'))

  # Check if file exists, download and unzip if it doesn't
  if(!file.exists(file.path(data.dir, 'raw_zip.files', 'parcel.zip'))){
    
  ## Parcel File
    # Download
    download.file(url='http://your.kingcounty.gov/extranet/assessor/Parcel.zip', 
                  destfile=file.path(data.dir, 'raw_zip_files', 'parcel.zip'))
  }
  
  # Unzip
  unzip(file.path(data.dir, 'raw_zip_files', 'parcel.zip'), 
                  exdir=file.path(data.dir, 'assessor'))
  
  # Check if file exists, download and unzip if it doesn't
  if(!file.exists(file.path(data.dir, 'raw_zip_files', 'resbldg.zip'))){
    
  ## ResBldg
    # Download
    download.file(url='http://your.kingcounty.gov/extranet/assessor/Residential Building.zip', 
                  destfile=file.path(data.dir, 'raw_zip_files', 'resbldg.zip'))
  }
    
  # Unzip
  unzip(file.path(data.dir, 'raw_zip_files', 'resbldg.zip'), 
                  exdir=file.path(data.dir, 'assessor'))

### SAVE FOR PROCESS TO DOWNLOAD NICK'S TWITTER DATA -------------------------------------  
  
  
  


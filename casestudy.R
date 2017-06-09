##########################################################################################
#                                                                                        #            
#  Code for Chapter 12 (Case Studies)                                                    #
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
  library(OpenStreetMap)
  library(BMA)
  library(MASS)
  library(rgeos)
  library(geosphere)
  library(spgwr)
  library(spdep)
  library(car)
  library(lmtest)
  library(gstat)
  library(scales)

 ## Set data and code directory

  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  
  
### Load Workspace from Modeling chapter -------------------------------------------------
  
  # Modeling workspace
  load(file.path(data.dir, 'model_workspace.RData'))

  # Load beat spatial file
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))

  # Sent Tweets (MOVE THIS TO MANAGE, etc)  
  tweet.sent <- read.csv('c:/dropbox/research/bigdatabook/data/tweetsentiment.csv',
                         header=T)
  
### Crime on Price -----------------------------------------------------------------------
  
 ## Load data
  
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')
  
  sales.conn <- dbConnect(dbDriver('SQLite'), data.db)
  crime.data <- dbReadTable(sales.conn, 'Crime')

  # Fix the date field
  crime.data$crime.date <- as.Date(crime.data$crime.date)
  
 ## Add crime stats to the sales data
  ## MOVE TO PREPARE IN FUTURE
   
  # Limite crime data to 2015 or later  
  crime.data <- crime.data[crime.data$year >= 2015, ]
  
  # Set distance threshold in Meters
  dist.thres <- 400
  
  # Set blank values
  sales.data$crime.violent <- 0
  sales.data$crime.property <- 0
  sales.data$crime.traffic <- 0
  sales.data$crime.behavior <- 0
  sales.data$crime.other <- 0
  
  # Loop through each and calculate local crime counts
  for(j in 1:nrow(sales.data)){
    
    # Extract sales data
    j.data <- sales.data[j,]
    
    # Calculate time difference
    x.days <- j.data$sales.date - crime.data$crime.date
    
    # Limit crime data to time window
    c.data <- crime.data[x.days > 0 & x.days < 365, ]
    
    # Calculate distances
    j.dist <- distHaversine(j.data[,c('longitude', 'latitude')],
                            c.data[,c('longitude', 'latitude')])
    
    # Limit data to those within threshold
    cx.data <- c.data[j.dist < dist.thres, ]
    
    # Add count to the sales data
    sales.data$crime.violent[j] <- length(which(cx.data$crime.type == 'violent'))
    sales.data$crime.property[j] <- length(which(cx.data$crime.type == 'property'))
    sales.data$crime.behavior[j] <- length(which(cx.data$crime.type == 'behavior'))
    sales.data$crime.traffic[j] <- length(which(cx.data$crime.type == 'traffic'))
    sales.data$crime.other[j] <- length(which(cx.data$crime.type == 'other'))
    
    # Report on progress
    if(j %% 100 == 0){
      cat('record number ', j, '\n')
    }
  }
  
 ## Build crime model
  
  # OLS Specification
  crime.lm <- lm(update(as.formula(base.lm), . ~ . + crime.violent + crime.property + crime.traffic +
                   crime.behavior + crime.other),
                 data=sales.data)
  
  
 ## Spatial error specification
  # Build Data
  sales.sp <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     sales.data)
  
  # Create neighbor list
  nbList <- knn2nb(knearneigh(sales.sp, 10))
  
  # Create Distances
  nbDists <- nbdists(nbList, sales.sp)    
  
  # Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, 
                          glist = lapply(nbDists, dwf),
                          style="W",
                          zero.policy=T))
  
  # Estimate model
  crime.se <- errorsarlm(as.formula(crime.lm),
                         data=sales.sp,
                         swm,
                         method="spam", 
                         zero.policy=TRUE)
  
 ## Compare appreciation rates to crime at the Beat level
  
  ### MOVE TO PREPARE
  
  # Create quarter variable
  sales.data$month <- as.numeric(substr(sales.data$sales.date, 6, 7))
  sales.data$qtr <- ((sales.data$month - 1) %/% 3) + 1
  
  # Create table of crime counts by beat
  beat.crime <- dplyr::group_by(crime.data, zone.beat) %>% 
    dplyr::summarize(
      violent=length(which(crime.type =='violent')),
      property=length(which(crime.type == 'property')),
      behavior=length(which(crime.type == 'behavior')),
      traffic = length(which(crime.type == 'traffic')),
      other = length(which(crime.type == 'other')),
      all= n())
  
  # Add blank appreciation and sales count fields
  beat.crime$appr <- 0
  beat.crime$sales <- 0
  
  # Estimate a model at for each beat
  for(b in 1:nrow(beat.crime)){
    
    # Select sales
    beat.sales <- sales.data[sales.data$beat == beat.crime$zone.beat[b], ]
    
    # If enough sales for a model
    if(nrow(beat.sales) >= 100){
      
      # Create spatial points data frame
      beat.sp <- SpatialPointsDataFrame(cbind(beat.sales$longitude,
                                              beat.sales$latitude),
                                        beat.sales)
      
      # Make neighbors
      nbList <- knn2nb(knearneigh(beat.sp, 10))
      
      # Create Distances
      nbDists <- nbdists(nbList, beat.sp)    
      
      # Building Weights Matrix
      swm <- listw2U(nb2listw(nbList, 
                              glist = lapply(nbDists, dwf), 
                              style="W",
                              zero.policy=T))
      
      # Create model specification
      beat.spec <- as.formula(base.lm)
      beat.spec <- update(beat.spec, ~ . - sales.date)
      beat.spec <- update(beat.spec, ~ . + as.factor(qtr))
      
      # Remove waterfront if none exist
      if(length(which(beat.sales$is.waterfront == 1)) == 0){
        beat.spec <- update(beat.spec, ~ . - is.waterfront)
      }
      
      # Remove views if none exist
      if(length(table(beat.sales$view.best)) <= 1){
        beat.spec <- update(beat.spec, ~ . - view.best)
      }
      
      # Estimate the model
      beat.se <- tryCatch(errorsarlm(beat.spec,
                                     data=beat.sp,
                                     swm, 
                                     method="spam", 
                                     zero.policy=TRUE), silent=T)
      
      # Extract coefficients
      if(class(beat.se) == 'sarlm'){
        coefs <- summary(beat.se)$coefficients
        
        # Extract quarter 4 appreciation rate
        b.coef <- coefs[grep('qtr)4', names(coefs))]
        beat.crime$appr[b] <- b.coef
        beat.crime$sales[b] <- nrow(beat.sales)
        
      } else {
        
        beat.crime$appr[b] <- NA
      
      }
      
    } else {
      
      beat.crime$appr[b] <- NA
    }  
  }
  
  # Convert beats shapefile from 'sf' to 'sp'
  beats.sp <- as(beats, 'Spatial')
  beats.sp@data$id <- paste0("ID", 1:nrow(beats.sp@data))
  beats.spf <- broom::tidy(beats.sp)
  beats.spf$beat <- beats.sp@data$beat[match(beats.spf$id, 
                                             beats.sp@data$id)]
  
  # Extract area measurement from the shapefile
  btp <- beats.sp@polygons
  llarea <- unlist(lapply(btp, function(x) x@area))
  
  # Convert to square miles
  llarea <- llarea * (68.99 ^ 2)
  
  # Add to data
  beats.sp@data$size <- llarea
  
  # Add area measurement
  beat.crime$area <- beats.sp@data$size[match(beat.crime$zone.beat,
                                              beats.sp@data$beat)]
  
  # Remove beats with no appreciation
  beat.crime <- beat.crime[!is.na(beat.crime$appr), ]
  
  # Calculate  crime per square mile
  beat.crime$viol.area <- beat.crime$violent / beat.crime$area
  beat.crime$prop.area <- beat.crime$property / beat.crime$area
  beat.crime$beha.area <- beat.crime$behavior / beat.crime$area
  beat.crime$traf.area <- beat.crime$traffic / beat.crime$area
  beat.crime$othe.area <- beat.crime$other / beat.crime$area
  beat.crime$all.area <- beat.crime$all / beat.crime$area
  
 ## Make Plots
  
  # All crime
  all.crime.plot <- ggplot(beat.crime, 
                           aes(x=all.area, y=appr)) + 
                      geom_point() +
                      stat_smooth(se=FALSE, size=2) +
                      xlab("Reported Crimes per Sq. Mile") + 
                      ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
                      ggtitle('All Crime vs Appreciation') +
                      theme(plot.title = element_text(hjust = 0.5))
  
  # Violent crime
  viol.crime.plot <- ggplot(beat.crime, 
                           aes(x=viol.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Violent vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))

  # Property crime
  prop.crime.plot <- ggplot(beat.crime, 
                            aes(x=prop.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Property vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Behavior crime
  beha.crime.plot <- ggplot(beat.crime, 
                            aes(x=beha.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Behavioral vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Property crime
  traf.crime.plot <- ggplot(beat.crime, 
                            aes(x=traf.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Traffic vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Other crime
  othe.crime.plot <- ggplot(beat.crime, 
                            aes(x=othe.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Other vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
### Sentiment analysis -------------------------------------------------------------------  
  
 ## Limit to Tweets in the city (MOVE TO PREPARE)
  
  # Create city boundary
  seattle.bound <- gUnaryUnion(beats.sp)
  
  # Convert tweet to spatial poing data frame
  tweet.sp <- SpatialPointsDataFrame(cbind(tweet.sent$longitude, tweet.sent$latitude),
                                     data=tweet.sent)
  proj4string(tweet.sp) <- CRS(proj4string(beats.sp))
  
  # Clip by city boundary
  in.seattle <- gIntersects(tweet.sp, seattle.bound, byid=T)
  tweet.sp <- tweet.sp[which(in.seattle), ]
  
 ## Create a dataset with only tweet with a non-zero sentiment
  
  # Make dataset
  twsent.sp <- tweet.sp[tweet.sp@data$SentimentScore != 0, ]
  
  # Convert all tweets to -1 or 1
  twsent.sp@data$SS <- ifelse(twsent.sp@data$SentimentScore < 0, -1, 1)
  
  # Create a simle DF
  twsent <- twsent.sp@data
  
 ## Make plot of sentiment tweets
    
  sent.map <- ggplot() + 
    geom_polygon(data=beats.spf, aes(x=long, y=lat, group=beat), 
                 color='gray40', fill='gray80')+
    # coord_cartesian(xlim=c(min(sales.data$longitude), max(sales.data$longitude)),
    #                 ylim=c(min(sales.data$latitude), max(sales.data$latitude))) +
     geom_point(data=twsent, 
                aes(x=longitude, y=latitude, color=as.factor(SS)), 
                size=2) +
    scale_color_manual(values=c('blue', 'red'),
                       labels=c("Negative   ", 'Positive    '),
                       name='Sentiment    ') +
    ylab('') + 
    xlab('') +
    theme(legend.position='bottom', 
          axis.text=element_blank(),
          axis.ticks = element_blank())
  
 ## Create sentiment surface
  
  # Build surface
  sent.surf <- point2Surface(twsent.sp, 
                             twsent.sp$SS, 
                             res=.004, 
                             clip=seattle.bound, 
                             idp.val=5)
  
  # Convert to a tidy format
  surf.df <- data.frame(long=sent.surf@coords[,1],
                        lat=sent.surf@coords[,2],
                        sentiment=sent.surf@data$var1.pred)
  
  # Make a map
  sent.map <- ggplot(surf.df, aes(x=long, y=lat, z=sentiment)) +
    geom_tile(aes(fill = sentiment), alpha=.7) +
    scale_fill_gradient2(low=muted('red'), high=muted('blue'),
                         name='Sentiment    ',
                         breaks=c(-.75, 0, .75),
                         labels=c('Negative', 'Neutral', 'Positive')) + 
    ylab('') + 
    xlab('') +
    ggtitle('Sentiment in Seattle') +
    theme(legend.position='bottom',
          legend.key.width=unit(3,'cm'),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
 ## Calculate local housing appreciation  
    
  # Set the specification
  gwr.spec <- update(as.formula(base.lm), ~ . - sales.date)
  gwr.spec <- update(gwr.spec, ~ . + as.factor(qtr))
  
  # Create spatial points data set
  gwr.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     data=sales.data)
  
  # Run the local GWR models
  price.gwr <-gwr(gwr.spec, gwr.data, fit.points=sent.surf@coords, bandwidth=.01)
  
  # Extract the coefficients for Q4 appreciations
  gwr.coef <- price.gwr$SDF@data
  appr.coef <- gwr.coef[,ncol(gwr.coef)]
  
  # Add these to the data frame of surfact values
  surf.df$appr <- appr.coef
  
 ## Plot relationship
  
  sent.appr.plot <- ggplot(surf.df, aes(x=sentiment, y=appr)) + 
    geom_point(alpha=.4, size=.9) + 
    stat_smooth(size=2, se=TRUE, color='red') +
    scale_y_continuous(breaks=seq(0,.08, .01),
                       labels=c('0%', '1%', '2%', '3%', '4%', '5%', '6%', '7%', '8%')) +
    xlab('Sentiment Score') +
    ylab('Appreciation Rate in 2016\n') +
    # coord_cartesian(ylim=c(-.07, .17)) + 
    ggtitle('Local House Appreciation vs Sentiment') +
    theme(plot.title = element_text(hjust = 0.5))
  
### Save workspace for Case Study analysis -----------------------------------------------
  
  save.image(file.path(data.dir, 'casestudy_workspace.RData'))
  
  
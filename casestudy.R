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

 ## Set data and code directory

  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  
  
### Load Workspace from Modeling chapter -------------------------------------------------
  
  load(file.path(data.dir, 'model_workspace.RData'))
  
  
  
  
  
  ### Summary Stats and map ----------------------------------------------------------------
  
  # MOVE TO CASE STUDY SEcTION  
  
  export.path <- 'c:/dropbox/research/bigdatabook/chap12/figures/'
  
  ## Map of transactoins
  
  ## Set bounds for OSM map
  lowerleft <- c(min(sales.data$latitude) - .002,
                 min(sales.data$longitude) - .011)
  upperright <- c(max(sales.data$latitude) + .002,
                  max(sales.data$longitude) + .011)
  
  ## Get OSM Map
  
  # Get map
  seattle.map <- openmap(lowerleft, upperright, type='osm')
  
  # Change Projection
  seattle.map <- openproj(seattle.map, projection="+proj=longlat +datum=WGS84 +no_defs")
  
  ## Plot Map and sales points   
  
  seattle.map <- autoplot(seattle.map) + 
    geom_point(data=sales.data, 
               aes(x=longitude, y=latitude), 
               size=1.6) +
    ylab('') + 
    xlab('') +
    theme(legend.position='bottom', 
          axis.text=element_blank(),
          axis.ticks = element_blank())

  ## Summary Statistics
  
  # Calculate Field Types
  field.types <- unlist(lapply(sales.data, class))
  
  # Calculate summary table
  summ.table <- t(do.call(cbind, 
                          lapply(sales.data[,field.types == 'numeric' |
                                              field.types == 'integer'],
                                 fullSummary)))
  

  ### Crime on Price -----------------------------------------------------------------------
  
  sales.db <- file.path(data.dir, 'seattleCaseStudy.db')
  
  sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
  crime.data <- dbReadTable(sales.conn, 'Crime')
  
  crime.data <- crime.data[crime.data$year >= 2015, ]
  thres = 400
  crime.data$crime.date <- as.Date(crime.data$crime.date)
  
  library(geosphere)
  sales.data$crime.violent <- 0
  sales.data$crime.property <- 0
  sales.data$crime.traffic <- 0
  sales.data$crime.behavior <- 0
  sales.data$crime.other <- 0
  
  for(j in 1:nrow(sales.data)){
    j.data <- sales.data[j,]
    x.days <- j.data$sales.date - crime.data$crime.date
    c.data <- crime.data[x.days >0 & x.days < 365, ]
    
    
    j.dist <- distHaversine(j.data[,c('longitude', 'latitude')],
                            c.data[,c('longitude', 'latitude')])
    cx.data <- c.data[j.dist < thres, ]
    
    sales.data$crime.violent[j] <- length(which(cx.data$crime.type == 'violent'))
    sales.data$crime.property[j] <- length(which(cx.data$crime.type == 'property'))
    sales.data$crime.behavior[j] <- length(which(cx.data$crime.type == 'behavior'))
    sales.data$crime.traffic[j] <- length(which(cx.data$crime.type == 'traffic'))
    sales.data$crime.other[j] <- length(which(cx.data$crime.type == 'other'))
    
    if(j%%100 == 0){
      cat('record number\n', j, '\n\n')
    }
  }
  
  ## Build crime model
  
  crime.lm <- lm(log(sale.price) ~ as.factor(present.use) + log(lot.size) + log(tot.sf) + 
                   as.factor(bldg.grade) + baths + eff.age + traffic.noise + 
                   deck.sf + gar.att.sf+ 
                   view.best + is.waterfront + bsmt.sf + condition + beds + 
                   as.factor(month) + crime.violent + crime.property + crime.traffic +
                   crime.behavior + crime.other,
                 data=sales.data)
  
  
  sp.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                          sales.data$latitude),
                                    sales.data)
  
  nbList <- knn2nb(knearneigh(sp.data, 5))
  
  ## Create Distances
  nbDists <- nbdists(nbList, sp.data)    
  
  ## Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                          , style="W",zero.policy=T))
  
  crime.se <- errorsarlm(as.formula(crime.lm),data=sp.data,
                         swm, method="spam", zero.policy=TRUE)
  
  ### Estimate models by beat
  
  sales.data$qtr <- ((sales.data$month - 1) %/% 3) + 1
  
  beat.crime <- dplyr::group_by(crime.data, zone.beat) %>% 
    dplyr::summarize(
      violent=length(which(crime.type =='violent')),
      property=length(which(crime.type == 'property')),
      behavior=length(which(crime.type == 'behavior')),
      traffic = length(which(crime.type == 'traffic')),
      other = length(which(crime.type == 'other')),
      all= n())
  
  beat.crime$appr <- 0
  beat.crime$sales <- 0
  
  for(b in 1:nrow(beat.crime)){
    
    beat.sales <- sales.data[sales.data$beat == beat.crime$zone.beat[b], ]
    
    if(nrow(beat.sales) >= 100){
      
      beat.sp <- SpatialPointsDataFrame(cbind(beat.sales$longitude,
                                              beat.sales$latitude),
                                        beat.sales)
      
      nbList <- knn2nb(knearneigh(beat.sp, 5))
      
      ## Create Distances
      nbDists <- nbdists(nbList, beat.sp)    
      
      ## Building Weights Matrix
      swm <- listw2U(nb2listw(nbList, 
                              glist = lapply(nbDists, dwf), 
                              style="W",
                              zero.policy=T))
      
      beat.spec <- as.formula(base.adj.lm)
      beat.spec <- update(beat.spec, ~ . - as.factor(month))
      beat.spec <- update(beat.spec, ~ . + as.factor(qtr))
      if(length(which(beat.sales$is.waterfront == 1)) == 0){
        beat.spec <- update(beat.spec, ~ . - is.waterfront)
      }
      if(length(table(beat.sales$view.best)) <= 1){
        beat.spec <- update(beat.spec, ~ . - view.best)
      }
      
      
      beat.se <- tryCatch(errorsarlm(beat.spec,
                                     data=beat.sp,
                                     swm, 
                                     method="spam", 
                                     zero.policy=TRUE), silent=T)
      
      if(class(beat.se) == 'sarlm'){
        coefs <- summary(beat.se)$coefficients
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
  
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))
  beats.sp <- as(beats, 'Spatial')
  beats.sp@data$id <- paste0("ID", 1:nrow(beats.sp@data))
  beats.spf <- broom::tidy(beats.sp)
  beats.spf$beat <- beats.sp@data$beat[match(beats.spf$id, 
                                             beats.sp@data$id)]
  
  btp <- beats.sp@polygons
  llarea <- unlist(lapply(btp, function(x) x@area))
  llarea <- llarea * (68.99^2)
  beats.sp@data$size <- llarea
  beat.crime$area <- beats.sp@data$size[match(beat.crime$zone.beat,
                                              beats.sp@data$beat)]
  
  beat.crime <- beat.crime[!is.na(beat.crime$appr), ]
  
  beat.crime$viol.area <- beat.crime$violent/beat.crime$area
  beat.crime$prop.area <- beat.crime$property/beat.crime$area
  beat.crime$beha.area <- beat.crime$behavior/beat.crime$area
  beat.crime$traf.area <- beat.crime$traffic/beat.crime$area
  beat.crime$othe.area <- beat.crime$other/beat.crime$area
  beat.crime$all.area <- beat.crime$all/beat.crime$area
  
  ggplot(beat.crime, aes(x=all.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  ggplot(beat.crime, aes(x=viol.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  ggplot(beat.crime, aes(x=prop.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  ggplot(beat.crime, aes(x=beha.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  ggplot(beat.crime, aes(x=traf.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  ggplot(beat.crime, aes(x=othe.area, y=appr)) + 
    geom_point() +
    stat_smooth()
  
  ### Sentiment analysis -------------------------------------------------------------------  
  
  ##########
  
  tweet.sent <- read.csv('c:/dropbox/research/bigdatabook/data/tweetsentiment.csv',
                         header=T)
  
  tss <- dplyr::group_by(tweet.sent, screenName) %>%
    dplyr::summarize(count=n())
  
  tweet.sent <- merge(tweet.sent, tss, by='screenName')
  
  tweet.sent <- tweet.sent[tweet.sent$longitude > -123 & tweet.sent$longitude < -122, ]
  tweet.sent <- tweet.sent[tweet.sent$latitude > 47 & tweet.sent$latitude < 48, ]
  
  tweet.sp <- SpatialPointsDataFrame(cbind(tweet.sent$longitude, tweet.sent$latitude),
                                     data=tweet.sent)
  proj4string(tweet.sp) <- CRS(proj4string(beats.sp))
  
  tweet.0 <- tweet.sent[tweet.sent$SentimentScore != 0, ]
  tw0.sp <- SpatialPointsDataFrame(cbind(tweet.0$longitude, tweet.0$latitude),
                                   data=tweet.0)
  proj4string(tw0.sp) <- CRS(proj4string(beats.sp))
  
  
  ggplot(tweet.0, aes(x=longitude, y=latitude)) + 
    geom_point() + 
    geom_polygon(data=beats.spf, aes(x=long, y=lat, group=beat), 
                 color='gray40', fill='gray80')+
    coord_cartesian(xlim=c(min(sales.data$longitude), max(sales.data$longitude)),
                    ylim=c(min(sales.data$latitude), max(sales.data$latitude))) +
    geom_point(data=tweet.sent, aes(x=longitude, y=latitude, 
                                    color=SentimentScore), size=2) +
    scale_color_gradient(low='black', high='green')
  
  ggplot(tweet.sent, aes(x=longitude, y=latitude)) + 
    geom_polygon(data=beats.spf, aes(x=long, y=lat, group=beat), 
                 color='gray40', fill='gray80')+
    coord_cartesian(xlim=c(min(sales.data$longitude), max(sales.data$longitude)),
                    ylim=c(min(sales.data$latitude), max(sales.data$latitude))) +
    geom_point(data=sales.data, aes(x=longitude, y=latitude), 
               color='red', size=.9)
  
  
  library(spgwr)
  
  library(rgeos)
  bound <- gUnaryUnion(beats.sp)
  
  tweet.0$sss <- ifelse(tweet.0$SentimentScore >= 1, 1, -1)
  gg <- point2Surface(tw0.sp, tweet.0$SentimentScore, res=.004, clip=bound, idp.val=3)
  ggs <- point2Surface(tw0.sp, tweet.0$sss, res=.004, clip=bound, idp.val=3)
  
  gwr.spec <- as.formula(base.adj.lm)
  gwr.spec <- update(gwr.spec, ~ . - as.factor(month))
  gwr.spec <- update(gwr.spec, ~ . + as.factor(qtr))
  
  gwr.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     data=sales.data)  
  aa<-gwr(gwr.spec, gwr.data, fit.points=gg@coords, bandwidth=.01)
  
  gwr.coef <- aa$SDF@data
  
  appr.coef <- gwr.coef[,ncol(gwr.coef)]
  
  rr <- data.frame(x=gg@coords[,1],y=gg@coords[,2], ss=gg@data$var1.pred, 
                   sss=ggs@data$var1.pred,
                   appr=appr.coef)
  
  ggplot(rr, aes(x=x, y=y, color=ss))+geom_point(size=9, shape=15) +
    scale_color_gradient(low='black', high='green')
  
  ggplot(rr, aes(x=x, y=y, color=sss))+geom_point(size=9, shape=15) +
    scale_color_gradient(low='black', high='green')
  
  ggplot(rr, aes(x=sss, y=appr)) + geom_point() + stat_smooth() +
    xlab('Sentiment Score') +
    ylab('Appreciation in 2016')
  
  
  
  
  lowerleft=c(min(sales.data$latitude)-.02,
              min(sales.data$longitude)-.02)
  upperright=c(max(sales.data$latitude)+.02,
               max(sales.data$longitude)+.02)
  
  
  
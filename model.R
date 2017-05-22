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
  library(OpenStreetMap)
  library(BMA)
  library(MASS)
  library(rgeos)
  library(geosphere)
  library(spgwr)
  library(spdep)

 ## Set data and code directory

  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  sales.db <- file.path(data.dir, 'assessorData.db')

### Load data ----------------------------------------------------------------------------  

# Read in Sales File
sales.conn <- dbConnect(dbDriver('SQLite'), sales.db)
sales.data <- dbReadTable(sales.conn, 'cleanSales')

sales.data$sales.date <- as.Date(sales.data$sales.date)

sales.data <- dplyr::filter(sales.data, sales.year == 2016)






### Summary Stats and map ----------------------------------------------------------------

## Map of transactoins

lowerleft=c(min(sales.data$latitude) - .002,
            min(sales.data$longitude) - .011)
upperright=c(max(sales.data$latitude) + .002,
             max(sales.data$longitude) + .011)

seattle.map <- openmap(lowerleft, upperright, type='osm')
seattle.map <- openproj(seattle.map, projection="+proj=longlat +datum=WGS84 +no_defs")
seattle.map <- autoplot(seattle.map) + 
   geom_point(data=sales.data, 
              aes(x=longitude, y=latitude), 
              size=1.6) +
   ylab('') + xlab('') +
   theme(legend.position='bottom', 
         axis.text=element_blank(),
         axis.ticks = element_blank())
 
 export.path <- 'c:/dropbox/research/bigdatabook/chap12/figures/'

 
 png(file=file.path(export.path, 'fig1.png'))
  seattle.map
 dev.off()
 
## Summary Statistics
 field.types <- unlist(lapply(sales.data, class))
 
 summ.table <- t(do.call(cbind, 
                         lapply(sales.data[,field.types == 'numeric' |
                                             field.types == 'integer'],
                                fullSummary)))
 write.csv(summ.table, 
           file=file.path(export.path, 'table1.csv'), row.names=FALSE)
 
### Basic Modeling -----------------------------------------------------------------------


# Basic model

base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + tot.sf + 
                bldg.grade + baths + eff.age + traffic.noise + deck.sf + gar.att.sf + 
                view.best + is.waterfront + bsmt.sf + sales.date + topo + restr.szshp +
                condition + gar.bsmt.sf + beds,
              data=sales.data)



step.lm <- stepAIC(base.lm, direction="both")



bma.lm <- bicreg(y=log(sales.data$sale.price),
                 x=sales.data[,c('present.use', 'lot.size', 'tot.sf', 
                                 'bldg.grade', 'baths', 'eff.age', 'traffic.noise', 
                                 'deck.sf', 'gar.att.sf', 'view.best', 'is.waterfront',
                                 'bsmt.sf', 'sales.date', 'topo', 'restr.szshp',
                                 'condition', 'gar.bsmt.sf' , 'beds')])

  # Stepwise
  # BMA

sales.data$condition[sales.data$condition == 1] <- 2
sales.data$traffic.noise[sales.data$traffic.noise == 3] <- 2

view.combine <- which(sales.data$view.best %in% c('view.other', 'view.smwater',
                                                  'view.city', 'view.lkwash',
                                                  'view.puget', 'view.rainier',
                                                  'view.terr'))
sales.data$view.best[view.combine] <- 'view.other'

bma.lm <- bicreg(y=log(sales.data$sale.price),
                 x=sales.data[,c('present.use', 'lot.size', 'tot.sf', 
                                 'bldg.grade', 'baths', 'eff.age', 'traffic.noise', 
                                 'deck.sf', 'gar.att.sf', 'view.best', 'is.waterfront',
                                 'bsmt.sf', 'sales.date', 'condition', 'beds')])


base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + tot.sf + 
                bldg.grade + baths + eff.age + traffic.noise + deck.sf + gar.att.sf + 
                view.best + is.waterfront + bsmt.sf + sales.date + condition + beds,
              data=sales.data)

base.lm.table <- summary(base.lm)[[4]]
base.lm.diag <- data.frame(item=c('rsquared', 'sterr', 'fstat', 'AIC'),
                           values=c(summary(base.lm)$r.squared,
                                    summary(base.lm)$sigma,
                                    summary(base.lm)$fstatistic[1],
                                    AIC(base.lm)))

write.csv(base.lm.table, 
          file=file.path(export.path, 'table2_coef.csv'), row.names=FALSE)

write.csv(base.lm.diag, 
          file=file.path(export.path, 'table2_diag.csv'), row.names=FALSE)


### Diagnostics --------------------------------------------------------------------------

# Diagnostics

# Multicollinearity
library(car)
vif(base.lm)
sqrt(vif(base.lm)) > 2

# Heteroskedasticity
# Nonlinearities
library(lmtest)

bptest(base.lm)
coeftest(base.lm, vcov=hccm(base.lm))

#crPlots(base.lm)

# log TotSF, lotSF, factorize bldggrade, chop up sales date

#ncvTest(base.lm)
#spreadLevelPlot(base.lm)

sales.data$bldg.grade[sales.data$bldg.grade < 6] <- 6
sales.data$bldg.grade[sales.data$bldg.grade > 10] <- 10


# New model
sales.data$month <- as.numeric(as.factor(substr(sales.data$sales.date, 1, 7)))
base.adj.lm <- lm(log(sale.price) ~ as.factor(present.use) + log(lot.size) + log(tot.sf) + 
                   as.factor(bldg.grade) + baths + eff.age + traffic.noise + 
                    deck.sf + gar.att.sf+ 
                   view.best + is.waterfront + bsmt.sf + condition + beds + 
                    as.factor(month),
                  data=sales.data)

#ncvTest(base.adj.lm)
#spreadLevelPlot(base.adj.lm)
#crPlots(base.adj.lm)

  # Spatial Autocorrelation


source('c:/code/tools/spatialTools/spatEconTools.R')
sp.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                        sales.data$latitude),
                                  sales.data)



# Spatial Models

# LM Test
# Error or lag model


# # Develop a spatial weights matrix
 
  library(spdep)  
  dwf <- function(x) {1 / ((x + .00025) ^ 2)}

  nbList <- knn2nb(knearneigh(sp.data, 5))

## Create Distances
  nbDists <- nbdists(nbList, sp.data)    

 ## Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                        , style="W",zero.policy=T))
  
  mi.test <- moran.test(base.adj.lm$resid, swm, zero.policy=TRUE)
 
 # Determine form of spatial dependence
  lm.test <- lm.LMtests(base.adj.lm, swm,
                       test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
 
 # Specify spatial error Model
   mod.se <- errorsarlm(as.formula(base.adj.lm),data=sp.data,
                        swm, method="spam", zero.policy=TRUE)
# 
   mi.test.se <- moran.test(mod.se$resid, swm, zero.policy=TRUE)
   
  # GWR Model as a test
    gwr.spec <- as.formula(base.adj.lm)
    
    gwr.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                             sales.data$latitude),
                                       data=sales.data)  
    mod.gwr <-gwr(gwr.spec, gwr.data, bandwidth=.1)
    summary(mod.gwr$SDF@data)

## Write out results   
  
   base.adj.lm.table <- summary(base.adj.lm)[[4]]
   base.adj.lm.diag <- data.frame(item=c('rsquared', 'sterr', 'fstat', 'AIC'),
                              values=c(summary(base.adj.lm)$r.squared,
                                       summary(base.adj.lm)$sigma,
                                       summary(base.adj.lm)$fstatistic[1],
                                       AIC(base.adj.lm)))
   se.adj.lm.table <- cbind(mod.se$coefficients, mod.se$rest.se)
   se.adj.lm.diag <- data.frame(item=c('rsquared', 'sterr', 'fstat', 'AIC'),
                                  values=c(0,
                                           sqrt(mod.se$s2),
                                           0,
                                           AIC(mod.se)))
   
   write.csv(base.adj.lm.table, 
             file=file.path(export.path, 'table3_coef_base.csv'), row.names=FALSE)
   
   write.csv(base.adj.lm.diag, 
             file=file.path(export.path, 'table3_diag_base.csv'), row.names=FALSE)
   
   write.csv(se.adj.lm.table, 
             file=file.path(export.path, 'table3_coef_se.csv'), row.names=FALSE)
   
   write.csv(se.adj.lm.diag, 
             file=file.path(export.path, 'table3_diag_se.csv'), row.names=FALSE)
   
   
   
   
### Sensitivity Test ---------------------------------------------------------------------

  # Specification
  
   mod.se.sens.s1 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + log(lot.size) + 
                                  log(tot.sf) + bldg.grade + baths + eff.age +
                                  deck.sf + gar.att.sf + bsmt.sf + condition + beds + 
                                  as.factor(month),
                                data=sp.data,
                                swm, method="spam", 
                                zero.policy=TRUE)
   
   mod.se.sens.s2 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + 
                                  lot.size + tot.sf + bldg.grade + baths + eff.age + 
                                  traffic.noise + deck.sf + gar.att.sf+ 
                                  view.best + is.waterfront + bsmt.sf + condition + beds + 
                                  as.factor(month),
                                data=sp.data,
                                swm, method="spam", 
                                zero.policy=TRUE)
   
   mod.se.sens.s3 <- errorsarlm(log(sale.price) ~ lot.size + tot.sf + bldg.grade + 
                                  eff.age + as.factor(month),
                                data=sp.data,
                                swm, method="spam", 
                                zero.policy=TRUE)
   
   mod.se.sens.s4 <- lm(log(sale.price) ~ lot.size + tot.sf + bldg.grade + 
                                  eff.age + as.factor(month),
                                data=sp.data)
   
   a0 <- as.data.frame(mod.se$coefficients)
   a0$coef <- rownames(a0)
   
   a1 <- as.data.frame(mod.se.sens.s1$coefficients)
   a1$coef <- rownames(a1)
   
   a2 <- as.data.frame(mod.se.sens.s2$coefficients)
   a2$coef <- rownames(a2)
   
   a3 <- as.data.frame(mod.se.sens.s3$coefficients)
   a3$coef <- rownames(a3)
   
   a4 <- as.data.frame(mod.se.sens.s4$coefficients)
   a4$coef <- rownames(a4)
   
   spec.sens <- merge(a0, a1, by='coef', all.x=TRUE, all.y=TRUE)
   spec.sens <- merge(spec.sens, a2, by='coef', all.x=TRUE, all.y=TRUE)
   spec.sens <- merge(spec.sens, a3, by='coef', all.x=TRUE, all.y=TRUE)
   spec.sens <- merge(spec.sens, a4, by='coef', all.x=TRUE, all.y=TRUE)
   
   spec.sens <- spec.sens[order(spec.sens$coef),] 
   
  # Discordant Values
  
   d1.data <- sp.data[sp.data@data$discordant == 0, ]
   nbList <- knn2nb(knearneigh(d1.data, 10))
   
   ## Create Distances
   nbDists <- nbdists(nbList, d1.data)    
   
   ## Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                           , style="W",zero.policy=T))
   
   mod.sens.d1 <- errorsarlm(as.formula(base.adj.lm),
                             data=d1.data,
                             swm, method="spam", zero.policy=TRUE)
  
   
   d2.id <- grep('sale.price', sales.data$disc.fields)
   d2.data <- sp.data[-d2.id, ]
   nbList <- knn2nb(knearneigh(d2.data, 10))
   
   ## Create Distances
   nbDists <- nbdists(nbList, d2.data)    
   
   ## Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                           , style="W",zero.policy=T))
   
   mod.sens.d2 <- errorsarlm(as.formula(base.adj.lm),
                             data=d2.data,
                             swm, method="spam", zero.policy=TRUE)
   
   
    
   d3.id <- grep('UV:', sales.data$disc.type)
   d3.data <- sp.data[-d3.id, ]
   nbList <- knn2nb(knearneigh(d3.data, 10))
   
   ## Create Distances
   nbDists <- nbdists(nbList, d3.data)    
   
   ## Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                           , style="W",zero.policy=T))
   
   mod.sens.d3 <- errorsarlm(as.formula(base.adj.lm),
                             data=d3.data,
                             swm, method="spam", zero.policy=TRUE)
   
   a0 <- as.data.frame(mod.se$coefficients)
   a0$coef <- rownames(a0)
   a0$d1 <- summary(mod.sens.d1)$coefficients
   a0$d2 <- summary(mod.sens.d2)$coefficients
   a0$d3 <- summary(mod.sens.d3)$coefficients
   
   sens.disc <- a0[,c(2,1,3,4,5)]
   
   # Sample (cross-validation)

   cv.list <- list()
   
   for(i in 1:10){
     
     set.seed(i)
     cv.id <- sample(1:nrow(sales.data), round(nrow(sales.data) * .75), 0)
     cv.data <- sp.data[-cv.id, ]
     nbList <- knn2nb(knearneigh(cv.data, 10))
     
     ## Create Distances
     nbDists <- nbdists(nbList, cv.data)    
     
     ## Building Weights Matrix
     swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                             , style="W",zero.policy=T))
     
     cv.list[[i]] <- errorsarlm(as.formula(base.adj.lm),
                               data=cv.data,
                               swm, method="spam", zero.policy=TRUE)
    
   }
   
  sens.data <-  cbind(cv.list[[1]]$coefficients,
                      cv.list[[2]]$coefficients,
                      cv.list[[3]]$coefficients,
                      cv.list[[4]]$coefficients,
                      cv.list[[5]]$coefficients,
                      cv.list[[6]]$coefficients,
                      cv.list[[7]]$coefficients,
                      cv.list[[8]]$coefficients,
                      cv.list[[9]]$coefficients,
                      cv.list[[10]]$coefficients)
   
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
  
  
  
  
  
  
  
     
   
   



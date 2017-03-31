
### Figure 1: Median Home Sale Prices, Metro Level ------------------------------

 ## Load Necessary Libraries

  library(ggplot2)
  library(reshape2)

 ## Set path to export

  export.path <- 'c:/dropbox/research/bigDataBook/chap9/'

 ## Read in custom script  
  
  source('c:/code/dataviztools/ggplottools.R')
  
 ## Download data from Zillow

  metro.sales <- read.csv(paste0('http://files.zillowstatic.com/',
                                 'research/public/Metro/Metro_',
                                 'MedianSoldPrice_AllHomes.csv'), 
                          header=T)
  
 ## Create comparison dataset
  
  # Extract Price Data
  comp.data <- data.frame(id=metro.sales$RegionID,
                          price=metro.sales$X2016.06)
  
  # Remove those with missing values
  comp.data <- comp.data[!is.na(comp.data$price), ]
  
  # Create a hypothetical normal distribution
  set.seed(123)
  comp.data$norm <- rnorm(n=nrow(comp.data),
                          mean=mean(comp.data$price),
                          sd=sd(comp.data$price))
  
  # Melt to a tidy data.frame
  comp.tdf <- melt(comp.data, id='id') 
  
  ## Make Plot
  
  metro.plot <- ggplot(data=comp.tdf, aes(x=value, group=variable, 
                       fill=variable, alpha=variable)) +
    geom_density()  +
    ylab('Frequency') +
    xlab('\nJune 2016 Median Sale Price') + 
    ggtitle('U.S. Metro Area Median Sale Prices') +
    scale_x_continuous(breaks=seq(0, 800000, 200000),
                       labels=c('$0', '$200k', '$400k', '$600k', '$800k'))+
    scale_alpha_manual(values=c(1, .2),
                       labels=c('Actual Prices  ', 'Hypothetical Normal Distribution'),
                       name='') +
    scale_fill_manual(values=c('gray50', 'gray20'),
                       labels=c('Actual Prices  ', 'Hypothetical Normal Distribution'),
                      name='') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y=element_blank()) +
    coord_cartesian(xlim=c(-100000, 900000))

 ## Create figure
  
  png(file=paste0(export.path, 'chp9_fig1.png'), 
      bg='transparent')
    metro.plot
  dev.off()
  
### Figure 2 - Outlier examples ----------------------------------------------------------
  
 ## Create basic size and price data
  
  size <- c(700, 900, 1050, 1100, 1225, 1275, 1300, 1500, 1700, 1800, 1950, 2200,
            2350, 2500, 2600, 2725, 2800, 2825, 2900, 3015)
  price <- size * runif(20, .85, 1.15) * 175
  
  sp.data <- data.frame(size=size,
                        price=price,
                        type=1)
 
 ## Create three outlier types
  
  sp.1 <- rbind(sp.data, data.frame(size=1301, price=410000, type=2))
  sp.2 <- rbind(sp.data, data.frame(size=7000, price=7000 * 185, type=2))
  sp.3 <- rbind(sp.data, data.frame(size=6100, price=140000, type=2))
  
 ## Make Contextual Outlier Plot  
  
  plot1 <- ggplot(sp.1, aes(x=size, y=price, 
                            colour=as.factor(type), 
                            shape=as.factor(type))) +
    geom_point(size=5) +
    xlab('\nHome Size (sq.ft.)') + 
    ylab('Home Price\n') +
    scale_y_continuous(breaks=seq(0,600000, 100000),
                       labels=c('$0', '$100,000', '$200,000', '$300,000', '$400,000',
                                '$500,000', '$600,000')) + 
    scale_colour_manual(values=c('gray50', 'gray10')) +
    theme_bw() +
    theme(legend.position='none', 
          plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=c(0, 600000)) + 
    ggtitle('Conditional Outlier')
  
 ## Make Leverage Point Plot  
  
  plot2 <- ggplot(sp.2, aes(x=size, y=price, 
                            colour=as.factor(type), 
                            shape=as.factor(type))) +
    geom_point(size=5) +
    xlab('\nHome Size (sq.ft.)') + 
    ylab('Home Price\n') +
    scale_y_continuous(breaks=seq(0,1250000, 250000),
                       labels=c('$0', '$250,000', '$500,000', '$750,000',
                                '$1,000,000', '$1,250,000')) + 
    scale_colour_manual(values=c('gray50', 'gray10')) +
    theme_bw() +
    theme(legend.position='none', 
          plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=c(0, 1350000))+ 
    ggtitle('Leverage Point')
  
  ## Make contaminant plot
  
  plot3 <- ggplot(sp.3, aes(x=size, y=price, 
                            colour=as.factor(type), 
                            shape=as.factor(type))) +
    geom_point(size=5) +
    xlab('\nHome Size (sq.ft.)') + 
    ylab('Home Price\n') +
    scale_y_continuous(breaks=seq(0,600000, 100000),
                       labels=c('$0', '$100,000', '$200,000', '$300,000', '$400,000',
                                '$500,000', '$600,000')) + 
    scale_colour_manual(values=c('gray50', 'gray10')) +
    theme_bw() +
    theme(legend.position='none', 
          plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=c(0, 600000))+ 
    ggtitle('Contaminant')
  
  ## Create figure
  
  png(file=paste0(export.path, 'chp9_fig2.png'), width = 580, height = 1380, 
      bg='transparent')
    ggMultiPlots(plot1, plot2, plot3, cols=1)
  dev.off()
  
## Figure 3
  
  ## Download data from Zillow
  
  state.zhvi <- read.csv(paste0('http://files.zillowstatic.com/',
                                'research/public/State/State_',
                                'Zhvi_SingleFamilyResidence.csv'), 
                         header=T)
  
  
  cal.zhvi <- state.zhvi[state.zhvi$RegionName == 'California', ]
  cal.zhvi$RegionID <- cal.zhvi$RegionName <- NULL
  cal.data <- melt(cal.zhvi, id='SizeRank')
  cal.data$month <- 1:nrow(cal.data)
  cal.data$SizeRank <- cal.data$variable <- NULL
  
  cal.data$type <- 'normal'
  cal.data$value[72] <- cal.data$value[72]*1.5
  cal.data$type[72] <- 'outlier'
  
  cal.index <- ggplot(cal.data, aes(x=month, y=value,
                       colour=as.factor(type),
                       size=as.factor(type))) +
    geom_point() +
    scale_color_manual(values=c('gray60', 'gray10'),
                       name='',
                       labels=c('Normal  ', 'Contextual Outlier'))+
    scale_size_manual(values=c(1.2,3),
                       name='',
                       labels=c('Normal  ', 'Contextual Outlier'))+
    scale_x_continuous(breaks=seq(10, 250, 24),
                       label=seq(1997,2017,2)) +
    xlab('') +
    scale_y_continuous(breaks=seq(200000, 600000, 100000),
                       labels=c('$200,000', '$300,000',
                                '$400,000', '$500,000', 
                                '$600,000')) + 
    ylab('Median Home Value\n') +
    ggtitle('Zillow Median Home Value for California\n') +
    coord_cartesian(ylim=c(120000, 600000))+ 
    theme_bw()+
    theme(legend.position='bottom', 
          plot.title = element_text(hjust = 0.5))
  
  ## Create figure
  
  png(file=paste0(export.path, 'chp9_fig3.png'), width = 680, height = 380, 
      bg='transparent')
    cal.index
  dev.off()
  

    
  
  county.zhvi <- read.csv(paste0('http://files.zillowstatic.com/',
                                'research/public/Zip/Zip_',
                                'Zhvi_Summary_AllHomes.csv'), 
                         header=T)
  
  

  
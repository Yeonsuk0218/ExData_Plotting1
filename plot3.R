######################
## Exploratory Data Analysis/Week 1/Course Project 1
## Peer-graded Assignment: Course Project 1
## plot3.R
######################
relative_path='./week1/ExData_Plotting1/'

## Save an object to a file
object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')

source(paste0(relative_path, 'plot1.R'))

## Plot3
plotGraph3 <- function(subset_hpconsumption, DateTime, legend_bty) {
    plot(x=DateTime
         , y=c(Sub_metering_1=subset_hpconsumption$Sub_metering_1)
         , type='l', col='black'
         , ylim=c(0, 38)
         , xlab='', ylab='Energy sub metering')              
    lines(x=DateTime
          , y=c(Sub_metering_2=subset_hpconsumption$Sub_metering_2)
          , type='l', col='red')
    lines(x=DateTime
          , y=c(Sub_metering_3=subset_hpconsumption$Sub_metering_3)
          , type='l', col='blue')
    legend('topright'
           , lty=1
           , bty=legend_bty   # if (bty='n') then, no border
           , col=c('black', 'red', 'blue')
           , legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
}    
    
    
## Create a PNG file 2
createPlot3 <- function(subset_hpconsumption, relative_path='./week1/ExData_Plotting1/') {
    ## image size
    WIDTH <- 480
    HEIGHT <- 480
    
    DateTime <- as.POSIXlt(strptime(subset_hpconsumption$DateTime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))
    
    ## change locale to 'English'
    Sys.setlocale('LC_TIME', 'English')
    
    ## 1. Plot line graph with time series data 
    ## 1.1 Plot a graph with data1
    ## 1.2 Add line graphs of other two data
    windows(WIDTH, HEIGHT)
    
    plotGraph3(subset_hpconsumption, DateTime, legend_bty='o')

    ## save image file 3
    png_filename <- paste0(relative_path, 'plot3.png')
    dev.copy(png, file=png_filename)    # copy plot to a PNG file
    dev.off()  # close the PNG device
    
    Sys.getlocale()
    
    message(paste0(png_filename, ' was successfully created!'))
}


##-------------------------------------------------------------
## Process: call callProcess1() 
##-------------------------------------------------------------
## check file.exists, if (R object file does not exist) then
if (!file.exists(object_filename)) {
    callProcess1(relative_path)
} 

## Read R object(.rds) file
subset_hpconsumption <- readRDS(object_filename)   

## Create PNG file 3
createPlot3(subset_hpconsumption, relative_path)

##---------- The end of Week1: Course Project1/plot3.R ----------------

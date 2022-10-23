######################
## Exploratory Data Analysis/Week 1/Course Project 1
## Peer-graded Assignment: Course Project 1
## plot4.R
######################
relative_path='./week1/ExData_Plotting1/'

## Save an object to a file
object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')

source(paste0(relative_path, 'plot1.R'))
source(paste0(relative_path, 'plot2.R'))
source(paste0(relative_path, 'plot3.R'))

## Create a PNG file 2
createplot4 <- function(subset_hpconsumption, relative_path='./week1/ExData_Plotting1/') {
    ## image size
    WIDTH <- 480
    HEIGHT <- 480
    
    DateTime <- as.POSIXlt(strptime(subset_hpconsumption$DateTime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))
    
    ## change locale to 'English'
    Sys.setlocale('LC_TIME', 'English')
    
    subset_hpconsumption$Global_reactive_power
    
    ## 1. Plot line graph with time series data 
    windows(WIDTH, HEIGHT)
    par(mfcol=c(2, 2))
    
    ## plot2, location: (1, 1)
    plotGraph2(subset_hpconsumption, DateTime, power_unit='')

    ## Plot3, location: (2, 1)
    plotGraph3(subset_hpconsumption, DateTime, legend_bty='n')
        
    ## location: (1, 2)
    plot(x=DateTime
         , y=c(Sub_metering_1=subset_hpconsumption$Voltage)
         , type='l', col='black'
         , xlab='datetime', ylab='Voltage')              

    ## location: (2, 2)
    plot(x=DateTime
         , y=c(Sub_metering_1=subset_hpconsumption$Global_reactive_power)
         , type='l', col='black'
         , xlab='datetime', ylab='Global_reactive_power')              
    
    ## save image file 4
    png_filename <- paste0(relative_path, 'plot4.png')
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

## Create PNG file 4
createplot4(subset_hpconsumption, relative_path)

##---------- The end of Week1: Course Project1/plot4.R ----------------

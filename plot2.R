######################
## Exploratory Data Analysis/Week 1/Course Project 1
## Peer-graded Assignment: Course Project 1
## plot2.R
######################
relative_path='./week1/ExData_Plotting1/'

## Save an object to a file
object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')

source(paste0(relative_path, 'plot1.R'))

## Plot2
plotGraph2 <- function(subset_hpconsumption, DateTime, power_unit='(kilowatts)') {
    y_lab <- paste0('Global Active Power ', power_unit)
    
    plot(x=DateTime, y=subset_hpconsumption$Global_active_power
         , type='l'
         , xlab=''
         , ylab=y_lab
    )
}


## Create a PNG file 2
createPlot2 <- function(subset_hpconsumption, relative_path='./week1/ExData_Plotting1/') {
    ## image size
    WIDTH <- 480
    HEIGHT <- 480
    
    DateTime <- as.POSIXlt(strptime(subset_hpconsumption$DateTime, format='%Y-%m-%d %H:%M:%S', tz='UTC'))
    
    ## change locale to 'English'
    Sys.setlocale('LC_TIME', 'English')
    
    ## 1. Plot line graph with time series data 
    windows(WIDTH, HEIGHT)
    
    plotGraph2(subset_hpconsumption, DateTime)

    ## save image file 2
    png_filename <- paste0(relative_path, 'plot2.png')
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

## Create PNG file 2
createPlot2(subset_hpconsumption, relative_path)

##---------- The end of Week1: Course Project1/plot2.R ----------------

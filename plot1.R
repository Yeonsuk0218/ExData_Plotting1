######################
## Exploratory Data Analysis/Week 1/Course Project 1
## Peer-graded Assignment: Course Project 1
## plot1.R
######################
## Memory Management
## - Advanced R by Hadley Wickham
library(pryr)   # Useful tools to pry back the covers of R and understand the language at a deeper level.
library(devtools)
library(readr)
library(dplyr)
library(data.table)
library(stringr)

##--------------------
## download zip file from web and unzip files
##--------------------
downloadFile <- function(file_path, fileURL, fileext=fileext) {
    temp <- tempfile(tmpdir=relative_path, fileext=fileext)
    download.file(url=fileURL, destfile=temp)
    unzip(zipfile=temp, exdir=relative_path)
    unlink(temp)
}


##--------------------
## read txt file
##--------------------
readFile <- function(filename) {
    library(hms)
    household_power_consumption <- NULL
    
    ## check file.exists()
    if (file.exists(filename)) {
        household_power_consumption <- fread(filename, na.strings='?')

        ## change data type from char to date
        household_power_consumption$Date <- as.Date(household_power_consumption$Date, '%d/%m/%Y')
        
        ## if "file does not exist" then
    } else {
        message(paste0(filename, ' does not exist! Please check exact filename.'))
    }
    
    return(household_power_consumption)
}


##--------------------
## Make a subset 
##--------------------
makeSubset <- function(full_df) {
    household_power_consumption <- full_df
    
    # sel_idx <- (household_power_consumption$Date==as.Date('2006-12-16')) |
    sel_idx <- (household_power_consumption$Date==as.Date('2007-02-01')) |
        (household_power_consumption$Date==as.Date('2007-02-02'))
    household_power_consumption <- household_power_consumption[which(sel_idx), ]
    
    ## save column names before cbind()
    n_colnames <- names(household_power_consumption)
    
    ## make new column, DateTime
    ## strptime(): characters to time objects
    ## strftime(): convert time objects to characters
    DateTime <- strptime(paste0(household_power_consumption$Date, ' ', household_power_consumption$Time)
                         , format='%Y-%m-%d %H:%M:%S', tz='UTC')
    
    ## add DateTime as a column to original dataframe using cbind()
    household_power_consumption <- cbind(household_power_consumption, DateTime=strftime(DateTime))
    
    ## change data type from character to numeric
    ## if (nrow() is too big) then, fread() has a bug while converting data type.
    for (i in seq_along(n_colnames)) {
        if (i >=3 & i <= 9) {
            household_power_consumption[[names(household_power_consumption)[i]]] <- as.numeric(as.vector(household_power_consumption[[names(household_power_consumption)[i]]]))
            
        }
    }
    
    return(household_power_consumption)
}

## Plot1
plotGraph1 <- function(subset_hpconsumption) {
    hist(with(subset_hpconsumption, Global_active_power)
         , col='red'
         , main='Global Active Power'
         , xlab='Global Active Power (kilowatts)'
         , ylab='Frequency'
    )
}


## Create a PNG file
createPlot1 <- function(subset_hpconsumption, relative_path='./week1/ExData_Plotting1/') {
    ## image size
    WIDTH <- 480
    HEIGHT <- 480
    
    ## 1. Plot Histogram using base graphic devices
    windows(WIDTH, HEIGHT)
    
    ## plot1
    plotGraph1(subset_hpconsumption)
    
    ## save image file 1
    png_filename <- paste0(relative_path, 'plot1.png')
    dev.copy(png, file=png_filename)    # copy plot to a PNG file
    dev.off()  # close the PNG device
    
    message(paste0(png_filename, ' was successfully created!'))  
}


##-------------------------------------------------------------
## Call Functions 1
## - to process file download from web and save R object file for reuse.
##-------------------------------------------------------------
callProcess1 <- function(relative_path='./week1/ExData_Plotting1/') {
    ##--------------------
    ## 1. Call downloadFile() 
    ##--------------------
    fileext <- '.zip'
    # relative_path <- './week1/ExData_Plotting1/'
    fileURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    downloadFile(relative_path, fileURL, fileext)
    
    ##--------------------
    ## 2. Call readFile()
    ##--------------------
    filename <- paste0(relative_path, 'household_power_consumption.txt')
    household_power_consumption <- readFile(filename)

    ## check object size: object_size() tells you the size of a single object.
    object_size(household_power_consumption)
    # 149.92 MB
    
    dim(household_power_consumption)   # (2075259, 9)
    
    ##--------------------
    ## 3. Call makeSubset() to make a subset
    ## - filter data set which Date is between '2007-02-01' and '2007-02-02'.
    ## - add new column, DateTime that combines Date and Time.
    ##--------------------
    subset_hpconsumption <- makeSubset(household_power_consumption)
    
    ## check object size
    object_size(subset_hpconsumption)
    # 555.57 kB
    
    ##--------------------
    ## 4. save R object to 'rds' file for reuse.
    ## 
    ## Saving Data into R Data Format: RDS and RDATA
    ##--------------------
    ## Save an object to a file
    object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')
    saveRDS(subset_hpconsumption, file=object_filename)
    
    message(paste0(object_filename, ' was created.'))
}


##-------------------------------------------------------------
## Call Functions 2
## - Open R object file(rds) and create PNG file 1
##-------------------------------------------------------------
callProcess2 <- function(relative_path='./week1/ExData_Plotting1/') {
    ##--------------------
    ## 5. Reload Saved Datasets
    ##--------------------
    # relative_path <- './week1/ExData_Plotting1/'
    object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')
    subset_hpconsumption <- readRDS(object_filename)    

    ##--------------------
    ## 6. Create a PNG file
    ##--------------------
    createPlot1(subset_hpconsumption, relative_path)
}


##-------------------------------------------------------------
## Process: call callProcess1() and callProcess2()
##-------------------------------------------------------------
relative_path='./week1/ExData_Plotting1/'

## Save an object to a file
object_filename <- paste0(relative_path, 'subset_hpconsumption.rds')

## check file.exists, if (R object file does not exist) then
if (!file.exists(object_filename)) {
    callProcess1(relative_path)
} 

callProcess2(relative_path)

##---------- The end of Week1: Course Project1/plot1.R ----------------


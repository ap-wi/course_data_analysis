## ------------------------------------------------------------------------
## R-scripts
##   household_power_read()   read data file "household_power.csv"
##   plot1()                  create file plot1.png   Histogram
##
## File "exdata_data_household_power_consumption.zip"
## 27.11.2016
## ------------------------------------------------------------------------

setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")  

##-------------------------------------------------------------------------
## function plot1()
## task: create file plot1.png   Histogram
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file
## output   : plot1.png or screen
plot1 <- function( f_screen = FALSE ) {
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")  
  
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png(filename = "plot1.png", width = 480, height = 480)
  }  
  
  ## histogramm at screen
  hist( as.numeric(my_data$Global_active_power), 
        main = "Global Active Power",
        xlim = c(0,6),
        ylim = NULL,
        xlab = "Global Active Power (kilowatts)",
        col = "red")
  
  if ( f_screen == FALSE ) {
    ## device back to screen
    dev.off()
  }  
  
}


##-------------------------------------------------------------
## function household_power_read()
## task         read data file "household_power.csv"
## arguments:
##   arg_dir    dirctory with data files              (character)
##   na.rm      TRUE = remove NA/NaN; FALSE = other   (logical)            
## return:
##   subdata    collection of pollution data          (data frame)    
##
household_power_read <- function(arg_dir=getwd(), na.rm=FALSE) {
  
  ## directory with data file
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")  
  
  ## Dataset: Electric power consumption 
  l_filename <- "household_power.csv"
  
  l_file <- file.path(arg_dir, l_filename)
  if ( file.exists(file=l_file) == FALSE ) {
    ## error message
    stop(paste("Error: file ", l_file, " not found!"))
  } else {
    ## csv file read
    l_colClasses <- c("character", 
                      "POCXct",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric")
    
    rawdata <- read.table(l_file, 
                          header=TRUE, 
                          sep=";", 
                          dec=",",
                          stringsAsFactors = FALSE) 
    
    ## selection days
    l_date1 <- "1/2/2007"; l_date2 <- "2/2/2007"
    
    ## subset of timezone 01.02.2007 - 02.02.2007
    sdata <- subset(rawdata, Date==l_date1 | Date==l_date2)
    
    if (na.rm==TRUE) {
      ## remove Na values
      sdata <- subset(sdata, is.na(Date) == FALSE &
                        is.na(Time) == FALSE &   
                        is.na(Global_active_power) == FALSE   )
    }
    
    ## transform
    date_time <- paste(sdata$Date, sdata$Time)
    sdata <- cbind(sdata, date_time)
    
    return(sdata)
  }
}  
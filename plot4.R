## ------------------------------------------------------------------------
## R-scripts
##   household_power_read()   read data file "household_power.csv"
##   plot4()                  create file plot4.png   
##                                   with Global aktive power
##                                        Voltage
##                                        Energy sub metering
##                                        Global reactive power
##   plot2()                  create file plot2.png   Global aktive power
##   plot3()                  create file plot3.png   Energy sub metering
##   plot5()                  create file plot5.png   Voltage                           
##   plot6()                  create file plot6.png   Global reactive power
##
## File "exdata_data_household_power_consumption.zip"
## 27.11.2016
## ------------------------------------------------------------------------

setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")  

##-------------------------------------------------------------------------
## function plot4
## task; create file plot4.png   
##              with Global aktive power    (plot2.png)
##                   Voltage                (plot5.png)
##                   Energy sub metering    (plot3.png)
##                   Global reactive power  (plot6.png)
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file
## output   : plot4.png or screen
plot4 <- function( f_screen = FALSE ) { 
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")
  
  ## File Import
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png( filename = "plot4.png")
    
    ## 2 x 2 plots
    par( mfrow = c(2, 2))
    
    ## plot "Global active power" 
    plot2( f_screen = TRUE, width = 480, height = 480 )
    
    ## plot "Voltage"
    plot5( f_screen = TRUE, width = 480, height = 480 )
    
    ## plot "Energy sub metering"
    plot3( f_screen = TRUE, width = 480, height = 480 )
    
    ## plot "Global inactive power"
    plot6( f_screen = TRUE, width = 480, height = 480 )    
    
    ## device back to screen
    dev.off()
    
  } else {
    
    ## 2 x 2 plots
    par( mfrow = c(2, 2))
    
    ## plot "Global active power" 
    plot2( f_screen = f_screen, width = 480, height = 480 )
    
    ## plot "Voltage"
    plot5( f_screen = f_screen, width = 480, height = 480 )
    
    ## plot "Energy sub metering"
    plot3( f_screen = f_screen, width = 480, height = 480 )
    
    ## plot "Global inactive power"
    plot6( f_screen = f_screen, width = 480, height = 480 )    
    
  }
  
}  


##-------------------------------------------------------------------------
## function plot2()
## task: create file plot2.png   Global aktive power
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file 
##            width = 480        png file with width
##            height = 480       png file with height
## output   : plot2.png or screen
plot2 <- function( f_screen = FALSE, width = 480, height = 480 ) { 
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")
  
  ## File Import
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png( filename = "plot2.png", width = width, height = height )
  }  
  
  ## scatterplot
  with( my_data, plot( x = date_time, 
                       y = Global_active_power, 
                       type = "b", 
                       xaxt = "n",
                       cex.axis = 0.8,                          
                       ylab = "Global Active Power (kilowatts)")) 
  
  axis( side = 1, 
        at = c(my_data$date_time[1], 
               my_data$date_time[length(my_data$date_time) %/% 2+1], 
               my_data$date_time[length(my_data$date_time)]), 
        c("Thu", "Fri", "Sat"),
        cex.axis = 0.8 )
  
  lines( x = my_data$date_time, 
         y = my_data$Global_active_power, 
         type = "l",
         lty = 1, 
         lwd = 1)
  
  if ( f_screen == FALSE ) {      
    ## device back to screen
    dev.off()
  } 
  
}


##-------------------------------------------------------------------------
## function plot3()
## task: create file plot3.png   Energy sub metering
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file
##            width = 480        png file with width
##            height = 480       png file with height
## output   : plot3.png or screen

plot3 <- function( f_screen = FALSE, width = 480, height = 480 ) { 
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")
  
  ## File Import
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png( filename = "plot3.png", width = width, height = height )
  }  
  
  ## scatterplot
  with( my_data, plot( x = date_time,
                       y = Sub_metering_1, 
                       type = "l",
                       ylim = c(0, 40),                         
                       xaxt = "n",
                       cex.axis = 0.8,                          
                       ylab = "Energy sub metering") ) 
  
  axis( side = 1, 
        at = c(my_data$date_time[1], 
               my_data$date_time[length(my_data$date_time) %/% 2+1], 
               my_data$date_time[length(my_data$date_time)]), 
        c("Thu", "Fri", "Sat"),
        cex.axis = 0.8 )  
  
  legend( "topright", 
          legend = c("Sub metering 1", "Sub metering 2", "Sub metering 3"),
          fill = "white",
          border = "white",
          bty = "n",            
          col = c("black", "red", "blue"), 
          ncol = 1, 
          lwd = 1)    
  
  lines( x = my_data$date_time, 
         y = my_data$Sub_metering_1, 
         type = "l",
         col = "black",
         lty = 1, 
         lwd = 1)    
  
  lines( x = my_data$date_time, 
         y = my_data$Sub_metering_2, 
         type = "l",
         col = "red",
         lty = 1, 
         lwd = 1)        
  
  lines( x = my_data$date_time, 
         y = my_data$Sub_metering_3, 
         type = "l",
         col = "blue",
         lty = 1, 
         lwd = 1)                
  
  if ( f_screen == FALSE ) {    
    ## device back to screen
    dev.off()    
  }
  
}


##-------------------------------------------------------------------------
## function plot5()
## task: create file plot5.png   Voltage 
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file
##            width = 480        png file with width
##            height = 480       png file with height
## output   : plot5.png or screen
##
plot5 <- function( f_screen = FALSE, width = 480, height = 480 ) {
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")
  
  ## File Import
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png( filename = "plot5.png", width = width, height = height )    
  }
  
  ## scatterplot
  with( my_data, plot( x = date_time,
                       y = Voltage, 
                       type = "l",
                       xaxt = "n",
                       cex.axis = 0.8,                          
                       xlab = "datatime",
                       ylab = "Voltage (volt)") ) 
  
  axis( side = 1, 
        at = c(my_data$date_time[1], 
               my_data$date_time[length(my_data$date_time) %/% 2+1], 
               my_data$date_time[length(my_data$date_time)]), 
        c("Thu", "Fri", "Sat"),
        cex.axis = 0.8 )  
  
  lines( x = my_data$date_time, 
         y = my_data$Voltage, 
         type = "l",
         lty = 1, 
         lwd = 1)        
  
  if ( f_screen == FALSE ) {    
    ## device back to screen
    dev.off()            
  }
  
}


##-------------------------------------------------------------------------
## function plot6
## task: create file plot6.png   Global reactive power
## arguments: f_screen = TRUE    then device is screen
##            f_screen = FALSE   then device is png file
##            width = 480        png file with width
##            height = 480       png file with height
## output   : plot6.png or screen
##
plot6 <- function( f_screen = FALSE, width = 480, height = 480 ) { 
  
  setwd("C:/Users/Paul/Desktop/coursera/household_power_consumption")
  
  ## File Import
  my_data <- household_power_read( arg_dir=getwd(), na.rm=FALSE )
  
  if ( f_screen == FALSE ) {
    ## graphic device pnp
    png( filename = "plot6.png", width = width, height = height )    
  }  
  
  ## scatterplot
  with( my_data, plot( x = date_time,
                       y = Global_reactive_power, 
                       type = "l",
                       xaxt = "n",
                       cex.axis = 0.8,                         
                       xlab = "datatime",
                       ylab = "Global reactive power") ) 
  
  axis( side = 1, 
        at = c(my_data$date_time[1], 
               my_data$date_time[length(my_data$date_time) %/% 2+1], 
               my_data$date_time[length(my_data$date_time)]), 
        c("Thu", "Fri", "Sat"),
        cex.axis = 0.8 )  
  
  lines( x = my_data$date_time, 
         y = my_data$Global_reactive_power, 
         type = "l",
         cex = 0.6,           
         lty = 1, 
         lwd = 1 )
  
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
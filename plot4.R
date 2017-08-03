plot4 <-function(){
  #Read the full content from the House power consumption file
  elecpowcons <- read.table("household_power_consumption.txt", sep = ";", 
                            header = TRUE, na.strings = '?', stringsAsFactors = F, 
                            check.names = F, comment.char = "")
  #change the date style
  elecpowcons$Date <- as.Date(elecpowcons$Date, format = "%d/%m/%Y")
  #Get the data from the dates 2007-02-01 and 2007-02-02
  reqdata <- elecpowcons[elecpowcons$Date == '2007-02-01' | elecpowcons$Date == '2007-02-02',]
  #delete the original data
  rm(elecpowcons)
  
  #Add date and time by default separator
  datetime <- paste(as.Date(reqdata$Date), reqdata$Time)
  #Create Column for Date Time
  reqdata$DateTime <- as.POSIXct(datetime)
  #copy the plot to Plot4.PNG
  png("plot4.png", width= 480, height = 480)
  
  #split screen into 2 rows and 2 cols and set the margin
  par(mfrow = c(2,2), mar = c(4,4,2,1))
  
  #Plot 1
  plot(reqdata$Global_active_power ~ reqdata$DateTime, type = "l", ylab = "Global Active Power", xlab = "")
  
  #Plot 2
  plot(reqdata$Voltage ~ reqdata$DateTime, type = "l", ylab = "Voltage", xlab = "datetime")
  
  #plot 3
  plot(reqdata$Sub_metering_1 ~ reqdata$DateTime, type = "n", ylab = "Energy sub metering", xlab = "")
  #Add lines to the plot
  lines(reqdata$Sub_metering_1 ~ reqdata$DateTime, type = "l")
  lines(reqdata$Sub_metering_2 ~ reqdata$DateTime, type = "l", col = "red")
  lines(reqdata$Sub_metering_3 ~ reqdata$DateTime, type = "l", col = "blue")
  #Add legends at right top corner
  legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n", cex = 0.75, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  #plot 4
  plot(reqdata$Global_reactive_power ~ reqdata$DateTime, type = "l", ylab = "Global_reactive_power", xlab = "datetime")
  
  dev.off()
}
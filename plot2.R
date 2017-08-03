plot2 <-function(){
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
  #copy the plot to Plot2.PNG
  png("plot2.png", width= 480, height = 480)
  
  #create plot
  plot(reqdata$Global_active_power ~ reqdata$DateTime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")
  
  dev.off()
}
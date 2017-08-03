plot1 <-function(){
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
  
  #copy the plot to Plot1.PNG
  png("plot1.png", width= 480, height = 480)
  
  #create histogram
  hist(reqdata$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
  
  dev.off()
}
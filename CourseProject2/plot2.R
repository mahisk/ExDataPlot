##############################################################################
#Create plot for:
#2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
##############################################################################

plot2 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  
  #get the subset for fips == "24510"
  baltimorecityPM25 <- subset(NEI, fips == '24510')
  
  #calculate the aggregate of the Emission for all years
  emissionsagg <- aggregate(Emissions ~ year, data = baltimorecityPM25, sum)
  
  #plot should be written in png file
  png("plot2.png", height = 480, width = 480)
  
  #plot year vs Emission in Kilotons
  barplot(emissionsagg$Emissions, names.arg=emissionsagg$year, xlab='Year',
          col="red", ylab=expression(paste('PM', ''[2.5])), 
          main = expression(paste('Total PM'[2.5], ' in Baltimore City, Maryland')))
  
  dev.off()
}
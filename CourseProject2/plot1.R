##############################################################################
#Create plot for:
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission 
#from all sources for each of the years 1999, 2002, 2005, and 2008.
##############################################################################

plot1 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  
  #calculate the aggregate of the Emission for all years
  emissionsagg <- aggregate(Emissions ~ year, data = NEI, sum)
  #convert to kilotons from tons
  emissionsagg$Emissionskilotons <- round(emissionsagg[,'Emissions']/1000,2)
  
  #plot should be written in png file
  png("plot1.png", height = 480, width = 480)
  
  #plot year vs Emission in Kilotons
  barplot(emissionsagg$Emissionskilotons, names.arg=emissionsagg$year, xlab='Year',
          col="red", ylab=expression(paste('PM', ''[2.5], ' in Kilotons')), 
          main = expression('Total Emission of PM'[2.5]))
  
  dev.off()
}
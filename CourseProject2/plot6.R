##############################################################################
#Create plot for:
#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
##############################################################################

plot6 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  #read Source Classification Code Table
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #fliter codes related  motor vehicle sources.. taking vehicles from EI.Sector
  SCCcode <- SCC[grepl('vehicle', SCC$EI.Sector, ignore.case = TRUE),"SCC"]
  
  #get the subset for fips == "24510" | fips == "06037"
  PM25 <- subset(NEI, (fips == '24510' | fips == '06037') & SCC %in%SCCcode)
  
  #calculate the aggregate of the Emission by years
  emissionsagg <- aggregate(Emissions ~ year + fips, data = PM25, sum)
  
  emissionsagg$City <- ifelse(emissionsagg$fips == "06037", "Los Angeles", "Baltimore")
    
  #plot should be written in png file
  png("plot6.png", height = 480, width = 480)
  
  #plot year vs Emission based on type
  ggplot(emissionsagg, aes(year, Emissions, group=City, color=City)) + 
    geom_line() + xlab('Year') + theme_light() +
    geom_point( size=4, shape=16, fill="white") +
    ylab(expression('PM'[2.5])) + 
    ggtitle(expression(paste('Total Emission of PM'[2.5] , ' from motor vehicle in Baltimore City & LA County')))
  
  dev.off()
}
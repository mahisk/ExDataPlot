##############################################################################
#Create plot for:
#5.How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
##############################################################################

plot5 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  #read Source Classification Code Table
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #fliter codes related  motor vehicle sources.. taking vehicles from EI.Sector
  SCCcode <- SCC[grepl('vehicles', SCC$EI.Sector, ignore.case = TRUE),"SCC"]
  
  #get the subset for fips == "24510"
  baltimorecityPM25 <- subset(NEI, fips == '24510' & SCC %in%SCCcode)
  
  #calculate the aggregate of the Emission by years
  emissionsagg <- aggregate(Emissions ~ year, data = baltimorecityPM25, sum)
  
  #plot should be written in png file
  png("plot5.png", height = 480, width = 480)
  
  #plot year vs Emission based on type
  ggplot(emissionsagg, aes(year, Emissions)) + 
    geom_line() + xlab('Year') + theme_light() +
    geom_point( size=4, shape=16, fill="white", color='red') +
    ylab(expression('PM'[2.5])) + 
    ggtitle(expression(paste('Total Emission of PM'[2.5] , ' from motor vehicle sources in Baltimore City')))
  
  dev.off()
}
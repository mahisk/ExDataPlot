##############################################################################
#Create plot for:
#Of the four types of sources indicated by the type 
#(point, nonpoint, onroad, nonroad) variable, which of these four sources 
#have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
##############################################################################

plot3 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  
  #get the subset for fips == "24510"
  baltimorecityPM25 <- subset(NEI, fips == '24510')
  
  #calculate the aggregate of the Emission by years and type
  emissionsagg <- aggregate(Emissions ~ year + type, data = baltimorecityPM25, sum)
  
  #plot should be written in png file
  png("plot3.png", height = 480, width = 480)
  
  #plot year vs Emission based on type
  ggplot(emissionsagg, aes(year, Emissions, group=type, color=type)) + 
    geom_point( size=4, shape=16, fill="white") + 
    geom_line() + xlab('Year') + ylab(expression('PM'[2.5])) + 
    ggtitle(expression(paste('Total PM'[2.5], 'by Type in Baltimore City, Maryland'))) +
    theme_light() + labs(colour = "Type")
  
  dev.off()
}
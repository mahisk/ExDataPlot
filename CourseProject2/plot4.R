##############################################################################
#Create plot for:
#Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999-2008?
##############################################################################

plot4 <- function(){
  #Read PM2.5 Emissions Data 
  NEI <- readRDS("summarySCC_PM25.rds")
  #read Source Classification Code Table
  SCC <- readRDS("Source_Classification_Code.rds")
  
  #fliter codes related to coal combustion
  SCCcode <- SCC[grepl('Coal', SCC$Short.Name, ignore.case = TRUE) & 
                   grepl('comb', SCC$Short.Name, ignore.case = TRUE),"SCC"]
  
  #get the subset of emission data for coal combustion
  PM25coalcomb <- subset(NEI, SCC %in% SCCcode)
  
  #calculate the aggregate of the Emission by years
  emissionsagg <- aggregate(Emissions ~ year, data = PM25coalcomb, sum)
  #convert to kilotons from tons
  emissionsagg$Emissionskilotons <- round(emissionsagg[,'Emissions']/1000,2)
  
  #plot should be written in png file
  png("plot4.png", height = 480, width = 480)
  
  #plot year vs Emission
  ggplot(emissionsagg, aes(year, Emissionskilotons)) + 
    geom_bar(stat = "identity", fill='darkorange2') + xlab('Year') + theme_light() +
    ylab(expression(paste('PM', ''[2.5], ' in Kilotons'))) + 
    ggtitle(expression(paste('Total Emission of PM'[2.5] , ' from coal combustion-related sources'))) + 
    geom_text(aes(label=round(Emissionskilotons,1), vjust = 2))
  
  dev.off()
}
# draws a geo location of the 10 locations with the 10 highest mean price

library(ggmap)
library(ggplot2)

source("visualizations.R")


# updates geoLocs with the locations present in meanPriceLoc$locatoin
# that are not already in the location attribute of geoLocs. All the
# rows with the added location has their lat and long attribute set to NA.
updateGeoLocWithNAs <- function(geoLocs,meanPriceLoc) {
  uniques <- unique(meanPricePerLocation$location)
  
  for (unLoc in uniques) {
    if (is.na(geoLocs[unLoc,]$location)) {
      geoLocs <- rbind(geoLocs,data.frame(location=c(unLoc),
                                          lat=c(NA),
                                          long=c(NA)))
    }
  }
  return(geoLocs)
}

 # draw the geo visualisation using the mean prices per location passed as 
 # an argument
 showMapWithMeanPriceLocation <- function(meanPricePerLocation) {
   # load latitude and longitude of locations fromgeoLocations
  geoLocs <- read.csv('geoLocations.csv',
                      colClasses = c('character','numeric','numeric'))
  
  
  # link the latitude and longitude of locations with the 
  # meanPricePerLocation data
  geoLocs <- updateGeoLocWithNAs(geoLocs,meanPricePerLocation)
  meanPricePerLocation <- merge(meanPricePerLocation,geoLocs,by="location")
  meanPricePerLocation <- meanPricePerLocation[!is.na(meanPricePerLocation$lat),]
  
  # retrieve map of malta
  maltaMap <- get_map(location = "malta", maptype = "roadmap", zoom = 11)
  

  # plot the mean price of the 10 localities with the highest mean price
  # on the map
  meanPriceMap <- 
    ggmap(maltaMap) +
    geom_point(data = meanPricePerLocation, 
               
               aes(x = long, y = lat,colour=location,
                   
               size=price_euro),alpha = (0.5)) +
    ylab("latitude") + 
    xlab("longitude") +
    theme(legend.title = element_text("mean price (€)")) +
    ggtitle("The 10 localities with the highest property mean price") 
   
  return(meanPriceMap)
  
 }
 
 # calculate mean price per location
 meanPricePerLocation <- 
   calculateMeanPricePerLocation(propDetails)
 
 # show mean price per location
 showMapWithMeanPriceLocation(meanPricePerLocation)
 ggsave("geovisualisation.png")



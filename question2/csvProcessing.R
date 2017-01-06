pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
} 


propertyDetails <- read.csv("unique_features.csv",
                            header = TRUE,sep = ",",
                            fileEncoding = "UTF-8",
                            na.strings = c(""),
                            row.names = NULL)

propertyDetails$price_euro <- as.numeric(propertyDetails$price_euro)

head(propertyDetails[is.na(propertyDetails$location),],30)

propDetails <- 
   propertyDetails[!is.na(propertyDetails$location) &
                  (ifelse(is.na(propertyDetails$price_euro),1,0)+
                  ifelse(is.na(propertyDetails$property_type),1,0)+
                  ifelse(is.na(propertyDetails$area_sqm),1,0)
                  <= 1),]

numberOfProperties <- dim(propDetails)[1]

pastePrint("size of dataset:",numberOfProperties)

missingAreas <- length(propDetails$area_sqm[is.na(propDetails$area_sqm)])
pastePrint("missing area: ",missingAreas)

missingPrices <- length(propDetails$price_euro[is.na(propDetails$price_euro)])
pastePrint("missing price: ",missingPrices)

pastePrint("missing property type",length(propDetails$property_type[is.na(propDetails$property_type)]))

uniqueLocation <- unique(propDetails$location)
uniqueLocationWithDefinedPrice <- unique(propDetails[is.na(propDetails$price_euro),"location"])

#find which locations do not have a defined price
ff<-function(l) {any(l==uniqueLocationWithDefinedPrice)}
locationMentioned <- lapply(FUN=ff,uniqueLocation)
(uniqueLocation[locationsWithUndefinedPrices == FALSE])

imputePrices <- function(propDetails) {
  medianPricePerLocation <-aggregate(list(price=propDetails$price_euro),
                                     by=list(location=propDetails$location),
                                     na.rm=TRUE,FUN=median)
  
  imputedPrices <- apply(X=propDetails,1,FUN=function(propRow){
    loc <- propRow["location"]
    price <- propRow["price_euro"]
    medPriceForLoc = medianPricePerLocation$price[medianPricePerLocation$location==loc]    
    if (!is.na(medPriceForLoc) && is.na(price)) {
      
      propRow["price_euro"] <- medPriceForLoc
    }
    return (propRow["price_euro"])
  })
  
  propDetails$price_euro <-imputedPrices;
  
  return(propDetails)
}

propDetails <- imputePrices(propDetails)
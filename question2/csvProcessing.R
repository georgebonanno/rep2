# loads the extracted features from 'unque_features.csv' and performs the
# necessary imputations, outlier removals and error correction

pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
} 

# correct errors
correctErrors <- function(propDetails) {
  #  correction of price since '250m000' was interpreted
  #  as 250m rather than 250,000 EUR for townhouse in 
  #  fleur-de-lys
  propDetails[!is.na(propDetails$contact_no) &
                propDetails$contact_no=='79537626' & 
                propDetails$price_euro==2.5e8,]$price_euro <- 250e3
  
  propDetails <- propDetails[propDetails$price_euro < 1e7,]
  propDetails <- propDetails[propDetails$area_sqm < 1e6,]
  propDetails <- propDetails[!is.na(propDetails$location),]
  return(propDetails)
}

# load extracted attribute from csv
propertyDetails <- read.csv("unique_features.csv",
                            header = TRUE,sep = ",",
                            na.strings = c(""),
                            colClasses = c("character","character","numeric",
                                           "character","numeric"),
                            row.names=NULL)



# filter adverts with an undefined location
propertyDetails <- propertyDetails[!is.na(propertyDetails$location),]

propertyDetails$price_euro <- as.numeric(propertyDetails$price_euro)

# filter adverts with more than one unknown
propDetails <- 
   propertyDetails[!is.na(propertyDetails$location) &
                  (ifelse(is.na(propertyDetails$price_euro),1,0)+
                  ifelse(is.na(propertyDetails$property_type),1,0)+
                  ifelse(is.na(propertyDetails$area_sqm),1,0)
                  <= 1),]

# correct any errors
propDetails <- correctErrors(propDetails)


prices <- propDetails[!is.na(propDetails$price_euro),]

numberOfProperties <- dim(propDetails)[1]

pastePrint("size of dataset:",numberOfProperties)

missingAreas <- length(propDetails$area_sqm[is.na(propDetails$area_sqm)])
pastePrint("missing area: ",missingAreas)

missingPrices <- length(propDetails$price_euro[is.na(propDetails$price_euro)])
pastePrint("missing price: ",missingPrices)

pastePrint("missing property type",length(propDetails$property_type[is.na(propDetails$property_type)]))

uniqueLocation <- unique(propDetails$location)
uniqueLocationWithDefinedPrice <- unique(propDetails[is.na(propDetails$price_euro),"location"])


#  assumption: area of a given property type (e.g. apartment) does not
#  is almost constant in a given location
#  price imputation is taken by taken the median price in that location
imputePrices <- function(propDetails) {
  medianPricePerLocation <-aggregate(list(price=propDetails$price_euro),
                                     by=list(location=propDetails$location),
                                     na.rm=TRUE,FUN=median)
  
  imputedPrices <- apply(X=propDetails,1,FUN=function(propRow){
    loc <- propRow["location"]
    price <- propRow["price_euro"]
    propType <- propRow["property_type"]
    if (is.na(price)) {
      medPriceForLoc = medianPricePerLocation$price[medianPricePerLocation$location==loc]    
      if (!is.na(medPriceForLoc)) {
        
        propRow["price_euro"] <- as.numeric(medPriceForLoc)
      }
    }
    return (propRow["price_euro"])
  })
  
  propDetails$price_euro <-as.numeric(imputedPrices);
  return(propDetails)
}

#  area imputation is taken by taken the median area of that property type
imputeArea <- function(propDetails) {
  medianAreaForPropertyType <- aggregate(x=list(area=propDetails$area_sqm),
                                         by=list(propType=propDetails$property_type),
                                         FUN=median,na.rm=TRUE)
  
  imputeAreaF <- function(prop) {
    if (is.na(prop["area_sqm"])) {
      propType <- prop["property_type"]
      medianArea <- medianAreaForPropertyType$area[medianAreaForPropertyType$propType==propType]
      if (!is.na(medianArea)) {
        prop["area_sqm"] <- medianArea
      }
    }
    return(prop["area_sqm"])
  }
  
  propDetails$area_sqm<-as.numeric(apply(X = propDetails,1,FUN=imputeAreaF))
  
  return(propDetails)
}

propDetails <- imputePrices(propDetails)
propDetails <- imputeArea(propDetails)

# remove 'HOSTELS' since no hostel adverts has a defined price
removeWithMissingValues <- function(propDetails) {
  propDetails <- propDetails[propDetails$property_type!= 'HOSTEL',]
  
  
  
  return (propDetails)
}

# remove outliers (the ranges were determine after plotting box plot)
removeOutliers <- function(propDetails) {
  propDetails <- propDetails[propDetails$price_euro >= 10000,]
  propDetails <- propDetails[propDetails$price_euro <= 3e6,]
  propDetails <- propDetails[propDetails$area_sqm <= 500,]
  propDetails <- propDetails[propDetails$area_sqm <= 1.5e5,]
  
  return(propDetails)
}


propDetails <- removeWithMissingValues(propDetails)
propDetails <- removeOutliers(propDetails)

head(propDetails[order(-propDetails$area_sqm),],50)


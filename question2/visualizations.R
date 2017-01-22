library(ggplot2)
library(reshape2)

# this script builds the visualations using the extrated features 
# from the adverts (propDetails)

# load extrated features from adverts
source('csvProcessing.R')


# show a box plot with the price per property type
box <- ggplot(data=propDetails, aes(x=property_type, y=price_euro))
box + geom_boxplot(aes(fill=property_type)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Price of Property (€)") + 
  xlab("property type") +
  ggtitle("Property Type - Price Boxplot")

ggsave("priceBoxPlot.png")

# show a box plot with the area per property type
box <- ggplot(data=propDetails, aes(x=property_type, y=area_sqm))
box + geom_boxplot(aes(fill=property_type)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("area/sqm") + 
  xlab("property type") +
  ggtitle("Property type - Area Boxplot")

ggsave("areaBoxPlot.png")


# extract the count of different property counts and returns the counts
# ordered by largest property type count first
propertyTypeCount <- function() {
  propDetails["count"] <- 1
  propertyTypeCounts <- 
    aggregate(x = list(property_count = propDetails$count),
            by=list(property_type=factor(propDetails$property_type)),FUN=sum)
  
  propertyTypeCounts <- 
    propertyTypeCounts[order(-propertyTypeCounts$property_count),]
  
  
  ggplot(data=propertyTypeCounts, 
         aes(x=reorder(propertyTypeCounts$property_type,
                       propertyTypeCounts$property_type,
                       function(x) {
                         -propertyTypeCounts[propertyTypeCounts$property_type==x,]$property_count
                       }),
             y=propertyTypeCounts$property_count)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            ggtitle("property type count") +
            labs(x="property type",y="count")
  
  return(propertyTypeCounts)
}

propertyTypeCounts <- propertyTypeCount()
# extract the 5 most common property types
mostPopularLand <- as.character(propertyTypeCounts$property_type[1:5])

# plot the price of the 5 most common properties
ggplot(subset(propDetails, property_type %in% mostPopularLand),
       aes(x=price_euro,color=property_type,fill=property_type))+
       geom_histogram(binwidth = 10000) + facet_grid(. ~ property_type) +
       theme_classic() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       xlab("price (€)")+
       ylab("number of properties for sale") + 
       ggtitle("property type for sale")
ggsave("priceMostCommonProperty.png")


# plot the area of the 5 most common properties
ggplot(subset(propDetails, property_type %in% mostPopularLand),
       aes(x=area_sqm,color=property_type,fill=property_type)) + #xlim(0,500) +
       theme_classic() +
       geom_histogram(binwidth = 50)  + facet_grid(. ~ property_type) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       xlab("area (sqm)")+
       ylab("number of properties for sale") + 
       ggtitle("Property Area Count distribution for the 5 most common property types")
ggsave("areaMostCommonProperty.png")

# filter only the most common property type adverts
mostCommonPropDetails <- propDetails[propDetails$property_type %in% mostPopularLand,]


# plot a histogram of the prices of different property types
# with a bin width of 10000 euros.
ggplot(mostCommonPropDetails,
       aes(x=price_euro,color=property_type))+
       geom_histogram(binwidth = 10000) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       facet_grid(~property_type)

# plot a histogram of the areas of different property types
# with a bin width of 50 sqm.
ggplot(mostCommonPropDetails,
       aes(x=area_sqm,color=property_type))+
       geom_histogram(binwidth = 50)  + xlim(0,500) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       facet_grid(~property_type)

# calculates the mean prices per location and returns the
# highest 20
calculateMeanPricePerLocation <-  function(propDetails) {
  meanPricePerLocation <-
    aggregate(x = list(price_euro = propDetails$price_euro),
              by=list(location=factor(propDetails$location)),
              FUN=mean)
  
  meanPricePerLocation <-
    head(meanPricePerLocation[order(-meanPricePerLocation$price_euro),],20)
  
  return(meanPricePerLocation)
}

# plots the mean price per location
showMeanPricePerLocation <- function(propDetails) {
  meanPricePerLocation <- calculateMeanPricePerLocation(propDetails)
  print("mean price")
  print(meanPricePerLocation)
  
  p <- ggplot(data=meanPricePerLocation, 
         aes(x=reorder(meanPricePerLocation$location,
                       meanPricePerLocation$location,
                       function(x) {
                         -meanPricePerLocation[meanPricePerLocation$location==x,]$price_euro
                       }),
             y=meanPricePerLocation$price_euro)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("mean price (euro) per location") +
    labs(x="location",y="mean price (euro)")
  
  return(p)
  
}

meanPricePerLocation <- showMeanPricePerLocation(propDetails)


# extract the 10 most popular location for every property
findPopularLocationsPerProperty <- function(propDetails) {
  propDetails <- propDetails[propDetails$property_type %in% mostPopularLand,]
  mostPopLocations <- 
    aggregate(x=list(cnt = propDetails$location),
              by=list(location = propDetails$location,
                      property_type=propDetails$property_type),FUN=length)
  
  first10 <- function(propertyType) {
    propWithType <- mostPopLocations[mostPopLocations$property_type == propertyType,]
    propWithType <- propWithType[order(-propWithType$cnt)[1:10],]
    return(propWithType)
  }
  
  mostPopularLocationsForProperty <-lapply(X = mostPopularLand,FUN=first10)
  
  return (do.call(rbind,mostPopularLocationsForProperty))
  
}

popularLocationsPerProperty <- 
  findPopularLocationsPerProperty(propDetails)

# plot the 10 hights locations for every property with their 
# count
ggplot(popularLocationsPerProperty,
       aes(x=reorder(popularLocationsPerProperty$location,
                     -popularLocationsPerProperty$cnt),
           y=cnt,
           color=property_type))+
      geom_bar(stat="identity",aes(fill=property_type)) +
      theme_classic() +
      facet_grid(. ~ property_type,scales="free_x") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("number of properties in location") + 
      xlab("locations") +
      ggtitle("The 10 most popular locations of the 5 most common property types") 


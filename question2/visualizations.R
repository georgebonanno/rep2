library(ggplot2)
library(reshape2)

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
mostPopularLand <- as.character(propertyTypeCounts$property_type[1:5])

ggplot(subset(propDetails, property_type %in% mostPopularLand),
       aes(x=price_euro,color=property_type))+
       geom_histogram(binwidth = 10000) + 
       xlim(0,2e6) + facet_grid(. ~ property_type) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       ylab("number of properties for sale") + 
       ggtitle("property type for sale") 

ggplot(subset(propDetails, property_type %in% mostPopularLand),
       aes(x=area_sqm,color=property_type)) + xlim(0,500) +
       geom_histogram(binwidth = 50)  + facet_grid(. ~ property_type)

propertyTypeCount()
#ggplot(p)

mostCommonPropDetails <- propDetails[propDetails$property_type %in% mostPopularLand,]

ggplot(mostCommonPropDetails,
       aes(x=price_euro,color=property_type))+
       geom_histogram(binwidth = 10000) + xlim(0,2e6) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       facet_grid(~property_type)

ggplot(mostCommonPropDetails,
       aes(x=area_sqm,color=property_type))+
       geom_histogram(binwidth = 50)  + xlim(0,500) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       facet_grid(~property_type)

calculateMeanPricePerLocation <-  function(propDetails) {
  meanPricePerLocation <-
    aggregate(x = list(price_euro = propDetails$price_euro),
              by=list(location=factor(propDetails$location)),
              FUN=mean)
  
  meanPricePerLocation <-
    head(meanPricePerLocation[order(-meanPricePerLocation$price_euro),],20)
  
  return(meanPricePerLocation)
}

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

box <- ggplot(data=propDetails, aes(x=property_type, y=price_euro))
box + geom_boxplot(aes(fill=property_type)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Price of Property") + 
  ggtitle("Price per Property Boxplot")

box <- ggplot(data=propDetails, aes(x=property_type, y=area_sqm))
box + geom_boxplot(aes(fill=property_type)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("area/sqm") + 
  ggtitle("Price per Property Boxplot")

box <- ggplot(data=propDetails, aes(x=property_type, y=area_sqm))
box + geom_boxplot(aes(fill=property_type)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(0,2.5e4)+
  ylab("Price of Property") + 
  ggtitle("Price per Property Boxplot") 

ggplot(propDetails, aes(x=property_type)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat="count") 

#propertyPrices <- melt(propDetails,id="property_type")

#ggplot(data=propertyPrices,
#       aes(x=))
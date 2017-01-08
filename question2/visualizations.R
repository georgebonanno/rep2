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
  
  return(p)
}

p <- propertyTypeCount()
#ggplot(p)

#propertyPrices <- melt(propDetails,id="property_type")

#ggplot(data=propertyPrices,
#       aes(x=))
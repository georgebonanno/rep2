pastePrint <- function(...,sepr=" ") {
  print(paste(...,sep=sepr))
} 


propertyDetails <- read.csv("unique_features.csv",
                            header = TRUE,sep = ",",
                            fileEncoding = "UTF-8",
                            na.strings = c(""),
                            row.names = NULL)


head(propertyDetails[is.na(propertyDetails$location),],30)

#propertyDetails$price_euro <- as.numeric(propertyDetails$price_euro)

naCount <- function(...) {
  params <- list(...)
  markNas <- function(prev,x) {
    if (length(x) > 1) {
      print(paste("warning: ",x))
    }
    
    if (is.na(x)) {
      n=1
    } else {
      n=0
    }
    
    return(n+prev)
  }
  Reduce(markNas,params,init=0)
}

propDetails <- 
   propertyDetails[!is.na(propertyDetails$location) &
                  (ifelse(is.na(propertyDetails$price_euro),1,0)+
                  ifelse(is.na(propertyDetails$property_type),1,0)+
                  ifelse(is.na(propertyDetails$area_sqm),1,0)
                  <= 1),]


pastePrint("missing area: ",length(propDetails$area_sqm[is.na(propDetails$area_sqm)]))
pastePrint("missing price: ",length(propDetails$area_sqm[is.na(propDetails$price_euro)]))
pastePrint("missing property type",length(propDetails$property_type[is.na(propDetails$property_type)]))
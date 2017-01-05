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

oneUnknown <- propertyDetails[ifelse(is.na(propertyDetails$location),1,0)+
                  ifelse(is.na(propertyDetails$price_euro),1,0)+
                  ifelse(is.na(propertyDetails$property_type),1,0)+
                  ifelse(is.na(propertyDetails$area_sqm),1,0)
                  <= 1,]

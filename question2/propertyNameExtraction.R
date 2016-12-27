library(stringi)
library(stringr)


propertyDescriptions <- c("apartment",
                          "townhouse",
                          "farmhouse",
                          "penthouse",
                          "townhouse",
                          "house",
                          "shop",
			                    "palazzo",
                          "maisonette",
                          "villa",
                          "garage",
                          "plot",
                          "flat",
                          "bungalow",
                          "office",
			                    "site",
			                    "\\sland",
			                    "airspace",
			                    "palazzino",
			                    "field",
			                    "restaurant",
			                    "hostel",
			                    "garage",
			                    "catering",
			                    "rent"
			                    )


extractPropertyDescription <- function(line) {
  words <- strsplit(line,"[\\s,\\.]",perl=TRUE)
  
  isPropertyWord <- function(locDesc) {
    grepProperties <- function(prop) {
      matches <- (grepl(locDesc,prop,ignore.case = TRUE,perl=TRUE))
      if (matches && grepl(locDesc,"^villa$",ignore.case = TRUE)) {
        matches <- !(grepl("village",prop,ignore.case=TRUE));
      } 
      return(matches)
      
    }
    
    matches <- lapply(FUN = grepProperties,X = words[[1]])
    propIndex <- match(TRUE,matches == TRUE)
    
    propertyDesc <- NA
    if (!is.na(propIndex)) {
      propertyDesc <- propertyDescriptions[propIndex]
    }
    
    return(propertyDesc);
  }
  
  matchOutcome <- sapply(FUN=isPropertyWord,X=propertyDescriptions)
  
  firstMatchIndex <-match(FALSE,is.na(matchOutcome))
  if (is.na(firstMatchIndex)) {
    propertyDesc <- NA
  } else {
    propertyDesc <- propertyDescriptions[firstMatchIndex]
  }
  
  #  default to apartment is none of the words
  #  were found
  if (is.na(propertyDesc)) {
    propertyDesc <- "apartment" 
  }
  return(str_to_upper(propertyDesc))
}

extractPropertyDescription("VICTORIA GARDENS. New on the market. Fully detached bungalow on .11ha (1 tumolo) having four bedrooms with en-suite bathrooms, large hall, sitting / dining, kitchen / breakfast, cinema / gym, three garages, surrounding garden with large pool and showers. Freehold. €1,600,000. Phone owner 9949 7924.")

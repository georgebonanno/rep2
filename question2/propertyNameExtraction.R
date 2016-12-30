library(stringi)
library(stringr)


propertyDescriptions <- c("apartment",
                          "townhouse",
                          "farmhouse",
                          "Farm",
                          "penthouse",
                          "townhouse",
                          "house",
                          "shop",
			                    "palazzo",
                          "maisonette",
                          "villa",
			                    "bungalow",
			                    "garage",
                          "plot",
                          "flat",
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
			                    "bar",
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

extractPropertyDescription("KENNEDY GROVE AREA. Garage space. Remote controlled gate on main road. Direct from owner. &euro;8,900, negotiable. Phone 2134 5529.")

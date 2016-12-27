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
isPropertyWord <- function(word) {
  grepProperties <- function(prop) {
    matches <- (grepl(prop,word,ignore.case = TRUE,perl=TRUE))
    if (matches && grepl(prop,"^villa$",ignore.case = TRUE)) {
      matches <- !(grepl("village",word,ignore.case=TRUE));
    } 
    return(matches)
              
  }
  
  matches <- lapply(FUN = grepProperties,X = propertyDescriptions)
  propIndex <- match(TRUE,matches == TRUE)
  
  propertyDesc <- NA
  if (!is.na(propIndex)) {
    propertyDesc <- propertyDescriptions[[propIndex]]
  }
  
  return(propertyDesc);
}

extractPropertyDescription <- function(line) {
  words <- strsplit(line,"[\\s,\\.]",perl=TRUE)
  matchOutcome <- sapply(FUN=isPropertyWord,X=words[[1]])
  
  firstMatchIndex <-match(FALSE,is.na(matchOutcome))
  if (is.na(firstMatchIndex)) {
    propertyDesc <- NA
  } else {
    propertyDesc <- words[[1]][firstMatchIndex]
  }
  
  #  default to apartment is none of the words
  #  were found
  if (is.na(propertyDesc)) {
    propertyDesc <- "apartment" 
  }
  return(str_to_upper(propertyDesc))
}

#isPropertyWord('penthouse')

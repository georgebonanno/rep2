library(stringi)
library(stringr)


propertyDescriptions <- c("apartment",
                          "penthouse",
                          "townhouse",
                          "house",
			  "palazzo",
                          "maisonette",
                          "villa",
                          "garage",
                          "farmhouse",
                          "plot",
                          "flat",
                          "form",
                          "basement",
                          "bungalow",
                          "garage",
                          "with well",
                          "office")
isPropertyWord <- function(word) {
  grepProperties <- function(prop) {
    return (grepl(prop,word,ignore.case = TRUE))
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
  
  return(propertyDesc)
}

#isPropertyWord('penthouse')

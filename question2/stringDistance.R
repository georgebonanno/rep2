library(stringdist)

# functions that allow the string distance to be computed between a given
# string and the valid location (stored in places.txt)

places <- scan("places.txt",what="",sep="\n")

placeAsPrefix <- function(placeDescription) {
  placesPrefixes <- (places[startsWith(placeDescription,places)])
  placesPrefixes <- placesPrefixes[order(-str_length(placesPrefixes))];
  #get longest prefix
  if (length(placesPrefixes) > 0) {
    placeAsPrefix <- placesPrefixes[1]
  } else {
    placeAsPrefix <- c()
  }
  
  return(placeAsPrefix)
}

findCorrectPlace <- function(extractedPlace) {
  correctedPlace <- placeAsPrefix(extractedPlace)
  if (length(correctedPlace) == 0) {
    mostNear <- amatch(c(extractedPlace),places,maxDist = 2)
    
    if (!is.na(mostNear) && length(mostNear) > 0) {
      mostNear <- mostNear[1]
      correctedPlace <- places[mostNear]
    } else {
      correctedPlace <- ""
    }  
  }
  
  return(correctedPlace)
}

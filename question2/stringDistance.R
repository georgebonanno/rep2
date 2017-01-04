library(stringdist)

places <- scan("places.txt",what="",sep="\n")

placeAsPrefix <- function(placeDescription) {
  return(places[startsWith(placeDescription,places)])
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

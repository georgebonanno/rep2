library(stringr)
library(stringi)

pastePrint <- function(...) {
  print(paste(...,sep = " "))
}


extractFeatures <- function(line) {
  if (!grepl("PROPERTIES for sale on Malta's best rated property website:",line)) {
    location <- gsub(" | w([^\\.]+)\\..*","\\1",line,perl=TRUE)
    phone <- gsub(".*([0-9]{4} *[0-9]{4}).*","\\1",text)
    
    pastePrint("location:",location);
    pastePrint("phone: ",phone);
    print(location)
  }
}

extractFeatures('GOZO, GĦAJNSIELEM. An ideal residential plot of land for investment or for a good sized terraced house, situated in a three story-height zone. €185,000. Phone 9947 6959.')
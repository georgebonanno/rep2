library(stringr)

locationMappings <- list(
  "ŻEBBUĠ"="Ħaż-Żebbuġ",
  "BAHAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "COSPICUA (Bormla)"="Bormla",
  "FORT CAMBRIDGE"="FORT CAMBRIDGE",
  "GĦochARGĦochUR"="Għargur",
  "SAN PAWL TAT-TARĠA"="SAN PAWL TAT-TARĠA",
  "SANTA LUĊIJA"="SANTA LUĊIJA",
  "SANTA MARIA ESTATE"="MELLIEHA",
  "ST PAUL' S BAY"="ST PAUL' S BAY",
  "ST VENERA"="ST VENERA",
  "TA' XBIEX"="TA' XBIEX",
  "THE STRAND Gżira"="Gżira",
  "THE VILLAGE"="THE VILLAGE",
  "VICTORIA GARDENS"="VICTORIA GARDENS",
  "VITTORIOSA (Birgu) St Angelo Mansions"="Birgu",
  "XEMXIJA"="XEMXIJA",
  "XGĦAJRA"="XGĦAJRA"
)

resolveLocation <- function(extractedLocation) {
  extractedLocation <- toupper(extractedLocation)
  locations <- strsplit(extractedLocation,' */ *',perl=TRUE)
  
  #  only one string given to strsplit. We can assign
  #  the split of the first (and only entry) to split
  locations<-locations[[1]]
  multiplePlaces <- (length(locations) > 1) 
  
  if (multiplePlaces) {
    location <- lapply(X = locations,FUN = resolveLocation)
  } else {
    gozoPlace <- (gsub("(GOZO)[ ,]*.*","\\1",extractedLocation))
    if (extractedLocation != gozoPlace) {
      location <- gozoPlace
    } else {
      location <- locationMappings[[extractedLocation]];
      if (is.null(location)) {
        location <- gsub("([^ ]+) *.*","\\1",perl=TRUE,extractedLocation)
      }
    } 
    location <- list(location)
  }
  return(location)
}
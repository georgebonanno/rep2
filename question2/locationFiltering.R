library(stringr)

locationMappings <- list(
  "ŻEBBUĠ"="ĦAŻ-ŻEBBUĠ",
  "BAHAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "COSPICUA (Bormla)"="Bormla",
  "BAĦAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "FORT CAMBRIDGE"="FORT CAMBRIDGE",
  "GĦOCHARGĦOCHUR"="GĦARGUR",
  "TA' GIORNI"="SAN GILJAN",
  "TA' PARIS"="TA' PARIS",
  "THE STRAND"="GŻIRA",
  "THE VILLAGE"="ST JULIANS",
  "SANTA MARIA ESTATE"="SANTA MARIA ESTATE",
  "GĦARGĦUR"="GĦARGUR",
  "VITTORIOSA (BIRGU)"="VITTORIOSA",
  "SAN PAWL TAT-TARĠA"="SAN PAWL TAT-TARĠA",
  "SANTA LUĊIJA"="SANTA LUĊIJA",
  "SANTA MARIA ESTATE"="MELLIEHA",
  "ST PAUL' S BAY"="SAN PAWL",
  "SAN ĠWANN"="SAN ĠWANN",
  "ST VENERA"="ST VENERA",
  "ST JULIANS"="ST JULIANS",
  "TA' XBIEX"="TA' XBIEX",
  "THE STRAND Gżira"="Gżira",
  "THE VILLAGE"="THE VILLAGE",
  "VICTORIA GARDENS"="VICTORIA GARDENS",
  "VITTORIOSA (BIRGU) St Angelo Mansions"="VITTORIOSA",
  "XEMXIJA"="XEMXIJA",
  "XGĦAJRA"="XGĦAJRA"
)

extractExactLocation <- function(locationWithComma) {
  if (grepl(',',locationWithComma)) {
    exactLocation <- (gsub("([^,]+)[ ,]*.*","\\1",locationWithComma))
  } else {
    exactLocation=NA;
  }
  
  return(exactLocation)
}

extractLocationWithPrefix <- function(loc) {
  maltesePrefixes <- c("TA'","SAN","THE")
  
  startsWithF <- function(prefix) {
    return(startsWith(loc,prefix))
  }
  
  if (any(sapply(FUN = startsWithF,X = maltesePrefixes))) {
    locationWithPrefix <- gsub("([^ ]+)  *([^ ]+) *.*","\\1 \\2",loc)
  } else {
    locationWithPrefix = NA
  }
  
  return(locationWithPrefix)
}


resolveLocation <- function(extractedLocation) {
  extractedLocation <- toupper(extractedLocation)
  extractedLocation <- gsub("^THE ","THE_",extractedLocation)
  locations <- strsplit(extractedLocation,' */ *',perl=TRUE)
  
  #  only one string given to strsplit. We can assign
  #  the split of the first (and only entry) to split
  locations<-locations[[1]]
  locations[1] <- gsub("^THE_","THE ",locations[1])
  extractedLocation <- gsub("^THE_","THE ",extractedLocation)
  #print("change locatoins")
  #print(locations[1])
  
  multiplePlaces <- (length(locations) > 1) 
  #print(multiplePlaces)
  if (multiplePlaces) {
    
    location <- lapply(X = locations,FUN = resolveLocation)
  } else {
    exactLocation <- extractExactLocation(extractedLocation)
    if (is.na(exactLocation)) {
      location=NA
    } else {
      if (grepl(" \\(",exactLocation)) {
        location <- NA
        extractedLocation <- exactLocation
      } else {
        location <- exactLocation
      }
    } 
    if (is.na(location)) {
        location <- locationMappings[[extractedLocation]];
        if (is.null(location)) {
          location <- extractLocationWithPrefix(extractedLocation)
          if (is.na(location)) {
            location <- gsub("([^ ]+) *.*","\\1",perl=TRUE,extractedLocation)
        }  
      }
    } 
    location <- list(location)
  }
  location <- location[grepl("^[A-Za-zŻ]",location) 
                       & location!= 'APARTMENTS'
                       & location != 'AMAZING'
                       & location != 'ADJACENT'
                       & location != 'AFFORDABLE']
  
  location[location == "ALLETTA"] <- "VALLETTA"
  return(location)
}
library(stringr)

locationMappings <- list(
  "ŻEBBUĠ"="ĦAŻ-ŻEBBUĠ",
  "BAHAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "BAĦAR IĊ-ĊAGHAQ"="BAHAR IĊ-ĊAGĦAQ",
  "BAHAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "BAĦAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "BAĦAR IĊĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "BAHAR IĊ-ĊAGĦAQ"="BAĦAR IĊ-ĊAGĦAQ",
  "COSPICUA (Bormla)"="BORMLA",
  "BAĦAR IĊ-ĊAGĦAQ"="BAHAR IĊ-ĊAGĦAQ",
  "FORT CAMBRIDGE"="FORT CAMBRIDGE",
  "FORTCAMBRIDGE"="FORT CAMBRIDGE",
  "GHADIRA"="GĦADIRA",
  "FORT"="FORT CAMBRIDGE",
  "GĦOCHARGĦOCHUR"="GĦARGUR",
  "BIROCHŻEBBUĠA"="BIRŻEBBUĠA",
  "TA' GIORNI"="SAN GILJAN",
  "TA' PARIS"="TA' PARIS",
  "THE STRAND"="GŻIRA",
  "THE VILLAGE"="ST JULIANS",
  "SANTA MARIA ESTATE"="SANTA MARIA ESTATE",
  "GĦARGĦUR"="GĦARGUR",
  "VITTORIOSA (BIRGU)"="VITTORIOSA",
  "SAN PAWL TAT-TARĠA"="SAN PAWL TAT-TARĠA",
  "SANTA LUĊIJA"="SANTA LUĊIJA",
  "TIGNÉ POINT"="TIGNÉ",
  "TOWER ROAD"="SLIEMA",
  "TIGNÉOCH"="TIGNÉ",
  "TIGNÈ"="TIGNÉ",
  "SANTA MARIA ESTATE"="MELLIEHA",
  "ST PAUL' S BAY"="SAN PAWL",
  "SAN ĠWANN"="SAN ĠWANN",
  "URMARRAD"="BURMARRAD",
  "ST VENERA"="ST VENERA",
  "ST JULIANS"="ST JULIANS",
  "BAOCHĦAR IĊ-ĊAGOCHĦAQ"="BAĦAR IĊ-ĊAGĦAQ",
  "BAOCHĦAR"="BAĦAR IĊ-ĊAGĦAQ",
  "BAOCHĦRIJA"="BAĦRIJA",
  "TA' XBIEX"="TA' XBIEX",
  "THE STRAND Gżira"="Gżira",
  "THE VILLAGE"="THE VILLAGE",
  "VICTORIA GARDENS"="VICTORIA GARDENS",
  "VITTORIOSA (BIRGU) St Angelo Mansions"="VITTORIOSA",
  "XEMXIJA"="XEMXIJA",
  "XGĦAJRA"="XGĦAJRA",
  "ŻEBBUG"="ŻEBBUĠ",
  "XGOCHĦAJRA"="XGĦAJRA",
  "BIROCHŻEBBUĠA"="BIRŻEBBUĠA",
  "ŻEBBIEGOCHĦ"="ŻEBBIEGĦ",
  "FAWWARA"="SIGGIEWI",
  "FLEUR DE-LYS"="FLEUR-DE-LYS",
  "GHARGHUR"="GĦARGUR",
  "GĦARGHUR"="GĦARGUR",
  "GHARGĦUR"="GĦARGUR",
  "GHAXAQ"="GĦAXAQ",
  "GIANPULA"="RABAT",
  "GOLDEN"="GOLDEN SANDS",
  "GĦAJN"="GĦAJN TUFFIEĦA",
  "GZIRA"="GŻIRA",
  "HAMRUN"="ĦAMRUN",
  "ĦARGOCHĦUR"="GĦARGUR",
  "ĦADIRA"="GĦADIRA",
  "ĦAXAQ"="GĦAXAQ",
  "ĦAŻ-ŻEBBUĠ"="ŻEBBUĠ",
  "IRKIRKARA"="BIRKIRKARA",
  "LANDRIJET"="LANDRIJIET",
  "LIEMA"="SLIEMA",
  "MARSACALA"="MARSASCALA",
  "MELLIEOCHĦA"="MELLIEĦĦA",
  "MĠARR AND NORTH"="MĠARR",
  "MIŻIEB"="MELLIEĦĦA",
  "MONTEROSA"="SAN GWANN",
  "MONTE ROSA GARDENS"="SAN GWANN",
  "MONTEROSA GARDENS"="SAN GWANN",
  "MONTEROSA GARDENS AND SAN PAWL TAT-TARĠA"="SAN GWANN",
  "MRIEHEL"="MRIEĦEL",
  "MĠRR"="MĠARR",
  "MĠARR AND NORTH"="MĠARR",
  "MUNXAR"="MARSASCALA",
  "MENSIJA"="SAN GWANN"
)

incorrectLocs <- c(
  'APARTMENT',
  'AMAZING',
  'ADJACENT',
  'AFFORDABLE',
  "UNCONVERTED",
  "UNDER",
  "TERRACED HOUSE",
  "TWO",
  "BOUTIQUE",
  "A",
  "WWW",
  "AN",
  "WINE"
)

correctLocation <- function(location) {
  correctedLoc <- locationMappings[[location]];
  if (!is.null(correctedLoc)) {
    location <- correctedLoc;
  }
  return(location)
}

extractExactLocation <- function(locationWithComma) {
  if (grepl(',',locationWithComma)) {
    exactLocation <- (gsub("([^,]+)[ ,]*.*","\\1",locationWithComma))
    exactLocation<-correctLocation(exactLocation)   
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
    
    locationWithPrefix<-correctLocation(locationWithPrefix)  
  } else {
    locationWithPrefix = NA
  }
  
  return(locationWithPrefix)
}

nameWithArticle <- function(extractLocation) {
  if (grepl("^([^ ]+ +[A-ZĠŻĦ][A-ZĠŻĦ]*-[^ ]+)$",extractLocation)) {
    location=extractLocation;
  } else {
    location <- gsub("([^ ]+ +[A-ZĠŻĦ][A-ZĠŻĦ]*-[^ ]+).*","\\1",extractLocation)
    if (location == extractLocation ){
      if (grepl("\\(",extractLocation)) {
        location <- gsub("([^ ]+) *.*","\\1",perl=TRUE,extractLocation)
      } else {
        location <- extractLocation
      }
    }
    
    location <- correctLocation(location)
    return(location)
  }
}

#removes words starting with numbers. Example:
#"BUNGALOW 1100SQM" becomes "BUNGALOW"
filterWordsStartingWithNumbers <- function(loc) {
  words <- strsplit(loc," +")[[1]]
  words <- words[!grepl("^[0-9]",words)]
  return (paste(words,collapse = " "))
}

isIncorrectLocation <- function(l) {
  return(any(sapply(X = incorrectLocs,
             FUN = function(pat) {
               grepl(l,pattern=paste("^",pat,sep=""))
             })))
}

resolveLocation <- function(extractedLocation) {
  
  extractedLocation <- toupper(extractedLocation)
  extractedLocation <- gsub("^THE ","THE_",extractedLocation)
  locations <- strsplit(extractedLocation,' */ *',perl=TRUE)
  
  #  only one string given to strsplit. We can assign
  #  the split of the first (and only entry) to split
  locations<-locations[[1]]
  locations <- filterWordsStartingWithNumbers(locations)
  locations[1] <- gsub("^THE_","THE ",locations[1])
  extractedLocation <- gsub("^THE_","THE ",extractedLocation)
  #print("change locatoins")
  #print(locations[1])
  
  locationFound <- (length(locations) > 0) 
  #print(multiplePlaces)
  if (locationFound) {
    extractedLocation <- locations[1]
  }  
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
  
  pastePrint("loc=",location)
  if (is.na(location)) {
      
      location <- locationMappings[[extractedLocation]];
      if (is.null(location)) {
        location <- extractLocationWithPrefix(extractedLocation)
        if (is.na(location)) {
          location <- nameWithArticle(extractedLocation)
      }  
    }
  } 
  
  if (!grepl("^[A-Za-zŻĦ]",location) ||
        isIncorrectLocation(location)) {
    location <- ""
  }
  
  location[location == "ALLETTA"] <- "VALLETTA"
  #locations startings with "A" or "AN" invalid
  return(location)
}
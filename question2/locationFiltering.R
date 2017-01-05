library(stringr)

source('stringDistance.R')

locationMappings <- list(
  "ŻEBBUĠ"="ĦAŻ-ŻEBBUĠ",
  "KENNEDY GROVE AREA"="ST PAUL'S BAY",
  "COSPICUA (Bormla)"="COTTONERA",
  "THREE CITIES"="COTTONERA",
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
  "VITTORIOSA \\(BIRGU\\)"="VITTORIOSA",
  "SAN PAWL TAT-TARĠA"="SAN PAWL TAT-TARĠA",
  "SANTA LUĊIJA"="SANTA LUĊIJA",
  "TIGNÉ POINT"="TIGNÉ",
  "TOWER ROAD"="SLIEMA",
  "TIGNÉOCH"="TIGNÉ",
  "TIGNÈ"="TIGNÉ",
  "SANTA MARIA ESTATE"="MELLIEHA",
  "IL-QORTIN"="MELLIEHA",
  "ST PAUL' S BAY"="SAN PAWL",
  "SAN ĠWANN"="SAN ĠWANN",
  "ST VENERA"="ST VENERA",
  "ST JULIANS"="ST JULIANS",
  "BAOCHĦAR IĊ-ĊAGOCHĦAQ"="BAĦAR IĊ-ĊAGĦAQ",
  "BAOCHĦAR"="BAĦAR IĊ-ĊAGĦAQ",
  "BAOCHĦRIJA"="BAĦRIJA",
  "TA' XBIEX"="TA' XBIEX",
  "THE STRAND Gżira"="Gżira",
  "THE VILLAGE"="THE VILLAGE",
  "VICTORIA GARDENS"="VICTORIA GARDENS",
  "VITTORIOSA \\(BIRGU\\) St Angelo Mansions"="VITTORIOSA",
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
  "MELLIEOCHĦA"="MELLIEĦA",
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
  "MENSIJA"="SAN GWANN",
  "LANDRIJIET"="RABAT",
  "MARSASCALA BELLA VISTA MAISONETTE APARTMENTS"="MARSASCALA",
  "MATER"="TAL-QROQQ",
  "MĠARR AND NORTH"="MĠARR",
  "ŻEJTUN OR FGURA"="ŻEJTUN",
  "BAĦAR IĊ- ĊAGĦAQ"="BAĦAR IĊ-ĊAGĦAQ",
  "BAHAR IĊ-ĊAGĦAQ"="BAĦAR IĊ-ĊAGĦAQ",
  "BIDNI"="BIDNIJA",
  "GĦADIRA"="MELLIEĦA",
  "MELLIEHA"="MELLIEĦA",
  "MISTRA VILLAGE"="MELLIEĦA",
  "NEAR STRAND"="SLIEMA",
  "NEAR TIGNÉ POINT"="SLIEMA",
  "OSPICUA"="COTTONERA",
  "OSTA"="MOSTA",
  "OSPICUA"="COTTONERA",
  "SPINOLA"="ST JULIANS",
  "OSTA"="MOSTA",
  "PENDERGARDENS"="PACEVILLE",
  "PENDER GARDENS"="PACEVILLE",
  "PIETÁ"="PIETÀ",
  "PIETÀOCH"="PIETÀ",
  "THE POINT"="TIGNÉ",
  "THE LAGUNA"="PORTOMASO",
  "THREE CITIES"="COTTONERA",
  "THE LAGUNA"="PORTOMASO",
  "THE GARDENS"="ST JULIANS",
  "QUI-SI-SANA"="SLIEMA",
  "SALIB TAL-GĦOLJA"="SIGGIEWI",
  "SAN GWANN"="SAN ĠWANN",
  "SAVOY GARDENS"="GŻIRA",
  "FILFLA VIEWS"="DINGLI",
  "SEAFRONT"="SLIEMA",
  "SENGLEA\\(L-ISLA\\)"="COTTONERA",
  "SIGGIEWI"="SIĠĠIEWI",
  "ST PAUL’ S"="ST PAUL’S BAY",
  "ST PAUL’S"="ST PAUL’S BAY",
  "ST PAUL`S BAY"="ST PAUL’S BAY",
  "ST PAULS BAY"="ST PAUL’S BAY",
  "ST PETER' S"="ZABBAR",
  "ST AUL' S BAY"="ST PAUL' S BAY",
  "STA MARIA ESTATE"="STA MARIJA ESTATE",
  "TAL-IBRAG"="TAL-IBRAĠ",
  "ST ANGELO MANSIONS"="COTTONERA",
  "TAS-SELLUM"="MELLIEĦA",
  "SENSARA MALTA"="",
  "SERIOUS BUYER"="",
  "MENSIJA"="SAN ĠWANN",
  "SHELL FORM"="",
  "TAL-FJURI"="ST PAUL'S BAY",
  "IL-QORTIN"="MELLIEĦA"
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
  "WINE",
  "BARGAIN",
  "BASEMENT",
  "BRAND NEW",
  "BUNGALOW",
  "BUNGALOW CONVERTED",
  "BUNGALOW LARGE LANDSCAPED GARDEN THREE BEDROOMS",
  "BUSINESS",
  "CENTRAL",
  "CENTRALLY",
  "CLAYTON",
  "COMMERCIAL",
  "COMMISSION",
  "CONFECTIONERY",
  "CONVERTED",
  "DESIGNER FINISHED AND FURNISHED LUXURY DUPLEX APARTMENT",
  "DPLAIN TRPAR436",
  "EXCELLENT",
  "FARMHOUSE IN SOUTH WITH ADJOINING LAND €360",
  "FARMHOUSE RESIDENCE IN THE SOUTH",
  "FARMHOUSE WITH LAND",
  "FIRST FLOOR THREE BEDROOM",
  "FLATLET",
  "FOCUSED",
  "FORT",
  "FULLY DETACHED FIRST FLOOR ",
  "GARDEN WITH OLD ROOM",
  "GARDEN WITH VIEWS OF FILFLA",
  "GOLDEN",
  "GROUNDFLOOR CORNER",
  "HIGH",
  "HIGHLY FINISHED FIRST FLOOR MAISONETTE WITH OPEN VIEWS",
  "HIGHLY FINISHED FULLY FURNISHED",
  "HIGH RIDGE",
  "HOTEL",
  "HOUSE",
  "HOUSE OF CHARACTER WITH LARGE MILL ROOM",
  "HOUSES",
  "HOUSES OF CHARACTER FROM €55",
  "HOUSES OF CHARACTER WITH MODERN BEDROOMS",
  "INVEST",
  "INVESTMENT",
  "JUNIOR",
  "LOS",
  "LUXURIOUSLY",
  "LUXURY",
  "MAISONETTE",
  "MAISONETTE NO STEPS",
  "GREEN AREAS FOR RECREATIONAL PURPOSES",
  "MINUTES",
  "NEW AREA",
  "NEW SHELL FORM BLOCK OF THREE BEDROOM APARTMENTS",
  "NICELY CONVERTED THREE BEDROOM",
  "OLDER TYPE GROUNDFLOOR MAISONETTE",
  "ONE OF THE FEW FULLY DETACHED BUNGALOWS BUILT IN THE AND RENOVATED RECENTLY",
  "ON PLAN",
  "ON PLAN SPACIOUS FARMHOUSES WITH SWIMMING POOL",
  "PARTLY FURNISHED THREE BEDROOM APARTMENT UNOBSTRUCTED SEA-VIEWS",
  "PLOT WITH PERMITS FOR ELEVATED SEMI-DETACHED",
  "SELECT AREAS",
  "SENSARA",
  "SOUTH AREA",
  "SOUTH",
  "PMARKET PROPERTIES",
  "GREEN AREAS FOR RECREATIONAL PURPOSES",
  "GUDJA OR GĦAXAQ",
  "GUDJA OR ŻEJTUN",
  "GŻIRA AND SLIEMA",
  "KENNEDY GROVE AREA",
  "MATER DEI AREA",
  "MELLIEĦA \\(TRIQ L-IŻBARK",
  "PRICED TO SELL THREE BEDROOM APARTMENT",
  "REDUCED FOR QUICK SALE",
  "RESTAURANT WITH ALL PERMITS REQUIRED",
  "RUE D' ARGENS",
  "SANTA MARIA ESTATE",
  "SECOND FLOOR TWO BEDROOM APARTMENT WITH LARGE TERRACE",
  "SELLING YOUR PROPERTY? PHONE",
  "SELL YOUR PROPERTY WITH US FOR JUST COMMISSION",
  "SEMI-DETACHED HAVING THREE BEDROOMS AND SPACIOUS BBQ AREA",
  "SEMI-DETACHED VILLA FOR THE PRICE OF A TERRACED HOUSE",
  "SEMI-DETACHED VILLA PLOT",
  "SEMI-DETACHED VILLA WITH CLEAR UNOBSTRUCTED SEA VIEWS",
  "SEMI-DETACHED VILLA WITH GARDEN AND BASEMENT",
  "SEMI-DETACHED VILLA WITH NICE OUTDOORS AND PARTIALLY RENOVATED",
  "SENSARA ON FARMHOUSE",
  "SENSARA ON FARMHOUSES ONLY",
  "SENSARA ON FIELDS AND FARMHOUSES",
  "SPACIOUS TOWNHOUSE WITH DECENT BACK GARDEN",
  "ST ANGELO MANSIONS",
  "THREE BEDROOM APARTMENT WITH GOOD SIZED BALCONY OVERLOOKING A BEAUTIFUL ODZ AREA",
  "THREE BEDROOM APARTMENT WITH VALLETTA AND MANOEL ISLAND VIEWS",
  "THREE BEDROOM APARTMENT WITH YARD",
  "THREE BEDROOM GROUNDFLOOR MAISONETTE",
  "TYPICAL MALTESE TOWNHOUSE WITH ALL FEATURES",
  "UNDIVIDED PROPERTY SHARES OF HEIRS IN VARIOUS PARTS OF MALTA",
  "VALLETTA CONVERTED TOWNHOUSE USED AS OFFICES €680"

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
  maltesePrefixes <- c("TA'","SAN","THE","ST")
  
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
               s<- grepl(l,pattern=paste("^",pat,sep=""))
              
               return(s)
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
  
  locationFound <- (length(locations) > 0) 
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

  location <- findCorrectPlace(location)
  return(location)
}
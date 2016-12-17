library(rvest)
library(stringi)
library(stringr)

readForPropertyForSale <- function(f) {
  html <- read_html(f)
  as <- html %>% html_nodes('li > p,.classified_date') %>% html_text()
  classDate <- ""
  i <- 0
  properties <- list()
  for (a in as) {
    i <- i+1
    propSaleLen <- str_length('\nProperty For Sale\n')
    
    propertyEntry <- list()
    if (startsWith(a,'\nProperty For Sale\n') & str_length(a) > propSaleLen) {
      len <- substr(a,propSaleLen+1,str_length(a))
      propertyEntry$date <- classDate;
      propertyEntry$propertyDesc <- len
    } else {
      classDate <- a;
    }
    properties[[i]] <- propertyEntry
  }
  
  return(properties)
}

extractDataFromDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  
  htmlFiles <- htmlFiles[1:1]
  for (htmlFile in htmlFiles) {
    pathName <- paste('data','/',htmlFile,sep = "")
    readForPropertyForSale(pathName)
  }
}

extractDataFromDir('data')
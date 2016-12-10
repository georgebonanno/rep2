library(rvest)

source('tm_utils.R')

pastePrint <- function(...) {
  print(paste(...,sep = " "))
}


extractPs <- function(f) {
  pastePrint("extract html text from ",f,"...")
  scraping_times <- read_html(f)
  
  tags <- c("p","h1","h2","h3","li","table","div")
  
  extraction <- list()
  for (t in tags) {
    htmlText <- scraping_times %>% html_nodes(t) %>% html_text()
    extraction[[t]]$text <- htmlText
    extraction[[t]]$tag <- t
  }
  pastePrint("data loaded from ",f)
  return(extraction)
}

extractDataFromHtmlInDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  htmlFiles <- htmlFiles[1:2]
  pastePrint("html files = ",htmlFiles)
  for (htmlFile in htmlFiles) {
    pastePrint('working on file ',htmlFile)
    f <- paste(sourceDir,'/',htmlFile,sep="")
    htmlTextForF <- extractPs(f)
    fileToHtmlTextInfo[[htmlFile]] <- htmlTextForF
  }
  
  return(fileToHtmlTextInfo)
}

extractStatsForDocs <- function(docsTextsForTag) {
  termMatrix <- list()
  docIndex <- 0
  for (doc in docsTextsForTag) {
    docIndex <- docIndex+1
    #doc <- docsTextsForTag[docIndex]
    #pastePrint("index",doc)
    termMatrix[[docIndex]] <- list()
    for (textInTag in doc) {
      concatText <- paste(textInTag$text,collapse="")
      pastePrint()
      tag <- textInTag$tag
      termMatrix[[docIndex]][[tag]] <- extractDocTermMatrixForListOfStrings(c(concatText))
    }
    
  }
  return(termMatrix)
}

#print("starting...")
docTextsPerTag <- extractDataFromHtmlInDir('data')
termMatrix <- extractStatsForDocs(docTextsPerTag)



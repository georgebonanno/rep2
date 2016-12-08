library(rvest)

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
    extraction[[t]] <- htmlText
  }
  pastePrint("data loaded from ",f)
  return(extraction)
}

extractDataFromHtmlInDir <- function(sourceDir) {
  htmlFiles <- list.files(sourceDir,pattern="*.html*")
  fileToHtmlTextInfo <- list()
  htmlFiles <- htmlFiles[1:1]
  pastePrint("html files = ",htmlFiles)
  for (htmlFile in htmlFiles) {
    pastePrint('working on file ',htmlFile)
    f <- paste(sourceDir,'/',htmlFile,sep="")
    htmlTextForF <- extractPs(f)
    fileToHtmlTextInfo[[htmlFile]] <- htmlTextForF
  }
  
  return(fileToHtmlTextInfo)
}

#print("starting...")
text <- extractDataFromHtmlInDir('data')


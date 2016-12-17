library(tm)

#applies stemming, whitespace removal and other nlp processing
#techniques to a list of documents (each of them being a string)
applyTM <- function(docs) {
  return(applyTMOnSource(VectorSource(docs)))
}

#applies stemming, whitespace removal and other nlp processing
#techniques to a source (e.g VectorSource or DocumentSource)
applyTMOnSource <- function(source) {
  
  docs <- Corpus(source)   
  
  docs <- tm_map(docs,removePunctuation)
  docs <- tm_map(docs,removeNumbers)
  docs <- tm_map(docs,tolower)
  
  numbers <- c("one","two","three","four","five","six","seven","eight","nine");
  docs <- tm_map(docs, removeWords, numbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stemDocument) 
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)  
  return (docs)
}

applyTmOnDoc <- function(directoryPath) {
  return(applyTMOnSource(DirSource(directoryPath)))
}

extractDocTermMatrix <- function(docs) {
  dtm <- DocumentTermMatrix(docs)
  freq <- colSums(as.matrix(dtm)) 
  wf <- data.frame(word=names(freq), freq=freq)
  
  wf <- wf[order(-wf$freq),]
  return(wf)
}

extractDocTermMatrixForTmSource <- function(tm) {
  
  dtm <- (extractDocTermMatrix(tm))
  tdm <- TermDocumentMatrix(tm,control = list(weighting = weightTfIdf, stopwords = TRUE))
  return(list(dtm=dtm,tdm=tdm))
}

extractDocTermMatrixForListOfStrings <- function(listOfString) {
  tm <- applyTM(listOfString);
  return(extractDocTermMatrixForTmSource(tm))
}

extractDocTermMatrixForDocs <- function(dirPath) {
  tm <- applyTmOnDoc(dirPath);
  return(extractDocTermMatrixForTmSource(tm))
}

test <- function() {
  strings <- c('hello, this is a discussion on a good book. It was written 
               while I was on a journey. You may ask, what is the journey?')
  
  d <- extractDocTermMatrixForListOfStrings(strings)
  
  return(d)
}

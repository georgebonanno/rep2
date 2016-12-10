library(tm)

#applies stemming, whitespace removal and other nlp processing
#techniques to a list of documents (each of them being a string)
applyTM <- function(docs) {
  
  docs <- Corpus(VectorSource(docs))   
  
  docs <- tm_map(docs,removePunctuation)
  docs <- tm_map(docs,removeNumbers)
  docs <- tm_map(docs,tolower)
  
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stemDocument) 
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)  
  return (docs)
}

extractDocTermMatrix <- function(docs) {
  dtm <- DocumentTermMatrix(docs)
  freq <- colSums(as.matrix(dtm)) 
  wf <- data.frame(word=names(freq), freq=freq)
  
  wf <- wf[order(-wf$freq),]
  return(wf)
}

extractDocTermMatrixForListOfStrings <- function(listOfString) {
  return (extractDocTermMatrix(applyTM(listOfString)))
}

test <- function() {
  strings <- c('hello, this is a discussion on a good book. It was written 
               while I was on a journey. You may ask, what is the journey?')
  
  d <- extractDocTermMatrixForListOfStrings(strings)
  
  return(d)
}

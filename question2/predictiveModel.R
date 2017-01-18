library('e1071')

source('csvProcessing.R')

propDetails <- propDetails[,c("location","price_euro","property_type",
                              "area_sqm")]

#  round n by 'by'. e.g round 56 by 5 = 55
roundNumber <- function(n,by) {
  mod <- n %% by
  int <- floor(n/by)
  if (mod > (by / 2)) {
    int <- int+1
  }
  return(int*by)
}

#  obtain min and max prices rounded to 5000 (min is less than the
#  actual min whilst max is greater than the actual max
minPrice <- round(10000*ceiling(min(propDetails$price_euro)/10000),5000)
maxPrice <- round(5000+(10000*(max(ceiling(propDetails$price_euro/10000)))),
                        5000)

#  obtain a list (trainingSet,testDataSet). A random sample (10%) of
#  the properties are extracted and used for the test data set. The rest
#  of the sample is used as the training set.
obtainTrainingAndTestData <- function() {
  dataSize <- dim(propDetails)[1]
  testDataSize <- floor(0.1*dataSize)
  
  allDataIndices <- c(1:dataSize)
  testDataIndices <- sample(1:dataSize,testDataSize,replace = FALSE) 
  trainingSetIndices <- allDataIndices[!(allDataIndices %in% testDataIndices)]
  
  return (list(trainingSet = propDetails[trainingSetIndices,],
                testDataSet=propDetails[testDataIndices,]))
}

propDetails$property_type <- factor(propDetails$property_type)
propDetails$location <- factor(propDetails$location)


propDetails$priceRange <- factor(cut(propDetails$price_euro,
                  seq(from=minPrice,to=maxPrice, by=5000),
                  right = TRUE))

trainingAndTestData <- obtainTrainingAndTestData()

trainingData <- trainingAndTestData$trainingSet
testData <- trainingAndTestData$testDataSet
priceRangesToPredict <- testData
priceRangesToPredict <- testData[,c("location","price_euro","property_type",
                                    "area_sqm")]

model <- naiveBayes(priceRange ~ .,data=trainingData)

predictedPriceRanges <- predict(model,priceRangesToPredict[1:10,])



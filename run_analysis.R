
mergeDatasets <- function(directory){
  setwd(directory)
  
  trainData <- tidyUpDatasets("train")
  testData <- tidyUpDatasets("test")
  
  dataset <- rbind(trainData, testData)
  dataset
}

tidyUpDatasets <- function(type){
  #Read the fatures file
  featuresTable <- read.table("features.txt", header = FALSE)
  old.dir <- getwd()
  setwd(paste0("./", type))
  fileSuffix <- paste0("_",type,".txt")
  
  activities <- c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  
  #Load subject data into tables
  subjectsTable <- read.table(paste0("subject", fileSuffix), header = FALSE)
  labelsTable <- read.table(paste0("y", fileSuffix), header = FALSE)
  allFeatures <- read.table(paste0("X",fileSuffix), header = FALSE)

  i <- 1
  features <- data.frame(id = integer(), name = character())
  for(name in featuresTable[,2]){
    if(grepl("-mean\\(\\)|-std\\(\\)", name)){
      features <- rbind(features, data.frame(id = i, name = name))
    }
    i <- i+1
  }
  
  #Select only means and stds.
  selectedFeatures <- allFeatures[,features[,1]]
  
  dataset <- cbind(subjectsTable[,1], selectedFeatures, labelsTable[,1])
  colnames(dataset) <- c("Subject", as.vector(features[,2]) , "Activity")
  
  setwd(old.dir)
  
  #Replace activities
  j = 1
  for(activity in dataset$Activity){
    dataset$Activity[[j]] <- activities[activity[[1]]]
    j <- j + 1
  }
  
  dataset
}
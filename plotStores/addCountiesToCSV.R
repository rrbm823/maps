require("plyr")
source("countystategrab.R")
source("dl store from SVN.R")
addCounties <- function(store){
  if(!file.exists(paste0(store, ".csv"))){
    theStore <- retrieveCSV(store)
  }else theStore <- read.csv(paste0(store, ".csv"))
  m <- nrow(theStore)
  n <- ncol(theStore)
  theStore[,n-1] <- as.numeric(theStore[,n-1])
  theStore[,n] <- as.numeric(theStore[,n])
  
  oneCountyAdd <- function(i) {
    lat <- theStore[i,n-1]
    lng <- theStore[i,n]
    x <- tryCatch(as.character(getcounty(lat, lng)), error = function(e) e)
    if(!inherits('x', 'error')) return(x)
  }
  theStore$Counties <- laply(1:m, oneCountyAdd, .progress = "text")
  

  return(theStore)
  
}
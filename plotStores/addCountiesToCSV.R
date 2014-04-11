require("plyr")
#source("countystategrab.R")
source("state Abbr to Name.R")
source("dl store.R")
addCounties <- function(store, addDemo = T, no_error_msg = F){
  ### In case we want to keep the files
  fileLoc <- paste0("./storeFiles/", store)
  if(!file.exists(fileLoc)){
    theStore <- retrieveCSV(store)
  }
  else theStore <- read.csv(fileLoc, header = T, stringsAsFactors = F)
  m <- nrow(theStore)
  n <- ncol(theStore)
  a <- names(theStore)
  ###c <- grep("[Cc]ounty", a)
  ###if(length(c) > 0) return(theStore)
  zip.pos <- grep("[Zz]ip", a)
  state.pos <- grep("[Ss]tate", a)
  
  if(length(zip.pos) > 0){
    
    zipCodes <- read.csv("zip-codes-database-DELUXE.csv", stringsAsFactors = F)
    zipCodes <- zipCodes[which(zipCodes[,2] == "P"), ]
    theStore[,zip.pos] <- as.numeric(gsub("-\\d+", "", theStore[,zip.pos]))
    theStore <- merge(theStore, zipCodes, by.x = zip.pos, by.y = "ZipCode")
    #b <- laply(theStore[,zip.pos], function(i) is.element(i, zipCodes$ZipCode))
    #missingStores <- theStore[which(!b), ]
    a <- names(theStore)
    state.pos <- grep("[Ss]tate.x", a)
    theStore$County <- paste(tolower(laply(theStore[,state.pos], stateAbb2Name)), tolower(theStore$County), sep = ",")
    if(!addDemo) theStore <- data.frame(theStore[,1:n], County = theStore$County)
    
  }
  else if(!no_error_msg) stop("can't find zip codes!")
  ##This takes a long time
#   else{
#     
#     theStore[,n-1] <- as.numeric(theStore[,n-1])
#     theStore[,n] <- as.numeric(theStore[,n])
#     
#     latlngCountyAdd <- function(i) {
#       lat <- theStore[i,n-1]
#       lng <- theStore[i,n]
#       x <- tryCatch(as.character(getcounty(lat, lng)), error = function(e) e)
#       if(!inherits('x', 'error')) return(x) else return('NA')
#     }
#     
#     theStore$County <- laply(1:m, latlngCountyAdd, .progress = "text")
#     splits <- laply(strsplit(theStore$County, ", "), identity)
#     theStore$County <- paste(tolower(laply(splits[,2], stateAbb2Name)), tolower(gsub(" County", "", splits[,1])), sep = ",")
#   }

  ###write.csv(theStore, fileLoc)
  return(theStore)
  
}
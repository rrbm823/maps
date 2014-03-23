owd <- getwd()
setwd("~/GitHub/AggData-Rcode")
require(maptools)
require(plotGoogleMaps)
require(RColorBrewer)
source("countysinglegrab.R")
source("GetStoreMap.r")
usco <- readShapeSpatial("co99_d00")
proj4string(usco) <- CRS("+proj=longlat +datum=NAD27")
fips <- read.csv("US_FIPS_Codes.csv", header = T, colClasses = rep("character", 4))

plotCounties <- function(Store = "", State = "", zcol = "OUTPUT"){
  StoreData <- getStoreMap(tolower(Store), plot = F)
  if(nrow(StoreData) < 2) return("There are less than 2 stores")
  n <- ncol(StoreData)
  stateAbb2Name <- function(i){
                    x <- append(state.name, c("Puerto Rico", "Virgin Islands", "Armed Forces Europe", "District of Columbia", "Armed Forces Americas", "Armed Forces Pacific", "American Samoa", "Guam", "Palau", "Federated States of Micronesia", "Northern Mariana Islands", "Marshall Islands"))
                    y <- append(state.abb, c("PR", "VI", "AE", "DC", "AA", "AP", "AS", "GU", "PW", "FM", "MP", "MH"))
                    if(nchar(i) < 3){
                      i <- x[which(is.element(y, i))]
                    }
                  else return(i)
                  }
  State <- unlist(lapply(State, stateAbb2Name))
  theseRows <- which(is.element(fips$State, State))
  fipsCodes <- fips[theseRows,]
  thisStatesCode <- unique(fipsCodes$FIPS.State)
  countyData <- fipsCodes[,c("State", "County.Name")]
  countyData$Count <- 0
  if(length(StoreData$Zip) == 0){
    usco1 <- usco[which(usco$STATE == thisStatesCode),]
    for(i in 1:nrow(StoreData)){
      PossError <- tryCatch(getcounty(StoreData[i,n-2],StoreData[i,n-1]), error = function(e) e)
      if(!inherits(PossError, "error")) {
        thisCounty <- getcounty(StoreData[i,n-2],StoreData[i,n-1])
        thisRow <- which(countyData$County == as.character(thisCounty))
        countyData[thisRow,2] <- countyData[thisRow,2] + 1
      }
    }
    theseCountyCodes <- merge(fipsCodes, countyData, by.x = "County.Name", by.y = "County")
    stateSpData <- merge(usco1, theseCountyCodes, by.x = "COUNTY", by.y = "FIPS.County")
  }
  else{
    zipCodes <- read.csv("zip_code_database.csv", sep=";")[,c("zip","state","county","estimated_population")] #maybe not a good source
    zipCodes$state <- unlist(lapply(as.character(zipCodes$state), stateAbb2Name))
    usco1 <- usco[which(is.element(usco$STATE, thisStatesCode)),]
    countyData2 <- merge(countyData, zipCodes, by.x = c("State","County.Name"), by.y = c("state","county"))
    for(i in StoreData$Zip){
      thisRow <- which(countyData2$zip == i)
      countyData2[thisRow,"Count"] <- countyData2[thisRow,"Count"] + 1
    }
    countyData2 <- aggregate(cbind(Count,estimated_population) ~ County.Name + State, data = countyData2, sum)
    theseCountyCodes <- merge(fipsCodes, countyData2, by = c("County.Name", "State"))
    usco1$FIPS <- paste0(usco1$STATE, usco1$COUNTY)
    theseCountyCodes$FIPS <- paste0(theseCountyCodes$FIPS.State, theseCountyCodes$FIPS.County)
    stateSpData <- merge(usco1, theseCountyCodes, by = "FIPS")
  }
  stateSpData$OUTPUT <- stateSpData$Count / stateSpData$AREA
  usmap2 <- plotGoogleMaps(stateSpData, zcol = zcol, filename = 'Map.html', colPalette = brewer.pal(7,"Reds"), strokeColor = "white")
  return(usmap2)
}
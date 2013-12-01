#By Robert G.
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
  State <- as.character(State)
  if(nchar(State) < 3) print("please enter the entire state name")
  stateCounties <- fips[which(fips$State == State),"County.Name"]
  fipsCodes <- fips[which(fips$State == State),]
  thisStatesCode <- fipsCodes$FIPS.State[1]
  storeCount <- rep(0, length(stateCounties))
  countyData <- data.frame(County = stateCounties, Count = storeCount)
  for(i in 1:nrow(StoreData)){
    PossError <- tryCatch(getcounty(StoreData[i,n-2],StoreData[i,n-1]), error = function(e) e)
    usco1 <- usco[which(usco$STATE == thisStatesCode),]
    if(!inherits(PossError, "error")) {
      thisCounty <- getcounty(StoreData[i,n-2],StoreData[i,n-1])
      thisRow <- which(countyData$County == as.character(thisCounty))
      countyData[thisRow,2] <- countyData[thisRow,2] + 1
    }
  }
  theseCountyCodes <- merge(fipsCodes, countyData, by.x = "County.Name", by.y = "County")
  stateSpData <- merge(usco1, theseCountyCodes, by.x = "COUNTY", by.y = "FIPS.County")
  stateSpData$OUTPUT <- stateSpData$Count / stateSpData$AREA
  usmap <- plotGoogleMaps(stateSpData, zcol = zcol, filename = 'Map.html', colPalette = brewer.pal(7,"Reds"), strokeColor = "white")
  return(usmap)
}
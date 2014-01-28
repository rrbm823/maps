source("internal distance.R")
source("dl store from SVN.R")
require(plotGoogleMaps)
require(RColorBrewer)

plotStores <- function(store){
  myStore <- retrieveCSV(store)
  origStore <- myStore
  n <- ncol(myStore)
  names(myStore)[n-1] <- "lat"
  names(myStore)[n] <- "lng"
  coordinates(myStore) <- ~lng+lat
  proj4string(myStore) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  m <- plotGoogleMaps(myStore, paste0(store, "_map.html"), add = T, previousMap = test, colPalette = "#FBB4AE")
  return(m)
  
  
  
}
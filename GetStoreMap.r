require("googleVis")
getStoreMap <- function(Store, plot = F) {
  #Set the AggData variable to the store locations directory
  AggData <- paste0(gsub('Documents', 'Google Drive',
                         path.expand('~/Scrapings/Store Locations/')))
  AggDataFiles <- list.files(AggData, pattern = '.csv')
  if(class(Store) == "numeric") {
    Store <- AggDataFiles[Store]
    if(length(Store) == 0) return("There was no store match.")
    ThisStore <- read.csv(paste0(AggData, Store), header = F)
  }
  else {
    ThisStoreFile <- grep(Store, AggDataFiles, value = T)[1]
    if(length(ThisStoreFile) == 0) return("There was no store match.")
    ThisStore <- read.csv(paste0(AggData, ThisStoreFile))
  }
  #Munging for lat/long for gvis
  ThisStore$LatLong <- paste0(as.character(ThisStore$Latitude), ":", as.character(ThisStore$Longitude))
  if(is.null(ThisStore$Store.name)) {
    StoreMap <- gvisMap(ThisStore, locationvar="LatLong", 
                        options = list(enableScrollWheel = T, showTip = T))
  }
  else {
    StoreMap <- gvisMap(ThisStore, locationvar="LatLong", tipvar = "Store.Name", 
                      options = list(enableScrollWheel = T, showTip = T))
  }
  if(plot) plot(StoreMap)
  #save to output folder
  html <- paste(unlist(StoreMap$html),collapse="")
  write(html, file = paste0(AggData, 'Output/', Store, '.html'))
  return(ThisStore)
}

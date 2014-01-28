require(PBSmapping)
myshpfl <- importShapefile("countyp010", readDBF = TRUE)
names <- read.dbf("countyp010.dbf")
getcounty <- function(lat,lon){
  addressEvent <- matrix(c(lon,lat,1),1)
  colnames(addressEvent) <- c("X","Y","EID")
  addressPoly <- findPolys(addressEvent,myshpfl)
  index <- merge(addressPoly, names, by.x=2, by.y=3)
  county <- index$COUNTY
  state <- index$STATE
  return(paste(as.character(county), as.character(state), sep = ", "))
}
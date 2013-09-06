setwd(shapefilepath)
myshpfl<-importShapefile("co99_d00", readDBF = TRUE)
names<-read.dbf("co99_d00.dbf")
getcounty<-function(lat,lon){
  addressEvent<-matrix(c(lon,lat,1),1)
  colnames(addressEvent)<-c("X","Y","EID")
  addressPoly<-findPolys(addressEvent,myshpfl)
  index<-merge(addressPoly, names, by.x=2,by.y=4)
  county<-index[1,10]
  return(county)
}
###Example###
getcounty(33,-84)
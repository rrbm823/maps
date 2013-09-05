outputsource<-"C:\\Users\\Erik\\Documents\\R\\output"
outputfile<-"test_out2.csv"
inputsource<-"C:\\Users\\Erik\\Documents\\R\\r_geocoding"
shapefilepath<-"C:\\Users\\Erik\\Documents\\R\\spatial"
##########################
library(maptools)
library(PBSmapping)

setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv")
ae<-length(a)
agglistorig<-do.call("list", lapply(a, read.csv, header = TRUE))
numbers<-matrix(1:dim(agglistorig[[1]][1]),dim(agglistorig[[1]][1]))
agglistorig2<-cbind(numbers,agglistorig[[1]])
len<-dim(agglistorig[[1]])[1]
shapelist<-agglistorig
for (i in 1:ae)
{
  shapelist[[i]]<-cbind(shapelist[[i]][,dim(shapelist[[i]])[2]],shapelist[[i]][,dim(shapelist[[i]])[2]-1],matrix(1:dim(shapelist[[i]])[1],dim(shapelist[[i]])[1]))
  shapelist[[i]]<-data.frame(shapelist[[i]])
  colnames(shapelist[[i]])<-c("X","Y","EID")
}
addressEvents<-vector("list",ae)
for (i in 1:ae)
{
  addressEvents[[i]]<-as.EventData(shapelist[[i]],projection=NA)
}

setwd(shapefilepath)
myshpfl<-importShapefile("ne_110m_admin_0_countries", readDBF = TRUE)
names<-read.dbf("ne_110m_admin_0_countries.dbf")
names2<-names$name

addressPolys<-vector("list",ae)
for (i in 1:ae)
{
  addressPolys[[i]]<-findPolys(addressEvents[[i]],myshpfl)
}
addressPolys2<-as.matrix(addressPolys[[1]])
index<-matrix(1:dim(addressPolys2)[[1]],dim(addressPolys2))
for (i in 1:dim(addressPolys2)[[1]])
{
  index[i,1]<-as.character(names$name[addressPolys2[i,2]])	
}
compiled<-cbind(addressPolys2,index)
compiled2<-merge(agglistorig2,compiled, all.x=TRUE,by.x=1,by.y=1)
compiled3<-compiled2[,c(-dim(compiled3)[2]-3,-dim(compiled3)[2]-2,-dim(compiled3)[2]-1,-1)]
colnames(compiled3)[dim(compiled3)[2]]<-"Country"
setwd(outputsource)
write.csv(compiled2,outputfile)
outputsource<-"C:\\Users\\Erik J\\Documents\\R\\r_geocoding"
outputfile<-"test_out.csv"
inputsource<-"C:\\Users\\Erik J\\Documents\\R\\r_geocoding"
shapefilepath<-"C:\\Users\\Erik J\\Documents\\R\\spatial"
##########################
library(maptools)
library(PBSmapping)

setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv")
ae<-length(a)
agglistorig<-do.call("list", lapply(a, read.csv, header = TRUE))
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
myshpfl<-importShapefile("co99_d00", readDBF = TRUE)
names<-read.dbf("co99_d00.dbf")
addressPolys<-vector("list",ae)
for (i in 1:ae)
{
	addressPolys[[i]]<-findPolys(addressEvents[[i]],myshpfl)
}
addressPolys2<-as.matrix(addressPolys[[1]])
index<-merge(addressPolys2, names, all.x=TRUE,by.x=2,by.y=4) 
countynames<-index[,c(2,10)]
countynames2<-matrix(1:len,len)
countynames3<-merge(countynames2,countynames,all.x=TRUE,by.x=1,by.y=1)
alltogether<-cbind(agglistorig[[1]],countynames3[,2])
colnames(alltogether)[dim(alltogether)[2]]<-"County"
setwd(outputsource)
write.csv(alltogether,outputfile)
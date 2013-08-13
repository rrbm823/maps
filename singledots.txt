outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
outputfile<-"a_and_w_05-16-12.jpg"
inputsource<-"C:\\Users\\Erik J\\Documents\\R\\test3"
importantfile<-"a_and_w_05-16-12.csv"
shapefilepath<-"C:\\Users\\Erik J\\Documents\\R\\spatial\\shapefiles"
##########################
library(maptools)
library(PBSmapping)
setwd(inputsource)
a<-read.csv(importantfile)
shapelist<-a
shapelist<-cbind(shapelist[,dim(shapelist)[2]],shapelist[,dim(shapelist)[2]-1],matrix(1:dim(shapelist)[1],dim(shapelist)[1]))
shapelist<-data.frame(shapelist)
colnames(shapelist)<-c("X","Y","EID")
addressEvents<-as.EventData(shapelist,projection=NA)
setwd(shapefilepath)
myshpfl<-read.csv("worldshapefileunifiedus.csv")
addressPolys<-findPolys(addressEvents,myshpfl)
usmap<-plotPolys(myshpfl,xlim=c(-125,-67), ylim=c(20,60),axes=FALSE,bg="white",xlab="",ylab="",col="white")
myTrtFC<-table(factor(addressPolys$PID,levels=levels(as.factor(usmap$PID))))
mapColors<-heat.colors(max(myTrtFC)+1,alpha=.6)[max(myTrtFC)-myTrtFC+1]
mapcolors1<-gsub("#FFFFFE99","gray75",mapColors)
mapcolors2<-gsub("#FF000099","lightgoldenrodyellow",mapcolors1)
usmapcol<-plotPolys(myshpfl,xlim=c(-125,-67), ylim=c(20,60),axes=FALSE,bg="skyblue1",xlab="",ylab="",col=mapcolors2)
addPoints(addressEvents,pch=16,col="red",cex=.5)
setwd(outputsource)
savePlot(outputfile, type = "jpg")
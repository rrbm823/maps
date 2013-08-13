outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
inputsource<-"C:\\Users\\Erik J\\Documents\\R\\test3"
shapefilepath<-"C:\\Users\\Erik J\\Documents\\R\\spatial\\shapefiles"
outputfile<-"multidots.jpg"
#########################
library(maptools)
library(PBSmapping)
setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv")
ae<-length(a)
agglistorig<-do.call("list", lapply(a, read.csv, header = TRUE))
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
colornames<-c("blue","red","yellow","green","orange","purple","brown","teal","pink")
color<-colornames[1:ae]
setwd(shapefilepath)
myshpfl<-read.csv("worldshapefileunifiedus.csv")
addressPolys<-vector("list",ae)
for (i in 1:ae)
{
	addressPolys[[i]]<-findPolys(addressEvents[[i]],myshpfl)
}
usmap<-plotPolys(myshpfl,xlim=c(-125,-67), ylim=c(20,60),axes=FALSE,bg="white",xlab="",ylab="",col="white")
myTrtFC<-vector("list",ae)
for (i in 1:ae)
{
	myTrtFC[[i]]<-table(factor(addressPolys[[i]]$PID,levels=levels(as.factor(usmap$PID))))
}
mapColors<-heat.colors(max(myTrtFC[[2]])+1,alpha=.6)[max(myTrtFC[[2]])-myTrtFC[[2]]+1]
mapcolors1<-gsub("#FFFFFE99","gray75",mapColors)
mapcolors2<-gsub("#FF000099","lightgoldenrodyellow",mapcolors1)
usmapcol<-plotPolys(myshpfl,xlim=c(-125,-67), ylim=c(20,60),axes=FALSE,bg="skyblue1",xlab="",ylab="",col=mapcolors2)

for (i in 1:ae)
{
	addPoints(addressEvents[[i]],pch=16,col=color[i],cex=.5)
}
for (i in 1:ae)
{
	addPoints(addressEvents[[ae-i+1]],pch=16,col=color[ae-i+1],cex=(.5/(i+1)))
}
legend("bottomright", cex=0.75, pch=16,col=colornames[1:ae], legend=c(a[1:ae]), ncol=2,bg="white")
setwd(outputsource)
savePlot(outputfile, type = "jpg")
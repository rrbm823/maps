outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
inputsource<-"C:\\Users\\Erik J\\Desktop\\AggData\\Premium\\Shopping"
importantfile<-"walmart_09-27-12.csv"
zipfilepath<-"C:\\Users\\Erik J\\Desktop\\AggData\\Subversion\\R code"
outputfile<-"walmartaverages1.csv"
###################
setwd(inputsource)
a<-read.csv(importantfile)
setwd(zipfilepath)
b<-read.csv("zipinfo.csv")
a1<-data.frame(a[,grep("Zip.Code",colnames(a))],v2='')
colnames(a1)<-c("v1","v2")
d<-merge(a1$v1,b,by.x=1,by.y=1)
e<-colMeans(d, na.rm = TRUE)
f<-e[-1]
setwd(outputsource)
write.csv(f,outputfile)
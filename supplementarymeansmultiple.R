outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
inputsource<-"C:\\Users\\Erik J\\Desktop\\AggData\\Premium\\Food & Dining"
zipfilepath<-"C:\\Users\\Erik J\\Desktop\\AggData\\Subversion\\R code"
importantfile<-"zipinfo.csv"
outputfile<-"foodaverages2.csv"
###################
setwd(zipfilepath)
zip<-read.csv("zipinfo.csv")
setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv",recursive=TRUE,include.dirs=TRUE)
a1<-grep("archive",a,invert=TRUE,value=TRUE)
ae<-length(a1)
b<-do.call("list", lapply(a1, read.csv, header = TRUE))
len<-matrix(1:ae,ae)
for(i in (1:ae))
	{
		len[i,]<-dim(b[[i]])[1]
	}
c<-matrix(1:ae,ae)
for (i in (1:ae))
	{
		c[i,]<-match("Zip.Code",colnames(b[[i]]))
	}
c1<-which(c %in% c(1:20))
p<-vector("list",ae)
for (i in c1)
	{
	p[[i]]<-b[[i]]["Zip.Code"]
	p[[i]]<-merge(zip,p[[i]],by.x=1,by.y=1)
	p[[i]]<-colMeans(p[[i]], na.rm = TRUE)
	}
p1<-which(p %in% "NULL")
testmat<-matrix(1:length(p[[1]]),length(p[[1]]))
for (i in p1)
	{
		p[[i]]<-testmat
	}
q<-data.frame(p)
q1<-rowSums(t(q))
r<-cbind(a1,len,t(q),q1)
r1<-subset(r,q1!=105)
ae1<-dim(r1)[1]
r2<-r1[,c(-3,-17)]
s<-matrix(1:(8*ae1),ae1)
for (i in 1:ae1)
	{
	for (j in 1:8)
		{
		s[i,j]<-as.numeric(r2[i,j+4])/as.numeric(r2[i,3])
		}
	}
r3<-cbind(r2[,c(-5:-12)],s)
names<-c("Store_Name","Locations",colnames(r3[,3:7]),colnames(r2[,5:12]))
colnames(r3)<-names
setwd(outputsource)
write.csv(r3,outputfile)

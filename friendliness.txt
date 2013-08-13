outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
inputsource<-"C:\\Users\\Erik J\\Documents\\R\\testcolleges2"
importantfilesource<-"C:\\Users\\Erik J\\Documents\\R\\testcolleges1"
importantfile<-"aaances1.csv"
outputfile<-"friendlinessfactor.csv"
###########################
setwd(importantfilesource)
b<-read.csv(importantfile)
b1<-b
b1<-cbind(b1[,dim(b1)[2]-1],b1[,dim(b1)[2]])
b1<-data.frame(b1)
b1<-b1*pi/180
setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv")
ae<-length(a)
agglistorig<-do.call("list", lapply(a, read.csv, header = TRUE))
agglist<-agglistorig
for (i in 1:ae)
{
	agglist[[i]]<-cbind(agglist[[i]][,dim(agglist[[i]])[2]-1],agglist[[i]][,dim(agglist[[i]])[2]])
	agglist[[i]]<-data.frame(agglist[[i]])
	agglist[[i]]<-agglist[[i]]*pi/180
}
et<-vector("list",ae)
for (j in 1:ae)
{
	c<-agglist[[j]]
	d<-b1
	ce<-length(t(agglist[[j]]))/length(agglist[[j]])
	de<-length(t(b1))/length(b1)
	det<-matrix(1:(ce*de),ce)
	for (i in 1:ce) {det[i,1:de]<-matrix(3963.1676*2*atan2(sqrt((sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2)))), sqrt(1-(sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2))))))
}
	et[[j]]<-det
}
ef<-vector("list",ae)
for (j in 1:ae)
{
	ce<-dim(et[[j]])[1]
	def<- matrix(1:ce,ce)
		for(i in 1:ce) {def[i]<-matrix(min(et[[j]][i,]))
		}
	ef[[j]]<-def
}
med<-matrix(1:ae,ae)
mea<-matrix(1:ae,ae)
for (i in 1:ae)
{
	test<-ef[[i]][1:dim(ef[[i]])[1]]
	med[i]<-median(test)
	mea[i]<-mean(test)
}
final<-cbind(a,med,mea)
colnames(final)<-c("Name","Median distance to nearest college","Mean distance to nearest college")
setwd(outputsource)
write.csv(final,outputfile)
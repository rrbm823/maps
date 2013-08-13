outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
inputsource<-"C:\\Users\\Erik J\\Documents\\R\\test3"
importantfile<-"whataburger_06-01-12.csv"
outputfile<-"whataburgeragain_06-01-12.csv"
###################
setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv")
a1<-a[1]
a2<-grep(importantfile,a)
a[a2]<-a[1]
a[1]<-importantfile
ae<-length(a)
agglistorig<-do.call("list", lapply(a, read.csv, header = TRUE))
agglist<-agglistorig
for (i in 1:ae)
{
	agglist[[i]]<-cbind(agglist[[i]][,dim(agglist[[i]])[2]-1],agglist[[i]][,dim(agglist[[i]])[2]])
	agglist[[i]]<-data.frame(agglist[[i]])
	agglist[[i]]<-agglist[[i]]*pi/180
}
et<-vector("list",(ae-1))
for (j in 2:ae)
{
	c<-agglist[[1]]
	d<-agglist[[j]]
	ce<-length(t(agglist[[1]]))/length(agglist[[1]])
	de<-length(t(agglist[[j]]))/length(agglist[[j]])
	det<-matrix(1:(ce*de),ce)
	for (i in 1:ce) {det[i,1:de]<-matrix(3963.1676*2*atan2(sqrt((sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2)))), sqrt(1-(sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2))))))
}
	et[[j-1]]<-det
}
ef<-vector("list",(ae-1))
for (j in 1:(ae-1))
{
	def<- matrix(1:ce,ce)
		for(i in 1:ce) {def[i]<-matrix(min(et[[j]][i,]))
		}
	ef[[j]]<-def
}
b<-a[-1]
for (i in 1:(ae-1))
{
	colnames(ef[[i]])<-b[i]
}
final<-cbind(agglistorig[[1]],ef)
setwd(outputsource)
write.csv(final,outputfile)
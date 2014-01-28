outputsource<-"C:\\Users\\Erik\\Documents\\R\\output"
inputsource<-"C:\\Users\\bj\\Google Drive\\Scrapings\\Store Locations"
importantfile<-"skyline_chili.csv"
outputfile<-"skyline_chili_analysis.csv"
rad<-5
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
for (i in 1:ae)#list all the data latlong, convert evertything to radians
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
  for (i in 1:ce) {det[i,1:de]<-matrix(3963.1676*2*atan2(sqrt((sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2)))), 
                                                         sqrt(1-(sin((t(d)[1,1:de]-c[i,1])/2)*sin((t(d)[1,1:de]-c[i,1])/2)+(cos(c[i,1])*cos(t(d)[1,1:de])*sin((t(d)[2,1:de]-c[i,2])/2)*sin((t(d)[2,1:de]-c[i,2])/2))))))
  }
  et[[j-1]]<-det
}
ct<-vector("list",(ae-1))
for (j in 1:(ae-1))
{
  dct<-matrix(1:ce,ce)
  for (i in 1:ce) {dct[i,]<-length(which(et[[j]][i,]<rad,arr.ind=TRUE))
  }
  ct[[j]]<-dct
}
b<-a[-1]
for (i in 1:(ae-1))
{
  colnames(ct[[i]])<-b[i]
}
final<-cbind(agglistorig[[1]],ct)
setwd(outputsource)
write.csv(final,outputfile)
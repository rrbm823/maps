outputsource<-"C:\\Users\\Erik J\\Documents\\R\\output"
outputfile<-"airlinecompilation.csv"
inputsource<-"C:\\Users\\Erik J\\Desktop\\AggData\\Subversion\\Store Locations\\Airlines"
mastersource<-"C:\\Users\\Erik J\\Desktop\\AggData\\Subversion\\Custom"
masterfile<-"airport_data.csv"
setwd(mastersource)
airdata<-read.csv(masterfile)
la<-dim(airdata)[1]
#########################
setwd(inputsource)
a<-list.files(path=inputsource,pattern=".csv",recursive=FALSE)
ae<-length(a)
airlines<-do.call("list", lapply(a, read.csv, header = TRUE))
alllines<-matrix(1:(la*(ae)),la)
for(i in 1:ae)
{
	airline1<-as.matrix(airlines[[i]])
	dups<-which(duplicated(airline1[,1]))
	airline2<-airline1[c(-1,-dups),]
	lal<-dim(airline2)[1]
	result<-merge(airline2[,1],airdata[,1],by.x=1,by.y=1)
	dummy<-matrix("YES",dim(result)[1])
	dumresult<-cbind(result,dummy)
	masterresult<-merge(airdata[,1],dumresult,all.x=TRUE,by.y=1)
	alllines[,i]<-masterresult[,2]
}
final<-cbind(airdata,alllines)
names<-colnames(airdata)
allnames<-c(names,a)
colnames(final)<-allnames
setwd(outputsource)
write.csv(final,outputfile)
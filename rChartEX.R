### this is a simple script to merge a store list with some race demographics and generating d3 chart via rCharts
if(!require(devtools)) install.packages("devtools")
install_github("rCharts", "ramnathv")

##read the demographics data in
dadata = read.csv("~/GitHub/maps/zip-codes-database-DELUXE.csv", stringsAsFactors = F)

#mean(is.na(dadata)) #proportion of missing data in the dataset

formula = as.formula(cbind(WhitePopulation,HispanicPopulation,BlackPopulation,AsianPopulation)~ZipCode)

##this is some of numeric data in that csv file averaged together by zips
aggdata = aggregate(formula, dadata, FUN = mean, na.action = na.omit)
# mean(is.na(aggdata))

##merge it with a store.. so get to the appropriate wd and search a store name

category = "Shopping" #this would be an input variable

setwd(paste0("C:/Egnyte/Shared/premium/", category))

store = "brookstone" #another input var

stores = list.files(pattern = store, recursive = T)

##loop over this stores list and apply a merge and combine it to a dataframe
if(!require(plyr)) install.packages("plyr")
comb.data = ldply(stores, function(i){
  temp = read.csv(i, stringsAsFactors = F)
  temp$date = gsub(".csv", "", strsplit(i, "_")[[1]])[2]
  merge(temp, aggdata, by.x = "Zip.Code", by.y = "ZipCode", all.x = T)
})
# mean(is.na(comb.data))
comb.data[is.na(comb.data)] = 0
comb.data$date = as.POSIXct(as.Date(comb.data$date, format = "%m-%d-%y"))
formula2 = as.formula(cbind(WhitePopulation,HispanicPopulation,BlackPopulation,AsianPopulation)~date)
aggdata2 = aggregate(formula2, comb.data, FUN = sum, na.action = na.omit)
if(!require(reshape2)) install.packages("reshape2")
aggdata2 = transform(aggdata2, date = as.character(date))

m <- mPlot(x = "date", y = paste0(c("White", "Black", "Hispanic", "Asian"), "Population"), data = aggdata2, type = "Line")
m$print(paste(store, "demographics"))


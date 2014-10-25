raceChart <- function(store = "", category = ""){
  oowd <- getwd()
  library(rCharts)
  library(dplyr)
  
  ##read the demographics data in
  #aggdata = read.csv("~/GitHub/maps/aggdata.csv", stringsAsFactors = F)
  
  #mean(is.na(dadata)) #proportion of missing data in the dataset
  
  ##merge it with a store.. so get to the appropriate wd and search a store name
  
  #category = "Shopping" #this would be an input variable
  
  setwd(paste0("C:/Egnyte/Shared/premium/", category))

  #store = "brookstone" #another input var
  
  stores = list.files(pattern = store, recursive = T)
  stores = stores[-grep("archives", stores)]
  
  
  
  
  
  ##loop over this stores list and read in files and combine it to a dataframe
  library(plyr)
  allstoredata = ldply(stores, function(i){
    cat(i, "\n")
    temp = tryCatch(read.csv(i, stringsAsFactors = F))
    if(!inherits(temp, "error")){
      n = names(temp) 
      z = grep("[Zz]ip", n)
      lat = grep("[Ll]atitude", n)
      lon = grep("[Ll]ongitude", n)
      if(length(z) == 0 || length(lat) == 0 || length(lon) == 0) return()
      names(temp)[z] <- "Zip"
      names(temp)[lat] <- "Latitude"
      names(temp)[lon] <- "Longitude"
      temp[,z] = gsub("-\\d*", "", temp[,z])
      temp$date = gsub("[^0-9-]*", "", i)
      temp$name = gsub(".*/(?=[^/]*)|_[0-9-]*.csv$", "", i, perl=T)
      temp$category = gsub("/.*", "", i)
      c = grep("category", n)
      return(temp[,1:c])
    } else return()
  })
  
  setwd(oowd)
  
  if(nrow(allstoredata) == 0) stop("no match")

  comb_data = merge(allstoredata, aggdata, by.x = "Zip.Code", by.y = "ZipCode")
      
  comb_data$date = as.POSIXct(as.Date(comb_data$date, format = "%m-%d-%y"))
  formula2 = as.formula(cbind(WhitePopulation,HispanicPopulation,BlackPopulation,AsianPopulation)~date)
  if(nrow(comb_data) == 0) stop("no match")
  aggdata_out = data.frame(name = unique(comb_data$name), WhitePopulation = NA,HispanicPopulation = NA,BlackPopulation = NA,AsianPopulation = NA)
  for(store_name in unique(comb_data$name)){
    comb_data.named = comb_data[comb_data$name == store_name,]
    if(!any(is.na(comb_data.named$date))){
      aggdata2 = aggregate(formula2, comb_data.named, FUN = sum, na.action = na.omit)
      if(!require(reshape2)) install.packages("reshape2")
      #aggdata2 = transform(aggdata2, date = as.character(date))
      agg_avg = apply(aggdata2[,-1], 2, mean)
      aggdata_out[aggdata_out$name == store_name,-1] = agg_avg
    }
    
  }
    #return(nrow(aggdata2))
#     m <- mPlot(x = "date",
#                y = paste0(c("White", "Black", "Hispanic", "Asian"), "Population"),
#                data = aggdata2,
#                type = "Line")
    #m$print(paste(name, "demographics"))
  setwd(oowd)
    #assign(paste0("m_", store_name), m, envir = globalenv())

  return(aggdata_out)
    
}
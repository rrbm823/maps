
retrieveCSV <- function(store){
  CSVlocation <- paste0("http://hathawayhome.net/scrapes/Store%20Locations/", store)
  #CSVlocation <- paste0("http://", username, ":", password, "@hathawayhome.net/scrapes/Store%20Locations/", store, ".csv")
  lines <- getURL(CSVlocation, userpwd = paste0("robert", ":", "r0bert!"))
  data <- read.csv(text = lines, header = T, stringsAsFactors = F)
  #data <- read.csv(CSVlocation, header = T, stringsAsFactors = F)
  #if(length(grep("[Ll]atitude", names(data))) == 0) data <- read.csv(text = lines, header = F, stringsAsFactors = F)
  return(data)
}

require(httr)
username = "robert"
password = "r0bert!"
  retrieveCSV <- function(store){
    response <- download.file(paste0("http://", username, ":", password, "@hathawayhome.net/scrapes/Store%20Locations/", store, ".csv"), paste0(store, ".csv"))
    inCSV <- read.csv(paste0(store, ".csv"), header = T, stringsAsFactors = F)
    if(!is.element("latitude", names(inCSV)) && !is.element("Latitude", names(inCSV))) inCSV <- read.csv(paste0(store, ".csv"), header = F, stringsAsFactors = F)
    return(inCSV)
  }

targets <- read.csv("AStargets.csv", header = F, stringsAsFactors = F)
da_urls <- targets[,2]
cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
autoScraper <- function(a_url,
                        rgxInput,
                        env = parent.frame(),
                        recurse = F,
                        debug = F){
  require(httr)
  require(XML)
  page <- tryCatch(GET(a_url, cainfo = cafile), error = function(e) e)
  if(inherits(page, "error")) return()
  tree <- xmlRoot(htmlTreeParse(page, useInternalNodes = T))
  
  links <- getHTMLLinks(tree)
  has_http <- grep("http", links)
  if(!exists("linklist", where = 1)) linklist <<- list()
  newlinks <- c(paste0(a_url, links[-has_http]), links[has_http])
  oldlinks <- get("linklist", pos = 1)
  newlinks <- newlinks[!newlinks %in% oldlinks]
  linklist <<- append(oldlinks, newlinks)
  
  climbTree(tree, rgx = rgxInput, debug = debug)
#   if(length(outvalue) > 0){
#     cat(outvalue, "\r\n")
#     user.resp <- NA
#     while(is.na(user.resp)){
#       user.resp <- readline("do you want to remove outvalues? y/n: \n")
#       if(grepl("y", user.resp)) rm(outvalue, pos = 1)
#     }
#   }
  if(recurse){
    user.resp <- NA
    while(is.na(user.resp)){
      user.resp <- readline("do you want to scrape linklist pages? y/n: \n")
      if(grepl("y", user.resp)) sapply(linklist,autoScraper,rgxInput,env = new.env())
    }
  }
}

addresses <- lapply(1:nrow(targets), function(i){
  autoScraper(targets[i,2], rgxInput = rgxLib("commaState")[[1]], recurse = T)
  list(name = targets[i,1], url = targets[i,2], address = outvalue)
  rm(outvalue, pos = 1)
  rm(outlist, pos = 1)
  rm(linklist, pos = 1)
})
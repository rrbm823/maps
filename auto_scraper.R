#targets <- read.csv("~/Github/maps/AStargets.csv", header = F, stringsAsFactors = F)
#targets <- addresses[,2]
#targets <- grep("http", targets, value = T)
source("~/Github/maps/docSearch.R")
citystates <- read.csv("~/Github/maps/citystates.csv", header = T)[,c("City", "State")]
cities <- unique(unlist(strsplit(as.character(citystates[,"City"]), " ")))
states <- unique(as.character(citystates[,"State"]))
a_url = "http://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number"
cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
searchin_words <- c("store", "location")

autoScraper <- function(a_url,
                        rgxInput = NULL,
                        noms = c(),
                        justGetLinks = F,
                        xpath = NULL,
                        env = parent.frame(),
                        json = F,
                        recurse = F,
                        debug = F){
  require(httr)
  require(XML)
  cat("Connecting to: ", a_url, "\r\n")
  cfg = list();
  if(json) cfg = accept_json();
  page <- tryCatch(GET(a_url, cainfo = cafile), error = function(e) e)
  if(inherits(page, "error")) return()
  tree <- xmlRoot(htmlTreeParse(page, useInternalNodes = T))
  links <- getHTMLLinks(tree)
  if(justGetLinks) return(links)
  if(!is.null(xpath)) {
    xpath_matched <- xpathApply(tree, xpath)
    cat(xpath_matched, "\r\n")
    for(i in xpath_matched){
      climbTree(i, rgx = rgxInput, nms = noms, debug = debug)
    }
  }
  else {
    climbTree(tree, rgx = rgxInput, nms = noms, debug = debug)
  }

  if(recurse){    
    has_http <- grep("http", links)
    if(!exists("linklist", where = 1)) linklist <<- list()
    newlinks <- c(paste0(a_url, links[-has_http]), links[has_http])
    oldlinks <- get("linklist", pos = 1)
    newlinks <- newlinks[!newlinks %in% oldlinks]
    linklist <<- append(oldlinks, newlinks)
    sl_links <- unlist(lapply(searchin_words, function(i) {
      link <- grep(i, linklist, value = T)
      ifelse(is.character(link), link, NULL)
    }))
    sapply(sl_links[!is.na(sl_links)], 
           autoScraper, 
           rgxInput = rgxInput,
           rgxInput2 = rgxInput2,
           debug = debug,
           env = new.env())
    ##Ask to scrape each link
#     user.resp <- NA
#     while(is.na(user.resp)){
#       user.resp <- readline("do you want to scrape external links? y/n: \n")
#       if(grepl("y", user.resp)) sapply(linklist,autoScraper,rgxInput,env = new.env())
#     }
  }
  outdf$url <<- a_url

}

# library(plyr)
# addresses2 <- ldply(1:nrow(targets), function(i){
#   autoScraper(targets[i,2], 
#               rgxInput = rgxLib("commaState"), 
#               rgxInput2 = rgxLib("phone"), 
#               recurse = T, 
#               debug = F)
#   outdf$name <- targets[i,1]
#   out <- outdf
# 
#   rm(linklist, pos = 1)
#   rm(outdf, pos = 1)
#   return(out)
# 
# })

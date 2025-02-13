
egnyteCSV <- function(store_name, subdir = "3 Month Updates") paste0("C:/Egnyte/Shared/Main/", subdir, "/", store_name, ".csv")
egnyteSSS <- function(store_name, subdir = "3 Month Updates") paste0("C:/Egnyte/Shared/Main/", subdir, "/", store_name, " (Scraping Session).sss")




readScrape <- function(scrape_name, subdir = "3 Month Updates", xpath = NULL, internal = TRUE){
  require(XML)
  ParsedScrape <- xmlTreeParse(egnyteSSS(scrape_name, subdir), useInternalNodes = internal)
  TreeScrape <- xmlRoot(ParsedScrape)
  
  if(length(xpath) > 0){
    print(xpathApply(TreeScrape, xpath))
  }
  return(TreeScrape)
        
}


getScrapefiles <- function(scrape_node, all = F, which_scrapefile = 1) {
  if(all)   return(xpathSApply(scrape_node, "//scrapeable-files" ))
  n <- length(xmlChildren(scrape_node))
  a <- which(sapply(1:n, function(i) xmlName(scrape_node[[i]])) == "scrapeable-files")
  q <- a[which_scrapefile]
  return(list(scrape_node[[q]], q))
}

getExtPattern <- function(scrape_node,
                          all = F,
                          which_scrapefile = 1,
                          which_ext = 1) 
  {
  if(all)       return(xpathSApply(scrape_node, "//extractor-patterns" ))
  q <- getScrapefiles(scrape_node, which_scrapefile = which_scrapefile)[[2]]
  n <- length(xmlChildren(scrape_node[[q]]))
  a <- which(sapply(1:n, function(i) xmlName(scrape_node[[q]][[i]])) == "extractor-patterns")
  
  return(list(scrape_node[[q]][[a[which_ext]]],c(q,a[which_ext])))
}

scrapeEditor <- function(scrape_name,
                         rgxInput = NULL,
                         addSubs = TRUE,
                         userRGX = NULL,
                         userTokens = NULL,
                         EP.positions = NULL,
                         save = FALSE){
  scrape <- readScrape(scrape_name)

  XMLd <- F  
  if(is.null(userTokens)){
    if(is.null(rgxInput)) stop("enter rgx opt")
    rgx <- rgxLib(rgxInput)[[1]]
    n <- length(xmlChildren(scrape))
    a <- which(sapply(1:n, function(i) xmlName(scrape[[i]])) == "scrapeable-files")
    for(i in a){
      o <- length(xmlChildren(scrape[[i]]))
      b <- which(sapply(1:o, function(x) xmlName(scrape[[i]][[x]])) == "extractor-patterns")
      for(j in b){
        p <- length(xmlChildren(scrape[[i]][[j]]))
        c <- which(sapply(1:p, function(y) xmlName(scrape[[i]][[j]][[y]])) == "pattern-text")
        for(k in c){
          patterntext <- xmlValue(scrape[[i]][[j]][[k]], recursive = FALSE)
          if(rgxInput == "json"){
            require(jsonlite)
            possjson <- validate(patterntext)
            if(possjson) {
              cat("json found: ", patterntext, "\r\n")
              EP.pos <- c(i,j)
              EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, perl = T)
            }
          } else{
            XMLd <- T
            possxml <- length(grep(rgxLib("xmlStringRGX")[[1]], patterntext)) > 0
            if(possxml) {
              cat("xml found: ", patterntext, "\r\n")
              EP.pos <- c(i,j)
              EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, perl = T)
            }
          }
        }
      }
    }
  
  
    if(!is.null(userRGX)) rgx <- userRGX
    
    cat(EPwithTokens, "\r\n")
    xmlValue(scrape[[EP.pos[1]]][[EP.pos[2]]][[1]]) <- EPwithTokens
    tokens <- unlist(strsplit(EPwithTokens, ","))
    if(XMLd) tokens <- unlist(strsplit(EPwithTokens, "><"))
  }
  
  if(!is.null(EP.positions)) EP.pos <- EP.positions
  
  if(addSubs){
    if(!is.null(userTokens)) tokens <- userTokens
    
    subExtPattern <- xmlRoot(xmlInternalTreeParse('<extractor-patterns sequence="1" automatically-save-in-session-variable="false" if-saved-in-session-variable="0" filter-duplicates="false" cache-data-set="false" will-be-invoked-manually="false"><pattern-text>pattern text</pattern-text><script-instances/></extractor-patterns>'))
    subexts <- lapply(1:length(tokens), function(i) {
      newSubExt <- xmlClone(subExtPattern)
      xmlValue(newSubExt[[1]][[1]]) <- tokens[i]
      xmlAttrs(newSubExt)[1] <- as.character(i)
      newSubExt
    })
  
    addChildren(scrape[[EP.pos[1]]][[EP.pos[2]]], kids = subexts)
  }
  if(save) saveXML(scrape, file = egnyteSSS(scrape_name))
  #return(EPwithTokens)

}


QA <- function(store_name, subdir = "3 Month Updates"){
  newversion <- read.csv(paste0("C:/Program Files/screen-scraper/", store_name, ".csv"),header = F)
  oldversion <- read.csv(egnyteCSV(store_name, subdir), header = F)
  oldversion <- oldversion[!duplicated(oldversion),]
  newversion <- newversion[!duplicated(newversion),]
  cat("old rows:", nrow(oldversion), "\nnew rows:", nrow(newversion), "\n")
  str(oldversion)
  str(newversion)
#  a <- readline("write out timestamp: (y/n)")
#   if(a == "y"){
#     atrium <- read.csv("atrium_cases.csv", header = F)
#     store_name <- gsub("_", " ", store_name)
#     i <- grep(store_name, atrium[,2], ignore.case = T)
#     out <- data.frame(store_name, as.character(atrium[i,1]), Sys.time())
#     write.table(out, "C:/Program Files/screen-scraper/cases_to_post.csv", sep = ",", row.names = F, col.names = F, append = TRUE)
# 
#   }
}

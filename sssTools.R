
egnyteCSV <- function(store_name, subdir = "3 Month Updates") paste0("C:/Egnyte/Shared/Main/", subdir, "/", store_name, ".csv")
egnyteSSS <- function(store_name, subdir = "3 Month Updates") paste0("C:/Egnyte/Shared/Main/", subdir, "/", store_name, " (Scraping Session).sss")




readScrape <- function(scrape_name, subdir = "3 Month Updates", xpath = NULL, internal = TRUE, ...){
  require(XML)
  ParsedScrape <- xmlTreeParse(egnyteSSS(scrape_name, subdir), useInternalNodes = internal)
  TreeScrape <- xmlRoot(ParsedScrape)
  
  if(length(xpath) > 0){
    return(xpathSApply(TreeScrape, xpath, ...))
  }
  return(TreeScrape)
        
}


getScrapefiles <- function(scrape_node, all = F, which_scrapefile = 1) {
  sfs <- xmlSApply(scrape_node, function(i) xmlName(i) == "scrapeable-files" )
  if(all) return(xmlChildren(scrape_node)[which(sfs)]) else return(xmlChildren(scrape_node)[which(sfs)][which_scrapefile])
}

getExtPattern <- function(scrape_node,
                          all = F,
                          which_scrapefile = 1,
                          which_ext = 1) 
  {  
  scrape_file <- getScrapefiles(scrape_node, which_scrapefile = which_scrapefile)[[1]]
  all_eps <- xpathApply(scrape_node, "//extractor-patterns")
  eps <- xmlSApply(scrape_file, function(i) xmlName(i) == "extractor-patterns" )
  if(all) return(all_eps) else return(scrape_file[which(eps)][[which_ext]]) 
  
}

scrapeEditor <- function(scrape_name,
                         subdir = "3 Month Updates",
                         rgxInput = NULL,
                         addSubs = TRUE,
                         userRGX = NULL,
                         userSplit = NULL,
                         userTokens = NULL,
                         save = FALSE){
  
  scrape <- readScrape(scrape_name, subdir)

  XMLd <- F  
  if(is.null(rgxInput)) stop("enter rgx opt")
  rgx <- rgxLib(rgxInput)[[1]]
  if(!is.null(userRGX)) rgx <- userRGX
  pt <- getNodeSet(scrape, "//scrapeable-files/extractor-patterns/pattern-text")          
  for(node in pt){
    ep <- xmlParent(node)
    patterntext <- xmlValue(node)
    cat(patterntext)
    if(rgxInput == "json"){
      library(jsonlite)
      possjson <- validate(patterntext)
      if(possjson) {
        cat("json found: ", patterntext, "\r\n")
        EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, perl = T)
        break
      }
    } else {
      XMLd <- T
      possxml <- length(grep(rgx[1], patterntext)) > 0
      cat(possxml)
      if(possxml) {
        cat("xml found: ", patterntext, "\r\n")
        EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, perl = T)
      }
    }
}
  cat(EPwithTokens, "\r\n")
  xmlValue(node) <- EPwithTokens
  tokens <- unlist(strsplit(EPwithTokens, ","))
  if(XMLd) tokens <- unlist(strsplit(EPwithTokens, "><"))
  if(!is.null(userSplit)) tokens <- unlist(strsplit(EPwithTokens, userSplit))
  print(tokens)
  if(addSubs){
    if(!is.null(userTokens)) tokens <- userTokens
    
    subExtPattern <- xmlRoot(xmlInternalTreeParse('<extractor-patterns sequence="1" automatically-save-in-session-variable="false" if-saved-in-session-variable="0" filter-duplicates="false" cache-data-set="false" will-be-invoked-manually="false"><pattern-text>pattern text</pattern-text><script-instances/></extractor-patterns>'))
    subexts <- lapply(1:length(tokens), function(i) {
      newSubExt <- xmlClone(subExtPattern)
      xmlValue(newSubExt[[1]][[1]]) <- tokens[i]
      xmlAttrs(newSubExt)[1] <- as.character(i)
      newSubExt
    })
  
    addChildren(ep, kids = subexts)
  }

  if(save) saveXML(scrape, file = egnyteSSS(scrape_name, subdir))
}


QA <- function(store_name, subdir = "3 Month Updates", plot = F, bind = F, stringsAsFactors = F){
  newversion <- read.csv(paste0("C:/Program Files/screen-scraper/", store_name, ".csv"),header = F, stringsAsFactors = stringsAsFactors)
  oldversion <- read.csv(egnyteCSV(store_name, subdir), header = F, stringsAsFactors = stringsAsFactors)
  oldversion <- oldversion[!duplicated(oldversion),]
  newversion <- newversion[!duplicated(newversion),]
  if(bind)write.csv(rbind(oldversion, newversion), "weight_watchers.csv", row.names = F)
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
  if(plot){
    library(maps)
    library(maptools)
    library(spatstat)
    oopar <- par()
    par(mfrow = c(2,1))
    newcol <- ncol(newversion)
    oldcol <- ncol(oldversion)
    oldversion[,oldcol] <- as.numeric(oldversion[,oldcol])
    oldversion[,oldcol-1] <- as.numeric(oldversion[,oldcol-1])
    newlong <- max(which(sapply(1:newcol, function(i){
      class(newversion[,i]) == "numeric"
      })))
    world <- map('world', plot=FALSE)
    us <- map('usa', plot=FALSE)
    #polys <- map2SpatialPolygons(us, IDs=us$names, proj4string=CRS("+proj=longlat +datum=WGS84"))
    #usowin <- as.owin.SpatialPolygons(polys)
    #old_pts <- ppp(oldversion[,oldcol], oldversion[,oldcol - 1], window = usowin)
    #new_pts <- ppp(newversion[,newlong], newversion[,newlong - 1], window = usowin)
    old_pts <- ppp(oldversion[,oldcol], oldversion[,oldcol - 1])
    new_pts <- ppp(newversion[,newlong], newversion[,newlong - 1])
    plot(density(old_pts, 1))
    plot(density(new_pts, 1))
    par(mfrow = oopar$mfrow)
  }
}

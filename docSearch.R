

climbTree <- function(tree, 
                      rgx = rgxLib("commaState"),            
                      nodeNumber = c(1),
                      reverseSearch = F,
                      debug = F,
                      nms = c(),
                      env = parent.frame()){
  if(!exists("outdf", where = 1)) outdf <<- data.frame(stringsAsFactors = F)
  n <- xmlSize(tree)
  nn <- xmlApply(tree, xmlName)
  if(n > 0){
    nodeValue <- xmlValue(tree, recursive = F)
    notJustSpace <- length(grep("[[:punct:]]|[[:alnum:]]", nodeValue)) > 0
    if(debug && notJustSpace) {
      cat("Node Value is: ", nodeValue, "\r\n")
    }
    
    ##begin substate
    if(reverseSearch){
      tree <- xmlParent(tree)
      b <- grep(as.character(rgx[[1]][1]), nodeValue)
    }
    
    b <- grep(as.character(rgx[[1]][1]), nodeValue)
    
    
    
    if(length(b) > 0) {
      if(debug) cat("MATCHED: \r\n")
      out <- rep(NA, length(nms))
      out[1] <- nodeValue
      
      
      if(length(nms) > 1){
        matched = FALSE
        for(i in 2:length(names)){
          #apply regex to children nodes
          nodeValues <- xmlApply(tree, xmlValue, recursive = F)
          for(j in nodeValues){
            if(debug) cat("\t", j, "\r\n")
            matched <- grep(as.character(rgx[[i]][1]), j)
            if(length(matched) > 0) {
              if(debug) cat("\t", "matches", "\r\n") 
              out[i] <- nodeValue
              break
            }
          }
        }
      }
      
        
      names(out) <- nms
      if(debug) cat(paste0(out, collapse = "\r\n"), "\r\n")
      outdf <<- rbind(get("outdf", pos = 1), out)
      names(outdf) <<- nms
    }
    sapply(1:n, function(i){
      climbTree(tree[[i]], rgx = rgx, nodeNumber = append(nodeNumber,i), env = new.env(), debug = debug, nms = nms)
    })
  } 
  outdf
  return("end")
}

downSearch <- function(tree, rgx){}
upSearch
rgxLib <- function(opt = c("json", "XML", "XMLattrs", "addressRGX", "xmlStringRGX"), addThis = NULL){
  rgxList <- as.list(read.csv("~/Github/maps/searches.csv", stringsAsFactors = F))
  if(!is.null(addThis)) {
    rgxList <- append(rgxList, addThis)
    write.csv(as.data.frame(rgxList), "searches.csv", row.names = F)
  }
  if(opt == "all") return(rgxList)
  return(rgxList[opt])
  
}

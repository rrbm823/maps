climbTree <- function(tree, 
                      nodeNames = NULL, 
                      rgx = rgxLib("json")[[1]], 
                      userRGX = NULL,
                      nodeNumber = c(1),
                      debug = F,
                      env = parent.frame()){
  if(!exists("outlist", where = 1)) outlist <<- list()
  if(!is.null(userRGX)) rgx <- userRGX
  n <- xmlSize(tree)
  if(debug) cat("Number of Children: ", n, "\r\n")
  nodeName <- xmlName(tree)
  if(length(which(nodeName %in% nodeNames)) > 0) cat("matched name at node # ", nodeNumber, "\r\n\t", nodeName, "\r\n")
  if(n > 0){
    nodeValue <- xmlValue(tree, recursive = F)
    if(debug) cat("Node Value is: ", nodeValue, "\r\n")
    b <- length(grep(as.character(rgx[[1]][1]), nodeValue))
    if(b > 0) {
      outlist <<- append(get("outlist", pos = 1), tree)
      #cat("value matched rgx at node # ", nodeNumber, nodeValue, sep = "\r\n\t")
    }
    sapply(1:n, function(i){
      climbTree(tree[[i]], nodeNames, rgx = rgx, nodeNumber = append(nodeNumber,i), env = new.env(), debug = debug)
    })
  } 
  outvalue <<- sapply(outlist, xmlValue)
  
  return("end")
}


rgxLib <- function(opt = c("json", "XML", "XMLattrs", "addressRGX", "xmlStringRGX"), addThis = NULL){
  rgxList <- as.list(read.csv("~/Github/maps/searches.csv", stringsAsFactors = F))
  if(!is.null(addThis)) {
    rgxList <- append(rgxList, addThis)
    write.csv(as.data.frame(rgxList), "searches.csv", row.names = F)
  }
  if(opt == "all") return(rgxList)
  return(rgxList[opt])
  
}

tokensReplacer <- function(tree, rgx, attrs = FALSE, asText = F){
  if(attrs){
    attrList <- xmlAttrs(tree)
    token.names <- names(attrList)
    tokens <- lapply(token.names, function(i) paste0(i, '="~@', toupper(i), '@~"'))
  } else {
    sub <- rgxLib(rgx)[[1]]
    tokens <- gsub(sub[1],sub[2],
                   ifelse(asText, tree, xmlValue(tree, recursive = F)),
                   perl = T)
  }
  return(tokens)
  
                    
  
}
climbTree <- function(tree, 
                      nodeNames = NULL, 
                      rgx = rgxLib("json")[[1]], 
                      nodeNumber = c(1), 
                      env = parent.frame()){
  if(!exists("outlist", where = 1)) outlist <<- list()
  n <- xmlSize(tree)
  if(n > 0)cat(rep("/", n), "\r\n") else cat("--", nodeNumber, "\r\n\t")
  nodeName <- xmlName(tree)
  if(length(which(nodeName %in% nodeNames)) > 0) cat("matched name at node # ", nodeNumber, "\r\n\t", nodeName, "\r\n")
  if(n > 0){
    nodeValue <- xmlValue(tree, recursive = F)
    b <- length(grep(as.character(rgx), nodeValue))
    if(b > 0) {
      outlist <<- append(get("outlist", pos = 1), tree)
      #cat("value matched rgx at node # ", nodeNumber, nodeValue, sep = "\r\n\t")
    }
    sapply(1:n, function(i){
      climbTree(tree[[i]], nodeNames, rgx = rgx, nodeNumber = append(nodeNumber,i), env = new.env())
    })
  } 
  outvalue <<- sapply(outlist, xmlValue)
  
  return("end")
}


rgxLib <- function(opt = c("json", "XML", "XMLattrs", "addressRGX", "xmlStringRGX"), addThis = NULL){
  rgxList <- as.list(read.csv("searches.csv", stringsAsFactors = F))
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
      

findJSONinTree <- function(tree, attr = NULL, addChild = NULL, env = parent.frame()){
  require(jsonlite)
  checkAttr <- is.null(attr)
  validate_this <- xmlValue(tree, recursive = FALSE)
  if(!checkAttr){
    validate_this <- xmlGetAttr(tree, attr)
  }
  if(validate(validate_this)){
    validated <- validate_this
    ## pull out this value from the tree
    assign("ab", validated, env = globalenv())
    tokens <- unlist(strsplit(tokensReplacer(tree, "json")[[1]], ","))
    subExtPattern <- xmlRoot(xmlInternalTreeParse('<extractor-patterns sequence="1" automatically-save-in-session-variable="false" if-saved-in-session-variable="0" filter-duplicates="false" cache-data-set="false" will-be-invoked-manually="false"><pattern-text>pattern text</pattern-text><script-instances/></extractor-patterns>'))
    subexts <- lapply(1:length(tokens), function(i) {
      newSubExt <- xmlClone(subExtPattern)
      xmlValue(newSubExt[[1]][[1]]) <- tokens[i]
      xmlAttrs(newSubExt)[1] <- as.character(i)
      newSubExt
    })
    assign("aba", subexts, env = globalenv())
    
  } else{
    n <- xmlSize(tree)
    if(n == 0) return()
    cat(xmlValue(tree, recursive = F), "\r\n")
    xmlSApply(tree, function(i) findJSONinTree(i, attr = attr, addChild = addChild, env = new.env()))
    #return(n)    
  }
}

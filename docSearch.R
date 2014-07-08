climbTree <- function(tree, 
                      nodeNames = NULL, 
                      rgx = rgxLib("json")[[1]], 
                      nodeNumber = 1, 
                      env = parent.frame()){
  n <- xmlSize(tree)
  nodeName <- xmlName(tree)
  if(length(which(nodeName %in% nodeNames)) > 0) cat("matched name at node # ", nodeNumber, "\r\n\t", nodeName, "\r\n")
  if(n > 0){
    nodeValue <- xmlValue(tree, recursive = F)
    b <- length(grep(rgx[1], nodeValue))
    if(b > 0) cat("\t\tvalue matched rgx at node # ", nodeNumber, "\r\n\t", nodeValue, "\r\n")
    sapply(1:n, function(i){
      climbTree(tree[[i]], nodeNames, rgx = rgx, nodeNumber = i, env = new.env())
    })
  } 
  return("end ")
}


rgxLib <- function(opt = c("json", "XML", "XMLattrs", "address", "xmlString")){
  
  jsonRGX <- c('\\"([^\\"]+)\\":([\\s\\"]*)([^\\"]*)([\\s\\"]*),', '\\"\\1\\":\\2~@\\U\\1\\E@~\\4,')
  xmlRGX <- c('<([^>]+)>[^<]*</([^>]+)>', '<\\1>~@\\U\\1\\E@~</\\1>')
  xmlattrRGX <- c('([^=\\s]+)="([^"]*)"', '\\1="~@\\U\\1\\E@~"')
  addressRGX <- c("^[0-9]+\\s\\D*$")
  xmlStringRGX <- c("<([a-zA-Z]+:)?[a-zA-Z]+(/?>| [a-zA-Z]+=[\"'])")
  
  rgxList <- list(jsonRGX, xmlRGX, xmlattrRGX, addressRGX, xmlStringRGX)
  rgx <- rgxList[which(c("json", "XML", "XMLattrs", "address", "xmlString") %in% opt)]
  return(rgx)
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

\name{scrapeEditor}
\alias{scrapeEditor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scrapeEditor(scrape_name, rgxInput = NULL, addSubs = TRUE, userRGX = NULL, userTokens = NULL, EP.positions = NULL, save = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scrape_name}{
%%     ~~Describe \code{scrape_name} here~~
}
  \item{rgxInput}{
%%     ~~Describe \code{rgxInput} here~~
}
  \item{addSubs}{
%%     ~~Describe \code{addSubs} here~~
}
  \item{userRGX}{
%%     ~~Describe \code{userRGX} here~~
}
  \item{userTokens}{
%%     ~~Describe \code{userTokens} here~~
}
  \item{EP.positions}{
%%     ~~Describe \code{EP.positions} here~~
}
  \item{save}{
%%     ~~Describe \code{save} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (scrape_name, rgxInput = NULL, addSubs = TRUE, userRGX = NULL, 
    userTokens = NULL, EP.positions = NULL, save = FALSE) 
{
    scrape <- readScrape(scrape_name)
    XMLd <- F
    if (is.null(userTokens)) {
        if (is.null(rgxInput)) 
            stop("enter rgx opt")
        rgx <- rgxLib(rgxInput)[[1]]
        n <- length(xmlChildren(scrape))
        a <- which(sapply(1:n, function(i) xmlName(scrape[[i]])) == 
            "scrapeable-files")
        for (i in a) {
            o <- length(xmlChildren(scrape[[i]]))
            b <- which(sapply(1:o, function(x) xmlName(scrape[[i]][[x]])) == 
                "extractor-patterns")
            for (j in b) {
                p <- length(xmlChildren(scrape[[i]][[j]]))
                c <- which(sapply(1:p, function(y) xmlName(scrape[[i]][[j]][[y]])) == 
                  "pattern-text")
                for (k in c) {
                  patterntext <- xmlValue(scrape[[i]][[j]][[k]], 
                    recursive = FALSE)
                  if (rgxInput == "json") {
                    require(jsonlite)
                    possjson <- validate(patterntext)
                    if (possjson) {
                      cat("json found: ", patterntext, "\r\n")
                      EP.pos <- c(i, j)
                      EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, 
                        perl = T)
                    }
                  }
                  else {
                    XMLd <- T
                    possxml <- length(grep(rgxLib("xmlStringRGX")[[1]], 
                      patterntext)) > 0
                    if (possxml) {
                      cat("xml found: ", patterntext, "\r\n")
                      EP.pos <- c(i, j)
                      EPwithTokens <- gsub(rgx[1], rgx[2], patterntext, 
                        perl = T)
                    }
                  }
                }
            }
        }
        if (!is.null(userRGX)) 
            rgx <- userRGX
        cat(EPwithTokens, "\r\n")
        xmlValue(scrape[[EP.pos[1]]][[EP.pos[2]]][[1]]) <- EPwithTokens
        tokens <- unlist(strsplit(EPwithTokens, ","))
        if (XMLd) 
            tokens <- unlist(strsplit(EPwithTokens, "><"))
    }
    if (!is.null(EP.positions)) 
        EP.pos <- EP.positions
    if (addSubs) {
        if (!is.null(userTokens)) 
            tokens <- userTokens
        subExtPattern <- xmlRoot(xmlInternalTreeParse("<extractor-patterns sequence=\"1\" automatically-save-in-session-variable=\"false\" if-saved-in-session-variable=\"0\" filter-duplicates=\"false\" cache-data-set=\"false\" will-be-invoked-manually=\"false\"><pattern-text>pattern text</pattern-text><script-instances/></extractor-patterns>"))
        subexts <- lapply(1:length(tokens), function(i) {
            newSubExt <- xmlClone(subExtPattern)
            xmlValue(newSubExt[[1]][[1]]) <- tokens[i]
            xmlAttrs(newSubExt)[1] <- as.character(i)
            newSubExt
        })
        addChildren(scrape[[EP.pos[1]]][[EP.pos[2]]], kids = subexts)
    }
    if (save) 
        saveXML(scrape, file = egnyteSSS(scrape_name))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

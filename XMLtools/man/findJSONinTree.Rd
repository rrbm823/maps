\name{findJSONinTree}
\alias{findJSONinTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findJSONinTree(tree, attr = NULL, addChild = NULL, env = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{attr}{
%%     ~~Describe \code{attr} here~~
}
  \item{addChild}{
%%     ~~Describe \code{addChild} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
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
function (tree, attr = NULL, addChild = NULL, env = parent.frame()) 
{
    require(jsonlite)
    checkAttr <- is.null(attr)
    validate_this <- xmlValue(tree, recursive = FALSE)
    if (!checkAttr) {
        validate_this <- xmlGetAttr(tree, attr)
    }
    if (validate(validate_this)) {
        validated <- validate_this
        assign("ab", validated, env = globalenv())
        tokens <- unlist(strsplit(tokensReplacer(tree, "json")[[1]], 
            ","))
        subExtPattern <- xmlRoot(xmlInternalTreeParse("<extractor-patterns sequence=\"1\" automatically-save-in-session-variable=\"false\" if-saved-in-session-variable=\"0\" filter-duplicates=\"false\" cache-data-set=\"false\" will-be-invoked-manually=\"false\"><pattern-text>pattern text</pattern-text><script-instances/></extractor-patterns>"))
        subexts <- lapply(1:length(tokens), function(i) {
            newSubExt <- xmlClone(subExtPattern)
            xmlValue(newSubExt[[1]][[1]]) <- tokens[i]
            xmlAttrs(newSubExt)[1] <- as.character(i)
            newSubExt
        })
        assign("aba", subexts, env = globalenv())
    }
    else {
        n <- xmlSize(tree)
        if (n == 0) 
            return()
        cat(xmlValue(tree, recursive = F), "\r\n")
        xmlSApply(tree, function(i) findJSONinTree(i, attr = attr, 
            addChild = addChild, env = new.env()))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

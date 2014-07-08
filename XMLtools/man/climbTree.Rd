\name{climbTree}
\alias{climbTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
climbTree(tree, nodeNames = NULL, rgx = rgxLib("json")[[1]], nodeNumber = 1, env = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{nodeNames}{
%%     ~~Describe \code{nodeNames} here~~
}
  \item{rgx}{
%%     ~~Describe \code{rgx} here~~
}
  \item{nodeNumber}{
%%     ~~Describe \code{nodeNumber} here~~
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
function (tree, nodeNames = NULL, rgx = rgxLib("json")[[1]], 
    nodeNumber = 1, env = parent.frame()) 
{
    n <- xmlSize(tree)
    nodeName <- xmlName(tree)
    if (length(which(nodeName \%in\% nodeNames)) > 0) 
        cat("matched name at node # ", nodeNumber, "\r\n\t", 
            nodeName, "\r\n")
    if (n > 0) {
        nodeValue <- xmlValue(tree, recursive = F)
        b <- length(grep(rgx[1], nodeValue))
        if (b > 0) 
            cat("\t\tvalue matched rgx at node # ", nodeNumber, 
                "\r\n\t", nodeValue, "\r\n")
        sapply(1:n, function(i) {
            climbTree(tree[[i]], nodeNames, rgx = rgx, nodeNumber = i, 
                env = new.env())
        })
    }
    return("end ")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

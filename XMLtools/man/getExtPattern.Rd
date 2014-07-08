\name{getExtPattern}
\alias{getExtPattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getExtPattern(scrape_node, all = F, which_scrapefile = 1, which_ext = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scrape_node}{
%%     ~~Describe \code{scrape_node} here~~
}
  \item{all}{
%%     ~~Describe \code{all} here~~
}
  \item{which_scrapefile}{
%%     ~~Describe \code{which_scrapefile} here~~
}
  \item{which_ext}{
%%     ~~Describe \code{which_ext} here~~
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
function (scrape_node, all = F, which_scrapefile = 1, which_ext = 1) 
{
    if (all) 
        return(xpathSApply(scrape_node, "//extractor-patterns"))
    q <- getScrapefiles(scrape_node, which_scrapefile = which_scrapefile)[[2]]
    n <- length(xmlChildren(scrape_node[[q]]))
    a <- which(sapply(1:n, function(i) xmlName(scrape_node[[q]][[i]])) == 
        "extractor-patterns")
    return(list(scrape_node[[q]][[a[which_ext]]], c(q, a[which_ext])))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{readScrape}
\alias{readScrape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readScrape(scrape_name, subdir = "3 Month Updates", xpath = NULL, internal = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scrape_name}{
%%     ~~Describe \code{scrape_name} here~~
}
  \item{subdir}{
%%     ~~Describe \code{subdir} here~~
}
  \item{xpath}{
%%     ~~Describe \code{xpath} here~~
}
  \item{internal}{
%%     ~~Describe \code{internal} here~~
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
function (scrape_name, subdir = "3 Month Updates", xpath = NULL, 
    internal = TRUE) 
{
    require(XML)
    ParsedScrape <- xmlTreeParse(egnyteSSS(scrape_name, subdir), 
        useInternalNodes = internal)
    TreeScrape <- xmlRoot(ParsedScrape)
    if (length(xpath) > 0) {
        print(xpathApply(TreeScrape, xpath))
    }
    return(TreeScrape)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

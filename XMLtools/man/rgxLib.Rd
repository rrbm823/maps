\name{rgxLib}
\alias{rgxLib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rgxLib(opt = c("json", "XML", "XMLattrs", "address", "xmlString"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opt}{
%%     ~~Describe \code{opt} here~~
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
function (opt = c("json", "XML", "XMLattrs", "address", "xmlString")) 
{
    jsonRGX <- c("\\\"([^\\\"]+)\\\":([\\s\\\"]*)([^\\\"]*)([\\s\\\"]*),", 
        "\\\"\\1\\\":\\2~@\\U\\1\\E@~\\4,")
    xmlRGX <- c("<([^>]+)>[^<]*</([^>]+)>", "<\\1>~@\\U\\1\\E@~</\\1>")
    xmlattrRGX <- c("([^=\\s]+)=\"([^\"]*)\"", "\\1=\"~@\\U\\1\\E@~\"")
    addressRGX <- c("^[0-9]+\\s\\D*$")
    xmlStringRGX <- c("<([a-zA-Z]+:)?[a-zA-Z]+(/?>| [a-zA-Z]+=[\"'])")
    rgxList <- list(jsonRGX, xmlRGX, xmlattrRGX, addressRGX, 
        xmlStringRGX)
    rgx <- rgxList[which(c("json", "XML", "XMLattrs", "address", 
        "xmlString") \%in\% opt)]
    return(rgx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

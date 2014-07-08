\name{tokensReplacer}
\alias{tokensReplacer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tokensReplacer(tree, rgx, attrs = FALSE, asText = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{rgx}{
%%     ~~Describe \code{rgx} here~~
}
  \item{attrs}{
%%     ~~Describe \code{attrs} here~~
}
  \item{asText}{
%%     ~~Describe \code{asText} here~~
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
function (tree, rgx, attrs = FALSE, asText = F) 
{
    if (attrs) {
        attrList <- xmlAttrs(tree)
        token.names <- names(attrList)
        tokens <- lapply(token.names, function(i) paste0(i, "=\"~@", 
            toupper(i), "@~\""))
    }
    else {
        sub <- rgxLib(rgx)[[1]]
        tokens <- gsub(sub[1], sub[2], ifelse(asText, tree, xmlValue(tree, 
            recursive = F)), perl = T)
    }
    return(tokens)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

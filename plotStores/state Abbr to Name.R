stateAbb2Name <- function(i){
  x <- append(state.name, c("Puerto Rico", "Virgin Islands", "Armed Forces Europe", "District of Columbia", "Armed Forces Americas", "Armed Forces Pacific", "American Samoa", "Guam", "Palau", "Federated States of Micronesia", "Northern Mariana Islands", "Marshall Islands"))
  y <- append(state.abb, c("PR", "VI", "AE", "DC", "AA", "AP", "AS", "GU", "PW", "FM", "MP", "MH"))
  if(nchar(i) < 3){
    i <- x[which(is.element(y, i))]
  }else i <- y[which(is.element(x, i))]
  return(i)
}

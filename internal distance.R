internal_distance <- function(store, showGraph = F, directory = "~/aggdata/Shared/Premium/Community & Government"){
  oldwd <- getwd()
  setwd(directory)
  importantstore <- read.csv(paste0(store, ".csv"))
  n <- ncol(importantstore)
  if(class(importantstore[,n]) == "numeric"){
    lats <- importantstore[,n - 1]
    lngs <- importantstore[,n]
  }
  coords <- cbind(lats,lngs)
  c <- coords * pi/180
  de <- nrow(c)
  d <- matrix(1, de)
  for(i in 1:de){
    y <- sqrt((sin((t(c)[1,]-c[i,1])/2)*sin((t(c)[1,]-c[i,1])/2)+(cos(c[i,1])*cos(t(c)[1,])*sin((t(c)[2,]-c[i,2])/2)*sin((t(c)[2,]-c[i,2])/2))))
    x <- sqrt(1-(sin((t(c)[1,]-c[i,1])/2)*sin((t(c)[1,]-c[i,1])/2)+(cos(c[i,1])*cos(t(c)[1,])*sin((t(c)[2,]-c[i,2])/2)*sin((t(c)[2,]-c[i,2])/2))))
    d <- cbind(d, matrix(3963.1676*2*atan2(y,x)))
  }
  out <- cbind(importantstore, d[,-1])
  if(!showGraph) return(out)
  if(nrow(out) > 500) stop("that's too much data")
  g <- graph.full(nrow(d))
  layout <- layout.mds(g, dist = d[,-1])
  plot(g, layout = layout, vertex.size = 3)
  setwd(oldwd)
  return(out)
}



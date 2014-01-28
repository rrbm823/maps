require(plyr)
require(maps)
source("state Abbr to Name.r")
source("addCountiestoCSV.r")
shinyServer(function(input, output) {
  changeData <- reactive({
    min = 0
    max = 100
    color = "red"
    data <- addCounties(tolower(input$store))
    data$Counties <- paste(tolower(laply(data[,2], stateAbb2Name)), tolower(gsub(" County", "", data[,1])), sep=",")
    m <- nrow(data)
    countyPop <- readRDS("counties.RDS")
    countyPop$stores <- 0
    for(i in 1:m){ 
      x <- which(countyPop$name == as.character(data$Counties[i]))
      countyPop$stores[x] <- countyPop$stores[x] + 1
    }
    return(countyPop)
  })
  output$map <- renderPlot(
    if(!is.null(input$store)){ 
      countyPop <- changeData()
      ct <- pmax(countyPop$stores, min)
      ct <- pmin(ct, max)
      pct <- cut(ct, 100, include.lowest = T, ordered = T)
      fills <- colorRampPalette(c("white", color))(100)[pct]
      out <- map("county", fill = TRUE, col = fills, resolution = 0, lty = 0, projection = "polyconic", myborder = 0, mar = c(0,0,0,0))
    })
})
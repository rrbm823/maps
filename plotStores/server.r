require(plyr)
require(maps)
require(mapproj)
require(RCurl)
require(shiny)
source("addCountiesToCSV.R")
shinyServer(function(input, output) {
  reaction <- reactive({
    min = input$range[1]
    max = input$range[2]
    color <- input$color
    store <- input$stores
    #for(i in strsplit())
    data <- addCounties(tolower(store), addDemo = F)
    m <- nrow(data)
    countyPop <- readRDS("counties.RDS")
    countyPop$stores <- 0
    for(i in 1:m){
      x <- which(countyPop$name == as.character(data$County[i]))
      countyPop$stores[x] <- countyPop$stores[x] + 1
    }
    
    at <- pmax(countyPop$stores, min)
    ct <- pmin(at, max)
    pct <- cut(ct, 100, include.lowest = T, ordered = T)
    fills <- colorRampPalette(c("white", color))(100)[pct]
    par(mfrow = c(1,2))
    out <- map("county", fill = TRUE, col = fills, resolution = 0, lty = 0, projection = "polyconic", myborder = 0, mar = c(0,0,0,0))
    out <- map("state", fill = FALSE, add = TRUE, col = "grey60", resolution = 0, lty = 1, lwd = 1, projection = "polyconic")
    if(input$log == "on") pct <- cut(log(ct + 1), 100, include.lowest = T, ordered = T)
    hist(countyPop$stores[-which(countyPop$stores == 0)], xlim = c(min, max), breaks = 100)
  })
  output$map <- renderPlot({
    reaction()
  })
})
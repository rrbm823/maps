require(plyr)
require(maps)
require(mapproj)
require(RCurl)
require(shiny)
##add county function
source("addCountiesToCSV.R")
source("scrapeRepo.R")

shinyServer(function(input, output) {
  
  getMin <- reactive({
    min <- input$range[1]
  })
  
  getMax <- reactive({
    max <- input$range[2]
  })
  
  getColor <- reactive({
    color <- input$color
  })
  
  ##this input is generated from the checkbox UI see below
  getStoreList <- reactive({
    stores <- input$stores
  })
  
  
  getStoreData <- function(store){
    storeData <- addCounties(store, addDemo = F, no_error_msg = T)
    return(storeData)  
  }
  
  addCountyInfoNow <- reactive({
    countyPop <- readRDS("counties.RDS")
    countyPop$stores <- 0
    list <- getStoreList()
    if(length(list) > 5) list <- list[1:5]
    for(i in list){
      error_msg <- tryCatch(data <- addCounties(i, addDemo = F), error = function(e) e)
      if(!inherits(error_msg, "error")){
        data <- addCounties(i, addDemo = F)
        m <- nrow(data)
        for(i in 1:m){
          x <- which(countyPop$name == as.character(data$County[i]))
          countyPop$stores[x] <- countyPop$stores[x] + 1
        }
      }
      else{
        warning("no zip codes")
        return(getStoreData(i))
      }
    }
    return(countyPop)
  })
  
  ##search the links list and allow the user to select which stores to plot
  output$matches <- renderUI({
    
    search <- input$userSearch
    
    search <- unlist(strsplit(search, ","))
    search <- gsub("^\\s*|\\s*$", "", search)
    results <- unlist(llply(search, function(i) grep(i, links, value = T)))
    if(length(results) > 0) checkboxGroupInput("stores", "Choose Stores", choices = results)
  })
  
  
  output$map <- renderPlot({
    countyPop <- addCountyInfoNow()
    countyCounts <- countyPop$stores
    if(length(which(countyCounts > 0)) == 0) stop("No info to display")
    at <- pmax(countyCounts, getMin()) 
    ct <- pmin(at, getMax())
    pct <- cut(ct, 100, include.lowest = T, ordered = T)
    fills <- colorRampPalette(c("white", getColor()))(100)[pct]
    outmap <- map("county", fill = TRUE, col = fills, resolution = 0, lty = 0, projection = "polyconic", myborder = 0, mar = c(0,0,0,0))
    outmap <- map("state", fill = FALSE, add = TRUE, col = "grey60", resolution = 0, lty = 1, lwd = 1, projection = "polyconic")
    
  })
  
  output$head <- renderText({
    names(addCountyInfoNow())
  }) 
  
  
  
  
  output$hist <- renderPlot({
    countyPop <- addCountyInfoNow()
    countyCounts <- countyPop$stores
    outhist <- hist(countyCounts[-which(countyCounts == 0)], xlim = c(getMin(), getMax()), breaks = 100,
                    main = paste0("Histogram of ", getStoreList()), xlab = "Stores", ylab = "Counties")
  })
  
  
})
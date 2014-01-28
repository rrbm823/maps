require(PBSmapping)
source("countysinglegrab.R")
shinyServer(function(input, output, session) {
  changeData <- reactive({
    lat <- as.numeric(input$lat)
    lng <- as.numeric(input$lng)
    if(is.numeric(lat) && is.numeric(lng)){
      getcounty(lat, lng)
    }
    else "Input coordinates"
  })
  
  output$message <- renderText({
    changeData()
  })
  
  firstTime <- TRUE
  
  output$hash <- reactiveText(function() {
    
    newHash = paste(collapse=",",
                    Map(function(field) {
                      paste(sep="=",
                            field,
                            input[[field]])
                    },
                        c("lat","lng")))
    
    return(
      if (!firstTime) {
        newHash
      } 
      else {
        if (is.null(input$hash)) {
          NULL
        } 
        else {
          firstTime <<- F;
          isolate(input$hash)
        }
      }
    )
  })
})
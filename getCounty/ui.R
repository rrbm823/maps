library(shiny)

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

shinyUI(pageWithSidebar(
  headerPanel("Get County Names"),
  sidebarPanel(
    textInput("lat", "latitude?", "DONT INPUT HERE"),
    textInput("lng", "longitude?", "INPUT GOES IN URL")
  ),
  mainPanel(
    includeHTML("URL.js"),
    h2(textOutput("message")),
    hashProxy("hash")
    )
))
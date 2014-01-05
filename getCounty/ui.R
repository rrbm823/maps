library(shiny)

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

shinyUI(pageWithSidebar(
  headerPanel("Get County Names"),
  sidebarPanel(
    textInput("lat", "latitude?", NULL),
    textInput("lng", "longitude?", NULL)
  ),
  mainPanel(
    includeHTML("URL.js"),
    h2(textOutput("message")),
    hashProxy("hash")
    )
))
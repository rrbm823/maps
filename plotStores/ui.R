library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("STORE MAPS"),
  
  sidebarPanel(helpText("enter one or more store names seperated by commas"), 
               textInput("stores", "STORE", value = ""),
               selectInput("color", "COLORS", colors(T)[-grep("light|pale|white", colors(T))]),
               sliderInput("range", label = "RANGE", min = 0, max = 500, value = c(0, 100)),
               radioButtons("log", "LOG", c("on", "off")),
               submitButton()),
  
  mainPanel(plotOutput("map"), plotOutput("hist"))
  
))
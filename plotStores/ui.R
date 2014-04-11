library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("STORE MAPS"),
  
  sidebarPanel(helpText("Enter one or more store names seperated by commas to search"),
               textInput("userSearch", "SEARCH"),
               selectInput("color", "COLORS", colors(T)[-grep("light|pale|white", colors(T))], "goldenrod"),
               sliderInput("range", label = "RANGE", min = 0, max = 500, value = c(0, 100)),
               uiOutput("matches"),
               submitButton()
               ),
  
  mainPanel(tabsetPanel(
            tabPanel("Map", plotOutput("map")),
            tabPanel("Histogram", plotOutput("hist")),
            tabPanel("Data", textOutput("head"))
            ))
  
))
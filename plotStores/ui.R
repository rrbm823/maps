library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("STORE MAPS"),
  
  sidebarPanel(helpText("enter a store name on the left"), 
               textInput("store", "STORE", value = "McDonalds"),
               submitButton()),
  
  mainPanel(plotOutput("map"))
  
))
require(XML)
require(RCurl)
#username <- input$username
#password <- input$password
username = "robert"
password = "r0bert!"
home <- "http://hathawayhome.net/scrapes/Store%20Locations/"
lines <- getURL(home, userpwd = paste0(username, ":", password))
doc <- htmlTreeParse(lines, asText = T, useInternalNodes = T)
doc <- xmlRoot(doc)
links <- grep(".csv", getHTMLLinks(doc), value = T)

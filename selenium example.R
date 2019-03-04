library(rvest)
library(RSelenium)

##go here
##http://selenium-release.storage.googleapis.com/3.9/selenium-server-standalone-3.9.1.jar

##an example to show you what's happening
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com/ncr")
remDr$close()
# stop the selenium server
rD[["server"]]$stop()
##end example


##fox example
rD <- rsDriver()
remDr <- rD[["client"]]

#page for NYTIMES
url <- 'https://www.nytimes.com/search?endDate=20181011&query=kavanaugh&sort=best&startDate=20180913'

#navigate to it
remDr$navigate(url)

#click the button 
webElem <- remDr$findElement(using = 'class name',"css-1t62hi8")

#with 752 results at 10 per page we need
#752 - 10 original = 742 / 10 per click = 75 clicks

for (i in 1:5) { #change this to 75 later
webElem$clickElement()
  Sys.sleep(runif(1, 1, 5))
}

webpage <-remDr$getPageSource()

#Reading the HTML code from the website - headlines
webpage2 <- read_html(unlist(webpage))

headline_data = html_nodes(webpage2,'a')

attr_data = html_attrs(headline_data) 
attr_data

urlslist = unlist(attr_data)
urlslist = urlslist[grep(".html", urlslist)]
urlslist = urlslist[grep("/2018", urlslist)]
urlslist = unique(urlslist)
urlslist

#paste NY on front
urlslist = paste("https://www.nytimes.com", urlslist, sep = "")

##start a data frame
NYtimesDF = matrix(NA, nrow = length(urlslist), ncol = 3)
colnames(NYtimesDF) = c("Source", "Url", "Text")
NYtimesDF = as.data.frame(NYtimesDF)

##for loops
for (i in 1:length(urlslist)){
  
  ##read in the URL
  webpage <- read_html(urlslist[i])
  
  ##pull the specific nodes
  headline_data = html_nodes(webpage,'.StoryBodyCompanionColumn') 
  
  ##pull the text
  text_data = html_text(headline_data)
  
  ##save the data
  NYtimesDF$Source[i] = "NY Times"
  NYtimesDF$Url[i] = urlslist[i]
  NYtimesDF$Text[i] = paste(text_data, collapse = "")
  Sys.sleep(runif(1, 1, 5))
} ##end for loop

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

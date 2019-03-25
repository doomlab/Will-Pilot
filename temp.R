library(rvest)
library(RSelenium)
library(beepr)
library(lubridate)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

url = "https://www.theblaze.com/search/?q=government+shutdown"

remDr$navigate(url)

webClick <- remDr$findElements(using = "css", ".action-btn")

#click next until done
while (length(webClick) > 0 ) { 
  
  #find the urls for page X
  webClick <- remDr$findElements(using = "css", ".action-btn")
  
  webClick[[1]]$clickElement()
  
  Sys.sleep(runif(1, 1, 5))
}

#find the urls for page X
webElems <- remDr$findElements(using = "css", "a")

#add the urls to a list
urlslist_final = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))

remDr$close()
# stop the selenium server
rD[["server"]]$stop()


##deal with the URLS
urlslist = urlslist_final
urlslist = unique(urlslist)
urlslist = urlslist[-c(grep("video/", urlslist))]
urlslist = urlslist[grep("/news/", urlslist)]
urlslist = urlslist[-c(grep("/201[3-7]", urlslist))]

##start a data frame
BLAZEDF = matrix(NA, nrow = length(urlslist), ncol = 3)
colnames(BLAZEDF) = c("Source", "Url", "Text")
BLAZEDF = as.data.frame(BLAZEDF)
int <- interval(ymd("2018-12-08"), ymd("2019-02-08"))


for (i in 1:length(urlslist)){
  
  webpage = read_html(urlslist[i])
  
  headline_data = html_nodes(webpage, '.post-date')
  text_data = html_text(headline_data)
  date = as.Date(text_data, "%B %d, %Y")
  
  if (date %within% int){
    headline_data2 = html_nodes(webpage,'h1, h3, p') 
    text_data2 = html_text(headline_data2)
    
    ##save the data
    BLAZEDF$Source[i] = "BLAZE"
    BLAZEDF$Url[i] = urlslist[i]
    BLAZEDF$Text[i] = paste(text_data2, collapse = "")
  }
  
  Sys.sleep(runif(1,1,10))
  
  }
  
  
beep(sound = 7)

write.csv(BLAZEDF, "BLAZE_gs.csv", row.names = F)
 
library(rvest)
library(RSelenium)
library(beepr)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

url = "https://slate.com/search?q=government+shutdown"

remDr$navigate(url)

#create a blank space to put the links
urlslist_final = list()

#loop over the results and go through them
#appear to be 10 pages
for (i in 1:10){
  
  #find the button to click on 
  webClick <- remDr$findElements(using = "css", ".gsc-cursor-page")
  
  #don't go too fast for the page to load
  Sys.sleep(runif(1, 1, 5))
  
  #click on the page number
  webClick[[i]]$clickElement()
  
  #page reload
  Sys.sleep(runif(1, 1, 5))
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "[href]")
  
  #add the urls to a list
  urlslist_final[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))
  
  #don't go too fast
  Sys.sleep(runif(1, 1, 5))
}

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

beep(sound = 3)

##deal with the URLS
urlslist = unlist(urlslist_final)

#find all the ones from 12 2018 to 02 2019
urlslist2 = urlslist[grep("/2018/12|/2019/01|/2019/02", urlslist)]
urlslist2 = unique(urlslist2)

#60 something articles seems suspiciously low, so also scraped this page
#https://slate.com/tag/governtment-shutdown

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

#create a blank space to put the links
urlslist_final = list()

#loop over the results and go through them
#354 / 20 = 18 pages
for (i in 1:18){
  
  url = paste0("https://slate.com/tag/government-shutdown/", i)
  
  remDr$navigate(url)
  
  #don't go too fast
  Sys.sleep(runif(1, 1, 5))
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "[href]")
  
  #add the urls to a list
  urlslist_final[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))
  
  #don't go too fast
  Sys.sleep(runif(1, 1, 5))
}

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

beep(sound = 3)

##deal with the URLS
urlslist = unlist(urlslist_final)

#find all the ones with /2018/
urlslist2.1 = urlslist[grep("/2018/12|/2019/01|/2019/02", urlslist)]
urlslist2.1 = unique(urlslist2.1)

urlslist3 = unique(c(urlslist2, urlslist2.1))

##start a data frame
SLATEDF = matrix(NA, nrow = length(urlslist3), ncol = 3)
colnames(SLATEDF) = c("Source", "Url", "Text")
SLATEDF = as.data.frame(SLATEDF)


##for loops
for (i in 1:length(urlslist3)){
  
  ##read in the URL
  webpage2 <- read_html(urlslist3[i])
  
  ##pull the specific nodes
  headline_data2 = html_nodes(webpage2,'.slate-paragraph') 
  
  ##pull the text
  text_data2 = html_text(headline_data2)
  
  ##save the data
  SLATEDF$Source[i] = "SLATE"
  SLATEDF$Url[i] = urlslist3[i]
  SLATEDF$Text[i] = paste(text_data2, collapse = "")
  Sys.sleep(runif(1, 1, 10))
} ##end for loop

#beep(sound = 3)

write.csv(SLATEDF, "SLATE_gs.csv", row.names = F)

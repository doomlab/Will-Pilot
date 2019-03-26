library(rvest)
library(RSelenium)
library(beepr)
library(lubridate)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

url = "http://www.hannity.com/trending/shutdown/"

remDr$navigate(url)

urlslist_final = list()

for (i in 1:4){
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "a")
  
  #add the urls to a list
  urlslist_final[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))
  
  #move on to the next page
  url = paste0("http://www.hannity.com/trending/shutdown/?pg=", i+1)
  
  remDr$navigate(url)
  
  Sys.sleep(runif(1,1,5))
  
}

remDr$close()
# stop the selenium server
rD[["server"]]$stop()


##deal with the URLS
urlslist = unlist(urlslist_final)
urlslist = unique(urlslist)

#also use google search to see if we can find more

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

url = "https://www.google.com/search?biw=1366&bih=632&tbs=cdr%3A1%2Ccd_min%3A12%2F8%2F2018%2Ccd_max%3A2%2F8%2F2019&ei=gECaXNe8Aaft_Qb9w7G4BA&q=government+shutdown+site%3Ahannity.com&oq=government+shutdown+site%3Ahannity.com&gs_l=psy-ab.3...0.0..1915...0.0..0.0.0.......0......gws-wiz.aFps0nA33mY"

remDr$navigate(url)

urlslist_final2 = list()

for (i in 1:9){
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "a")
  
  #add the urls to a list
  urlslist_final2[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))
  
  #find the thing to click on 
  webClick <- remDr$findElements(using = "link text", "Next")
  
  #click on the page number
  webClick[[1]]$clickElement()
  
  Sys.sleep(runif(1,1,5))
  
}

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

urlslist2 = unlist(urlslist_final2)
urlslist2 = c(urlslist, urlslist2)
urlslist2 = unique(urlslist2)
urlslist3 = urlslist2[grep("/media-room/[a-zA-Z]", urlslist2)]
urlslist3 = urlslist3[-c(grep("webcache", urlslist3))]
urlslist3 = urlslist3[-c(grep("radio-show", urlslist3))]


##start a data frame
HANNITYDF = matrix(NA, nrow = length(urlslist), ncol = 3)
colnames(HANNITYDF) = c("Source", "Url", "Text")
HANNITYDF = as.data.frame(HANNITYDF)
int <- interval(ymd("2018-12-08"), ymd("2019-02-08"))

for (i in 1:length(urlslist3)){
  
  webpage = read_html(urlslist3[i])
  
  headline_data = html_nodes(webpage, '.author-info')
  text_data = html_text(headline_data)
  text_data = strsplit(text_data, " ")[[1]][7]
  date = as.Date(text_data, "%m.%d.%y")
  
  if (date %within% int){
    headline_data2 = html_nodes(webpage,'h1, p') 
    text_data2 = html_text(headline_data2)
    
    ##save the data
    HANNITYDF$Source[i] = "HANNITY"
    HANNITYDF$Url[i] = urlslist3[i]
    HANNITYDF$Text[i] = paste(text_data2, collapse = "")
  }
  
  Sys.sleep(runif(1,1,10))
  
  }
  
  
beep(sound = 7)

write.csv(HANNITYDF, "HANNITY_gs.csv", row.names = F)
 
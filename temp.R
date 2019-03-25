library(rvest)
library(RSelenium)
library(beepr)
library(lubridate)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

#create a blank space to put the links
urlslist_final = list()

url = "https://www.rushlimbaugh.com/?s=government%20shutdown&is_v=1"

remDr$navigate(url)

#loop over the results and go through them
##18 pages
for (i in 1:47){
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "a")
  
  #add the urls to a list
  urlslist_final[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))

  #find the thing to click on 
  webClick <- remDr$findElements(using = "css", ".ais-pagination--link")
  
  #click on the page number
  webClick[[18]]$clickElement()
  
  #page reload
  Sys.sleep(runif(1, 1, 5))
  
}

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

beep(sound = 6)

##deal with the URLS
urlslist = unlist(urlslist_final)
urlslist = unique(urlslist)
urlslist = urlslist[-c(grep("store", urlslist))] #take out store
urlslist = urlslist[grep("/daily/[0-9]*/[0-9]*/[0-9]*/[a-zA-Z]", urlslist)]

##find the right dates
temp = strsplit(urlslist, split = "/")
temp2 = plyr::ldply(temp, rbind)
temp2 = as.data.frame(apply(temp2, 2, as.character), stringsAsFactors = F)

#pull only the right dates
rows = rownames(temp2[ (temp2$`5` == 2018 | temp2$`5` == 2019) & #must be 2018/19 and  
                         (
                           (temp2$`6` == "12" & as.numeric(temp2$`7`)>8 & temp2$`5` == 2018) | #after dec 12
                             (temp2$`6`== "01" & temp2$`5` == 2019) | #all of jan
                             (temp2$`6` == "02" & as.numeric(temp2$`7`)<8 & temp2$`5` == 2019)
                         ), #before feb 8
                       ])
urlslist3 = urlslist[as.numeric(rows)]

##start a data frame
RUSHDF = matrix(NA, nrow = length(urlslist3), ncol = 3)
colnames(RUSHDF) = c("Source", "Url", "Text")
RUSHDF = as.data.frame(RUSHDF)


for (i in 1:length(urlslist3)){
  
  webpage = read_html(urlslist3[i])
  
  headline_data2 = html_nodes(webpage,'p') 
  text_data2 = html_text(headline_data2)
  
  ##save the data
  RUSHDF$Source[i] = "RUSH"
  RUSHDF$Url[i] = urlslist3[i]
  RUSHDF$Text[i] = paste(text_data2, collapse = "")
  
  
  Sys.sleep(runif(1,1,10))
  
  }
  
  
beep(sound = 7)

write.csv(RUSHDF, "RUSH_gs.csv", row.names = F)
 
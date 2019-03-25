library(rvest)
library(RSelenium)
library(beepr)
library(lubridate)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

url = "https://www.theblaze.com/search/?q=kavanaugh"

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
urlslist = urlslist[grep("/news/[0-9]*/[0-9]*/[0-9]*/[a-zA-Z]", urlslist)]

##find the right dates
temp = strsplit(urlslist, split = "/")
temp2 = plyr::ldply(temp, rbind)
temp2 = as.data.frame(apply(temp2, 2, as.character), stringsAsFactors = F)

#pull only the right dates
rows = rownames(temp2[ temp2$`5` == 2018 & #must be 2018 and 
                         (temp2$`6` == "09" & as.numeric(temp2$`7`)>=13 | 
                            temp2$`6` == "10" & as.numeric(temp2$`7`)<=11), #Must be 9 or 10 
                       ])

urlslist3 = urlslist[as.numeric(rows)]

##start a data frame
BLAZEDF = matrix(NA, nrow = length(urlslist3), ncol = 3)
colnames(BLAZEDF) = c("Source", "Url", "Text")
BLAZEDF = as.data.frame(BLAZEDF)


for (i in 1:length(urlslist3)){
  
  webpage = read_html(urlslist3[i])
  
  headline_data2 = html_nodes(webpage,'h1, h3, p') 
  text_data2 = html_text(headline_data2)
  
  ##save the data
  BLAZEDF$Source[i] = "BLAZE"
  BLAZEDF$Url[i] = urlslist3[i]
  BLAZEDF$Text[i] = paste(text_data2, collapse = "")
  
  
  Sys.sleep(runif(1,1,10))
  
  }
  
  
beep(sound = 7)

write.csv(BLAZEDF, "BLAZE_kav.csv", row.names = F)
 
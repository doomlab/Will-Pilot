library(rvest)
library(RSelenium)
library(beepr)
library(lubridate)

#open the browser
rD <- rsDriver(browser=c("chrome"), chromever="73.0.3683.68")
remDr <- rD[["client"]]

#create a blank space to put the links
urlslist_final = list()

url = "https://www.breitbart.com/tag/government-shutdown/"

remDr$navigate(url)

#loop over the results and go through them
for (i in 1:16){
  
  #find the urls for page X
  webElems <- remDr$findElements(using = "css", "h2 a")
  
  #add the urls to a list
  urlslist_final[[i]] = unlist(sapply(webElems, function(x) {x$getElementAttribute("href")}))

  if (i > 1)
    {
  url = paste0("https://www.breitbart.com/tag/government-shutdown/page/",
               i+1, "/")
  remDr$navigate(url)
  }
  
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
BREITBARTDF = matrix(NA, nrow = length(urlslist3), ncol = 3)
colnames(BREITBARTDF) = c("Source", "Url", "Text")
BREITBARTDF = as.data.frame(BREITBARTDF)


for (i in 1:length(urlslist3)){
  
  webpage = read_html(urlslist3[i])
  
  headline_data2 = html_nodes(webpage,'h2, p') 
  text_data2 = html_text(headline_data2)
  
  ##save the data
  BREITBARTDF$Source[i] = "BREITBART"
  BREITBARTDF$Url[i] = urlslist3[i]
  BREITBARTDF$Text[i] = paste(text_data2, collapse = "")
  
  
  Sys.sleep(runif(1,1,10))
  
  }
  
  
beep(sound = 7)

write.csv(BREITBARTDF, "BREITBART_gs.csv", row.names = F)

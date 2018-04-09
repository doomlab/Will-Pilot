library(rvest)
####NY Times####
#Specifying the url for desired website to be scrapped
url <- 'https://www.nytimes.com/section/politics'

#Reading the HTML code from the website - headlines
webpage <- read_html(url)
headline_data = html_nodes(webpage,'.story-link a, .story-body a')
#headline_data = html_text(headline_data) #convert to readable text
#head(headline_data) #hmmm, I think it could work

#okay, so I can get the urls this way, but idk how to get rid of the other crap
attr_data = html_attrs(headline_data) 
attr_data

urlslist = unlist(attr_data)
urlslist = urlslist[grep("http", urlslist)]
urlslist = unique(urlslist)
urlslist

##start a data frame
NYtimesDF = matrix(NA, nrow = length(urlslist), ncol = 3)
colnames(NYtimesDF) = c("Source", "Url", "Text")
NYtimesDF = as.data.frame(NYtimesDF)

##for loops
for (i in 1:length(urlslist)){
  
  ##read in the URL
  webpage <- read_html(urlslist[i])
  
  ##pull the specific nodes
  headline_data = html_nodes(webpage,'.story-content') 
  
  ##pull the text
  text_data = html_text(headline_data)
  
  ##save the data
  NYtimesDF$Source[i] = "NY Times"
  NYtimesDF$Url[i] = urlslist[i]
  NYtimesDF$Text[i] = paste(text_data, collapse = "")
    } ##end for loop

####NPR original front page####
url2 = 'https://www.npr.org/sections/politics/'
webpage2 = read_html(url2)
headline_data2 = html_nodes(webpage2,'.title a')
#headline_data = html_text(headline_data)
#head(headline_data) #Very nice. NPR Politics does not show a huge list on its front page

#Los Localizadores Uniformes de Recursos (URLs)
attr_data2 = html_attrs(headline_data2) 
attr_data2

urlslist2 = unlist(attr_data2)
urlslist2 = urlslist2[grep("http", urlslist2)]
urlslist2

##start a data frame
NPRDF = matrix(NA, nrow = length(urlslist2), ncol = 3)
colnames(NPRDF) = c("Source", "Url", "Text")
NPRDF = as.data.frame(NPRDF)

##for loops
for (i in 1:length(urlslist2)){
  
  ##read in the URL
  webpage2 <- read_html(urlslist2[i])
  
  ##pull the specific nodes
  headline_data2 = html_nodes(webpage2,'#storytext > p') 
  
  ##pull the text
  text_data2 = html_text(headline_data2)
  
  ##save the data
  NPRDF$Source[i] = "NPR"
  NPRDF$Url[i] = urlslist2[i]
  NPRDF$Text[i] = paste(text_data2, collapse = "")
} ##end for loop

####Fox News####
url3 = 'http://www.foxnews.com/politics.html'
url3.1 = 'http://www.foxnews.com/category/politics/executive.html'
url3.2 = 'http://www.foxnews.com/category/politics/senate.htm.html'
url3.3 = 'http://www.foxnews.com/category/politics/house-representatives.html'
url3.4 = 'http://www.foxnews.com/category/politics/judiciary.html'
url3.5 = 'http://www.foxnews.com/category/politics/foreign-policy.html'
url3.6 = 'http://www.foxnews.com/category/politics/elections.html'
webpage3 = read_html(url3)
webpage3.1 = read_html(url3.1)
webpage3.2 = read_html(url3.2)
webpage3.3 = read_html(url3.3)
webpage3.4 = read_html(url3.4)
webpage3.5 = read_html(url3.5)
webpage3.6 = read_html(url3.6)

headline_data3 = html_nodes(webpage3, '.story- a , .article-list .title a')
headline_data3.1 = html_nodes(webpage3.1, '.story- a , .article-list .title a')
headline_data3.2 = html_nodes(webpage3.2, '.story- a , .article-list .title a')
headline_data3.3 = html_nodes(webpage3.3, '.story- a , .article-list .title a')
headline_data3.4 = html_nodes(webpage3.4, '.story- a , .article-list .title a')
headline_data3.5 = html_nodes(webpage3.5, '.story- a , .article-list .title a')
headline_data3.6 = html_nodes(webpage3.6, '.story- a , .article-list .title a')

#headline_data = html_text(headline_data)
head(headline_data3) #doesn't look bad, the output that is...

attr_data3 = html_attrs(headline_data3) 
attr_data3.1 = html_attrs(headline_data3.1) 
attr_data3.2 = html_attrs(headline_data3.2) 
attr_data3.3 = html_attrs(headline_data3.3) 
attr_data3.4 = html_attrs(headline_data3.4) 
attr_data3.5 = html_attrs(headline_data3.5) 
attr_data3.6 = html_attrs(headline_data3.6) 

attr_data3

urlslist3 = c(unlist(attr_data3), unlist(attr_data3.1), 
              unlist(attr_data3.2), unlist(attr_data3.3), 
              unlist(attr_data3.4), unlist(attr_data3.5, attr_data3.6))
##find all that are href
urlslist3 = urlslist3[grep("http|.html", urlslist3)]
##fix the ones without the leading foxnews.com
urlslist3F = paste("http://www.foxnews.com", urlslist3[grep("^http", urlslist3, invert = T)], sep = "")
urlslist3N = urlslist3[grep("^http", urlslist3)]
urlslist3 = c(urlslist3N, urlslist3F)
urlslist3 = unique(urlslist3)
urlslist3

##start a data frame
FoxDF = matrix(NA, nrow = length(urlslist3), ncol = 3)
colnames(FoxDF) = c("Source", "Url", "Text")
FoxDF = as.data.frame(FoxDF)

##for loops
for (i in 1:length(urlslist3)){
  
  ##read in the URL
  webpage3 <- read_html(urlslist3[i])
  
  ##pull the specific nodes
  headline_data3 = html_nodes(webpage3,'p+ p , .fn-video+ p , .twitter+ p , .speakable') 
  
  ##pull the text
  text_data3 = html_text(headline_data3)
  
  ##save the data
  FoxDF$Source[i] = "Fox News"
  FoxDF$Url[i] = urlslist3[i]
  FoxDF$Text[i] = paste(text_data3, collapse = "")
} ##end for loop

####Breitbart####
url4 = 'http://www.breitbart.com/big-government/'
webpage4 = read_html(url4)
headline_data4 = html_nodes(webpage4, '.title a , #grid-block-0 span , #BBTrendUL a , #disqus-popularUL a , font')
#headline_data4 = html_text(headline_data4)
head(headline_data4) 

attr_data4 = html_attrs(headline_data4) 
attr_data4

urlslist4 = unlist(attr_data4)
##find all that are href
urlslist4 = urlslist4[grep("http|/big-government", urlslist4)]
##fix the ones without the leading bb
urlslist4F = paste("http://www.breitbart.com", urlslist4[grep("^http", urlslist4, invert = T)], sep = "")
urlslist4N = urlslist4[grep("^http", urlslist4)]
urlslist4 = c(urlslist4N, urlslist4F)
urlslist4 = unique(urlslist4)
urlslist4

##start a data frame
BreitbartDF = matrix(NA, nrow = length(urlslist4), ncol = 3)
colnames(BreitbartDF) = c("Source", "Url", "Text")
BreitbartDF = as.data.frame(BreitbartDF)

##for loops
for (i in 1:length(urlslist4)){
  
  ##read in the URL
  webpage4 <- read_html(urlslist4[i])
  
  ##pull the specific nodes
  headline_data4 = html_nodes(webpage4,'.entry-content p , h2') 
  
  ##pull the text
  text_data4 = html_text(headline_data4)
  
  ##save the data
  BreitbartDF$Source[i] = "Breitbart"
  BreitbartDF$Url[i] = urlslist4[i]
  BreitbartDF$Text[i] = paste(text_data4, collapse = "")
} ##end for loop

####NPR Archive####
url5 = 'https://www.npr.org/sections/politics/archive'
webpage5 = read_html(url5)
headline_data5 = html_nodes(webpage5,'.title a')
#headline_data = html_text(headline_data)
#head(headline_data)

#Los Localizadores Uniformes de Recursos (URLs)
attr_data5 = html_attrs(headline_data5) 
attr_data5

urlslist5 = unlist(attr_data5)
urlslist5 = urlslist5[grep("http", urlslist5)]
urlslist5

##start a data frame
NPRArchiveDF = matrix(NA, nrow = length(urlslist5), ncol = 3)
colnames(NPRArchiveDF) = c("Source", "Url", "Text")
NPRArchiveDF = as.data.frame(NPRArchiveDF)

##for loops
for (i in 1:length(urlslist5)){
  
  ##read in the URL
  webpage5 <- read_html(urlslist5[i])
  
  ##pull the specific nodes
  headline_data5 = html_nodes(webpage5,'#storytext > p') 
  
  ##pull the text
  text_data5 = html_text(headline_data5)
  
  ##save the data
  NPRArchiveDF$Source[i] = "NPR"
  NPRArchiveDF$Url[i] = urlslist5[i]
  NPRArchiveDF$Text[i] = paste(text_data5, collapse = "")
} ##end for loop

####put together####
##set your working directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Will-Pilot")

##import the overalldata 
overalldata = read.csv("overalldata.csv")

##combine with new data (add the other DF names in here...)
newdata = rbind(overalldata, NYtimesDF, NPRDF, FoxDF, BreitbartDF, NPRArchiveDF)

#temp NPR updates
#newdata = newdata[ , -4]
#newdata = rbind(newdata, NPRArchiveDF)

#change politics archive to NPR, so we can eliminate dupes
#newdata$Source[newdata$Source == "NPR Politics Archive"] = "NPR"
#newdata$Source = droplevels(newdata$Source)

##make the newdata unique in case of overlap across days
newdata = unique(newdata)

##write it back out
write.csv(newdata, "overalldata.csv", row.names = F)

##number of articles
table(newdata$Source)

##number of words
library(ngram)
newdata$Text = as.character(newdata$Text)
for (i in 1:nrow(newdata)) {
  
  #newdata$writing[i] = preprocess(newdata$Text[i], case="lower", remove.punct=TRUE)
  newdata$wordcount[i] = string.summary(newdata$Text[i])$words
  
}

tapply(newdata$wordcount, newdata$Source, mean)
tapply(newdata$wordcount, newdata$Source, sum)


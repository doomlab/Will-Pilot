
# Web Scraping Code -------------------------------------------------------

#we ran this part daily until the bottom numbers were past 250K words for each one

library(rvest)
####NY Times####
#Specifying the url for desired website to be scrapped
url <- 'https://www.nytimes.com/section/politics'

#Reading the HTML code from the website - headlines
webpage <- read_html(url)
headline_data = html_nodes(webpage,'.story-link a, .story-body a')

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

#URLs
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
head(headline_data3) 

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

#URLs
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

##set your working directory
#setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Will-Pilot")

##import the overalldata 
overalldata = read.csv("overalldata.csv")

##combine with new data (add the other DF names in here...)
newdata = rbind(overalldata, NYtimesDF, NPRDF, FoxDF, BreitbartDF, NPRArchiveDF)

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
  newdata$WordCount[i] = string.summary(newdata$Text[i])$words
}

tapply(newdata$WordCount, newdata$Source, mean)
tapply(newdata$WordCount, newdata$Source, sum)

# Word Counting Data ---------------------------------------------------------

## pull in the data
master = read.csv("overalldata.csv", stringsAsFactors = F)

##get rid of the blank stuff
library(ngram)

for (i in 1:nrow(master)) {
  master$WordCount[i] = wordcount(master$Text[i])
}

nozero = subset(master, WordCount > 0)
tapply(nozero$WordCount, nozero$Source, mean)
tapply(nozero$WordCount, nozero$Source, sum)

##take out the disqus ones
nodis = nozero[ -c(grep("disqus", nozero$Url)), ]

##figure out the duplicates
nodis$URLS = duplicated(nodis$Url)

##all data
final = subset(nodis, URLS == FALSE)

# Pull in Original MFD -----------------------------------------------

##the amount of time people used the original MFD words
original_mfd = read.csv("original_mfd.csv", stringsAsFactors = F)

##adding forms/conjugations of words to original_mfd
#harm/care
original_mfd[c(27:95),1] = c("abusive","abuser","abused","abusing",
                             "sympathetic","sympathies","damaged",
                             "damaging","attacked","attacking","attacker",
                             "attacks","attackers", "benefits", "benefitted",
                             "benefitting", "cares", "cared", "caring", "crushes",
                             "crushing", "crushed", "dangerous", "defends", "defending",
                             "defended", "detroys", "destroyed", "destroying",
                             "fights", "fought", "fighting", "guards", "guarded",
                             "guarding", "harms", "harmed", "harming", "hurts",
                             "hurting", "kills", "killed", "killing", "preserves",
                             "preserved", "preserving", "protects", "protected",
                             "protecting", "protection", "ruins", "ruined", "ruining",
                             "safely", "safer", "shelters", "sheltered", "sheltering",
                             "spurns", "spurned", "spurning", "stomps", "stomped",
                             "stomping", "suffers", "suffered", "suffering",
                             "violence", "warring")

#fairness/reciprocity
original_mfd[c(20:64),2] = c("balances", "balanced", "balancing","biased",
                             "biases", "discriminates", "discriminated",
                             "discriminating", "discrimination", "equals",
                             "equaled", "equaling", "equates", "equated",
                             "equating", "evens", "evened", "evening",
                             "excludes", "excluded", "excluding", "fairness",
                             "favors", "favored", "favoring", "honesty",
                             "impariality", "justice", "justifies", "justified",
                             "justifying", "prefers", "preferred", "preferring",
                             "prejudices", "prejudiced", "prejudicing", "reasons",
                             "reasoned", "reasoning", "rights", "tolerates",
                             "tolerated", "tolerating", "toleration")

#ingroup/loyalty
original_mfd[c(16:47),3] = c("collects", "collected", "collecting", "collective",
                             "communities", "deceives", "deceived", "deceiving",
                             "deception", "deceptions", "deserts", "deserted",
                             "deserting", "desertion", "families", "fellows",
                             "foreigners", "groups", "grouped", "grouping",
                             "indviduals", "indvidualize", "individualized",
                             "individualizing", "members", "nations", "sides",
                             "togetherness", "traits", "unites", "united", "uniting")

#authority/respect
original_mfd[c(31:112),4] = c("abides", "abided", "abiding", "authorities",
                              "classes", "classed", "classing", "command",
                              "commanded", "commanding", "controls", "controlled",
                              "controlling", "defects", "defected", "defecting",
                              "defers", "deferred", "deferring", "deference",
                              "defies", "defied", "defying", "defiance", "deserts",
                              "deserted", "deserting", "desertion", "duties",
                              "faiths", "fathers", "fathered", "fathering",
                              "honors", "honored", "honoring", "laws", "leads",
                              "leading", "mothers", "mothered", "mothering",
                              "obeys", "obeyed", "obeying", "opposes", "opposed",
                              "opposing", "orders", "ordered", "ordering", "permits",
                              "permitted", "permitting", "positions", "positioned",
                              "positioning", "preserves", "preserved", "preserving",
                              "preservation", "protests", "protested", "protesting",
                              "refuses", "refused", "refusing", "refusal", "respects",
                              "respected", "respecting", "respectful", "reveres",
                              "revered", "revering", "reverence", "serves", "served",
                              "serving", "traditions", "traditional", "traits")

#purity/sanctity
original_mfd[c(21:60),5] = c("abstains", "abstained", "abstaining", "abstinence",
                             "adulteries", "adulterous", "adulterer", "adulterers",
                             "churches", "cleans", "cleaned", "cleaning", "cleanse",
                             "cleanliness", "dirty", "diseases", "diseased", 
                             "disgusts", "disgusted", "disgusting", "grossness",
                             "innocence", "modesty", "preserves", "preserved",
                             "preserving", "preservation", "promiscuity",
                             "promiscuities", "purity", "rights", "ruins", "ruined",
                             "ruining", "sacredness", "sickness", "sicknesses",
                             "sins", "wholeness", "wholesome")

# Stemming the Data -------------------------------------------------------

##load new libraries
library(tm)

final = na.omit(final)

#process the Text column (write a loop) - save processed text as a separate column
for(i in 1:nrow(final)) {
  
  final$edited[i] = preprocess(final$Text[i], #one value at a time
                               case = "lower", 
                               remove.punct = TRUE,
                               remove.numbers = FALSE, 
                               fix.spacing = TRUE)
  
  ##stem the words - do this second in the loop
  final$Processed[i] = stemDocument(final$edited[i], language = "english")
} 

#process the mfd
for(i in 1:nrow(original_mfd))
{
  original_mfd$harm[i] = stemDocument(original_mfd$harm[i], language = "english")
  
  original_mfd$fair[i] = stemDocument(original_mfd$fair[i], language = "english")
  
  original_mfd$ingroup[i] = stemDocument(original_mfd$ingroup[i], language = "english")
  
  original_mfd$authority[i] = stemDocument(original_mfd$authority[i], language = "english")
  
  original_mfd$purity[i] = stemDocument(original_mfd$purity[i], language = "english")
}


# Counting MFD ------------------------------------------------------------

for (i in 1:nrow(final)) {

  ##first make a table of a response, unlisting everything
  temp = as.data.frame(table(unlist(strsplit(final$Processed[i], " "))))
  
  ##find the rows that match the mfd list and sum and save
  final$HarmSum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$harm[original_mfd$harm != "" & original_mfd$harm != "NA"])])
  final$FairSum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$fair[original_mfd$fair != "" & original_mfd$fair != "NA"])])
  final$IngroupSum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$ingroup[original_mfd$ingroup != "" & original_mfd$ingroup != "NA"])])
  final$AuthoritySum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$authority[original_mfd$authority != "" & original_mfd$authority != "NA"])])
  final$PuritySum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$purity[original_mfd$purity != "" & original_mfd$purity != "NA"])])
 
}

#create percentages 
final$HarmPercent = final$HarmSum / final$WordCount * 100
final$FairPercent = final$FairSum / final$WordCount * 100
final$IngroupPercent = final$IngroupSum / final$WordCount * 100
final$AuthorityPercent = final$AuthoritySum / final$WordCount * 100
final$PurityPercent = final$PuritySum / final$WordCount * 100

#create final dataset
final_data = final[ , -c(5,6)]
write.csv(final_data, "sage_data.csv", row.names = F)

# Create a Picture --------------------------------------------------------

#reshape the data for ggplot2
library(reshape)
long_data = melt(final_data[ , c("Source", "HarmPercent", "FairPercent", 
                                "IngroupPercent", "AuthorityPercent", "PurityPercent")], 
                 id = c("Source"))

library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

#create a column that allows you panel the data
long_data$lean = long_data$Source
long_data$lean = gsub("Breitbart", "Conservative", long_data$lean)
long_data$lean = gsub("Fox News", "Conservative", long_data$lean)
long_data$lean = gsub("NPR", "Liberal", long_data$lean)
long_data$lean = gsub("NY Times", "Liberal", long_data$lean)

#sensible column names
colnames(long_data)[2:3] = c("MoralFoundation", "Percent")

#change the labels for the moral foundations
long_data$MoralFoundation = factor(long_data$MoralFoundation,
                                   levels = levels(long_data$MoralFoundation),
                                   labels = c("Harm", "Fair", "Ingroup",
                                              "Authority", "Purity"))

overallgraph = ggplot(long_data, aes(lean, Percent, fill = Source))
overallgraph +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = .2) +
  xlab("Moral Category") +
  ylab("Percent") + 
  cleanup +
  facet_grid(~MoralFoundation) + 
  scale_fill_manual(values = c("grey20", "grey40", "grey60", "grey80")) +
  NULL

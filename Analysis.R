##set working directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Will-Pilot")

##pull in the data
master = read.csv("overalldata.csv", stringsAsFactors = F)

##get rid of the blank stuff
library(ngram)

for (i in 1:nrow(master)) {
  master$wordcount[i] = wordcount(master$Text[i])
}

nozero = subset(master, wordcount > 0)
tapply(nozero$wordcount, nozero$Source, mean)
tapply(nozero$wordcount, nozero$Source, sum)

##take out the disqus ones
nodis = nozero[ -c(grep("disqus", nozero$Url)), ]

##figure out the duplicates
nodis$URLS = duplicated(nodis$Url)

##all data
final = subset(nodis, URLS == FALSE)

tapply(final$wordcount, final$Source, mean)
tapply(final$wordcount, final$Source, sum)

write.csv(final, "finaldata.csv")

##figure out the percent of MFD words

##the amount of time people used the original MFD words
original_mfd = read.csv("original_mfd.csv", stringsAsFactors = F)

#process the Text column (write a loop) - save processed text as a separate column
preprocess(x, #one value at a time
           case = "lower", 
           remove.punct = TRUE,
           remove.numbers = FALSE, 
           fix.spacing = TRUE)

##stem the words - do this second in the loop
stemDocument(final$Text[i], language = "english")

##stem the original MFD stuff - separate loop on only the mfd data frame
length( #counting the number of matches
  grep("the", #put in the MFD stemmed word
       unlist(strsplit(final$Text[i], " ") #separates out the text one at time
              )))






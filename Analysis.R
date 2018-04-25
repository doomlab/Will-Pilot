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

#write.csv(final, "finaldata.csv")

####start here####
final = read.csv("finaldata.csv", stringsAsFactors = F)
final = na.omit(final) #one weird NA line

##figure out the percent of MFD words

##the amount of time people used the original MFD words
original_mfd = read.csv("original_mfd.csv", stringsAsFactors = F)

library(tm)
library(ngram)

#process the Text column (write a loop) - save processed text as a separate column
for(i in 1:nrow(final)) {
  
  final$edited[i] = preprocess(final$Text[i], #one value at a time
           case = "lower", 
           remove.punct = TRUE,
           remove.numbers = FALSE, 
           fix.spacing = TRUE)

##stem the words - do this second in the loop
  final$stemmed[i] = stemDocument(final$edited[i], language = "english")
} #Whoa!!! - why do the 'edited' and 'stemmed' column all have the exact same text?

##stem the original MFD stuff - separate loop on only the mfd data frame
for(i in 1:nrow(original_mfd)) {
  original_mfd$h2[i] = stemDocument(original_mfd$h2[i], language = "english")
  original_mfd$f2[i] = stemDocument(original_mfd$f2[i], language = "english")
  original_mfd$i2[i] = stemDocument(original_mfd$i2[i], language = "english")
  original_mfd$a2[i] = stemDocument(original_mfd$a2[i], language = "english")
  original_mfd$p2[i] = stemDocument(original_mfd$p2[i], language = "english")
}

final$hsum = NA
final$fsum = NA
final$isum = NA
final$asum = NA
final$psum = NA

for (i in 1:nrow(final)) {
##first make a table of a response, unlisting everything
temp = as.data.frame(table(unlist(strsplit(final$stemmed[i], " "))))
##find the rows that match the mfd list and sum and save
final$hsum[i] = sum(temp$Freq[temp$Var1 %in% original_mfd$h2[original_mfd$h2 != ""]])
final$fsum[i] = sum(temp$Freq[temp$Var1 %in% original_mfd$f2[original_mfd$f2 != ""]])
final$isum[i] = sum(temp$Freq[temp$Var1 %in% original_mfd$i2[original_mfd$i2 != ""]])
final$asum[i] = sum(temp$Freq[temp$Var1 %in% original_mfd$a2[original_mfd$a2 != ""]])
final$psum[i] = sum(temp$Freq[temp$Var1 %in% original_mfd$p2[original_mfd$p2 != ""]])
}

final$hper = final$hsum / final$wordcount * 100
final$fper = final$fsum / final$wordcount * 100
final$iper = final$isum / final$wordcount * 100
final$aper = final$asum / final$wordcount * 100
final$pper = final$psum / final$wordcount * 100

#let's look at the means
tapply(final$hper, final$Source, mean)
tapply(final$fper, final$Source, mean)
tapply(final$iper, final$Source, mean)
tapply(final$aper, final$Source, mean)
tapply(final$pper, final$Source, mean)


##Playing with stemming
#Check it out! Only 'adultery' stems to 'adulteri'
stemDocument("adultery", language = "english")
stemDocument("adulterous", language = "english")
stemDocument("adulterer", language = "english")
stemDocument("adulterous", language = "english")

#sympathy - more weirdness
stemDocument("sympathy", language = "english")
stemDocument("sympathetic", language = "english")
stemDocument("sympathies", language = "english")

#abuse - seems to be consistent across several forms/conjugations
stemDocument("abuse", language = "english")
stemDocument("abusive", language = "english")
stemDocument("abuser", language = "english")
stemDocument("abused", language = "english")
stemDocument("abusing", language = "english")

#damage - consistent!!
stemDocument("damages", language = "english")
stemDocument("damaged", language = "english")
stemDocument("damaging", language = "english")



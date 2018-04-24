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

library(tm)
library(ngram)

#process the Text column (write a loop) - save processed text as a separate column
for(i in 1:length(final)) {
  
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

#make a harm data frame
saveh = matrix(NA, nrow = nrow(final), ncol = nrow(original_mfd))

for(i in 1:nrow(original_mfd)) { #counting the number of matches
  
  for (r in 1:nrow(final)){
    
  if ( length(
      grep(original_mfd$h2[i], #put in the MFD stemmed word
         unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
         ) ##close unlist
         ) ##close grep
  ) > 0 ) {
    saveh[r,i] = length(grep(original_mfd$h2[i], #put in the MFD stemmed word
                      unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
                      ) ##close unlist
    ) ##close grep
    ) ##close length
  } else {saveh[r,i] = 0 }
    
  
  # grep(original_mfd$f2[i], #put in the MFD stemmed word
  #      unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
  #      ))
  # 
  # grep(original_mfd$i2[i], #put in the MFD stemmed word
  #      unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
  #      ))
  # 
  # grep(original_mfd$a2[i], #put in the MFD stemmed word
  #      unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
  #      ))
  # 
  # grep(original_mfd$p2[i], #put in the MFD stemmed word
  #      unlist(strsplit(final$stemmed[r], " ") #separates out the text one at time
  #      ))
  }
}


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



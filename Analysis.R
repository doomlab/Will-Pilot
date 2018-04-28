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
##load libraries
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

##Attempt at MLM
mlm_data = final[-c(2:8)]
library(reshape)
long_mlm = melt(mlm_data,
                id = "X",
                measured = c("hsum","fsum", 
                             "isum", "asum",
                             "psum","hper",
                             "fper","iper",
                             "aper","pper"))

##set up the analysis
library(nlme)
#####intercept only model####
##gls = generalized least squares
##ML = maximum likelihood
model1 = gls(value ~ 1, #DV ~ IV (which is only the intercept; is y-average diff than 0?)
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

####random intercept only model####
##note we switched to LME function
model2 = lme(value ~ 1, 
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|X) #sets random intercept for each participant
summary(model2) #note changed Value in output; also, 
#Random effects tells how much intercept varies among Pps
anova(model1, model2) #tests if necessary to nest; Yes, we need to do MLM

####second level####
model2.1 = lme(value ~ 1, 
               data = long_mlm, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|X, ~1|variable))
summary(model2.1)
anova(model1, model2, model2.1) 

####predictor model####
model3 = lme(value ~ variable, #now we want to switch the 1 for IV of interest
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|X)
summary(model3)
anova(model1, model2, model3)

####random slopes####
model4 = lme(value ~ variable,
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~ variable|X, #random intercept AND random slope
             control = lmeControl(msMaxIter = 200))
summary(model4)
anova(model1, model2, model3, model4)



##Playing with stemming
#Check it out! Only 'adultery' stems to 'adulteri'
# stemDocument("adultery", language = "english")
# stemDocument("adulterous", language = "english")
# stemDocument("adulterer", language = "english")
# stemDocument("adulterous", language = "english")
# 
# #sympathy - more weirdness
# stemDocument("sympathy", language = "english")
# stemDocument("sympathetic", language = "english")
# stemDocument("sympathies", language = "english")
# 
# #abuse - seems to be consistent across several forms/conjugations
# stemDocument("abuse", language = "english")
# stemDocument("abusive", language = "english")
# stemDocument("abuser", language = "english")
# stemDocument("abused", language = "english")
# stemDocument("abusing", language = "english")
# 
# #damage - consistent!!
# stemDocument("damages", language = "english")
# stemDocument("damaged", language = "english")
# stemDocument("damaging", language = "english")



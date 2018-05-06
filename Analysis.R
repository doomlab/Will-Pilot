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

##separate them into h low, h high until you have ten categories 
#harm/care
original_mfd$h2hi[35:112] = NA 
original_mfd$h2hi[1:34] = c("benefit", "care", "defend", "guard", "preserve",
                      "protect", "safe", "shelter", "sympathetic",
                      "sympathies", "benefits", "benefitted",
                      "benefitting", "cares", "cared", "caring","defends", 
                      "defending","defended", "guards", "guarded",
                      "guarding","preserves",
                      "preserved", "preserving", "protects", "protected",
                      "protecting", "protection",  
                      "safely", "safer", "shelters", 
                      "sheltered", "sheltering")
original_mfd$h2lo[61:112] = NA
original_mfd$h2lo[1:60] = c("abuse", "attack", "cruel", "crush",
                      "damage", "danger", "destroy", "fight", "harm",
                      "hurt", "kill", "ruin", "spurn", "stomp", "suffer",
                      "violent", "war","abusive",
                      "abuser","abused","abusing","damaged",
                      "damaging","attacked","attacking","attacker",
                      "attacks","attackers","crushes",
                      "crushing", "crushed", "dangerous","detroys", 
                      "destroyed", "destroying",
                      "fights", "fought", "fighting","harms", 
                      "harmed", "harming", "hurts",
                      "hurting", "kills", "killed", "killing",
                      "ruins", "ruined", "ruining", "spurns", 
                      "spurned", "spurning", "stomps", "stomped",
                      "stomping", "suffers", "suffered", "suffering",
                      "violence", "warring")

#fairness/reciprocity
original_mfd$f2hi[41:112] = NA
original_mfd$f2hi[1:40] = c("balance", "constant", "equal", "equate", "even",
                      "fair", "honest", "just", "justify", "reason",
                      "right", "tolerate", "impartial", "balances", 
                      "balanced", "balancing",
                      "equals", "equaled", "equaling", "equates", "equated",
                      "equating", "evens", "evened", "evening","fairness",
                      "honesty", "impariality", "justice", "justifies", 
                      "justified", "justifying", "reasons",
                      "reasoned", "reasoning", "rights", "tolerates",
                      "tolerated", "tolerating", "toleration")
original_mfd$f2lo[41:112] = NA
original_mfd$f2lo[1:40] = c("bias", "discriminate", "exclude", "favor",
                      "prefer", "prejudice", "biased", "biases", "discriminates", 
                      "discriminated", "discriminating", "discrimination",
                      "excludes", "excluded", "excluding", 
                      "favors", "favored", "favoring", "prefers", "preferred", "preferring",
                      "prejudices", "prejudiced", "prejudicing")

#ingroup/loyalty
original_mfd$i2hi[30:112] = NA
original_mfd$i2hi[1:29] = c("collect", "community", "family", "fellow",
                      "group", "member", "nation", "side", "together",
                      "trait", "unite", "collects", 
                      "collected", "collecting", "collective",
                      "communities", "families", "fellows",
                      "groups", "grouped", "grouping",
                      "members", "nations", "sides",
                      "togetherness", "traits", "unites", 
                      "united", "uniting")
original_mfd$i2lo[19:112] = NA 
original_mfd$i2lo[1:18] = c("deceive", "desert", "foreign", "individual",
                      "deceives", "deceived", "deceiving",
                      "deception", "deceptions", "deserts", "deserted",
                      "deserting", "desertion", "foreigners",
                      "indviduals", "indvidualize", "individualized",
                      "individualizing")

#authority/respect
original_mfd$a2hi[86:112] = NA
original_mfd$a2hi[1:85] = c("abide", "authority", "class", "command",
                      "control", "defer", "duty", "faith", "father", "honor",
                      "law", "lead", "legal", "mother", "obey", 
                      "order", "permit", "position", "preserve",
                      "respect", "revere", "serve", "tradtition", "trait",
                      "abides", "abided", "abiding", "authorities",
                      "classes", "classed", "classing", "command",
                      "commanded", "commanding", "controls", "controlled",
                      "controlling","defers", "deferred", "deferring", "deference",
                      "duties","faiths", "fathers", "fathered", "fathering",
                      "honors", "honored", "honoring", "laws", "leads",
                      "leading", "mothers", "mothered", "mothering",
                      "obeys", "obeyed", "obeying", "orders", 
                      "ordered", "ordering", "permits",
                      "permitted", "permitting", "positions", "positioned",
                      "positioning", "preserves", "preserved", "preserving",
                      "preservation", "respects",
                      "respected", "respecting", "respectful", "reveres",
                      "revered", "revering", "reverence", "serves", "served",
                      "serving", "traditions", "traditional", "traits")
original_mfd$a2lo[28:112] = NA
original_mfd$a2lo[1:27] = c("defect", "defy", "desert","oppose", 
                      "protest", "refuse", "defects", "defected", 
                      "defecting","defies", "defied", "defying", "defiance", "deserts",
                      "deserted", "deserting", "desertion", "opposes", "opposed",
                      "opposing", "protests", "protested", "protesting",
                      "refuses", "refused", "refusing", "refusal")

#purity/sanctity
original_mfd$p2hi[32:112] = NA
original_mfd$p2hi[1:31] = c("abstain", "church", "clean", "innocent", "modest",
                      "preserve", "pure", "right", "sacred", "whole",
                      "abstains", "abstained", "abstaining", "abstinence",
                      "churches", "cleans", "cleaned", "cleaning", "cleanse",
                      "cleanliness", "innocence", "modesty", "preserves", "preserved",
                      "preserving", "preservation", "purity", "rights", 
                      "sacredness", "wholeness", "wholesome")
original_mfd$p2lo[30:112] = NA
original_mfd$p2lo[1:29] = c("adultery", "dirt", "disease", "disgust", "gross",
                      "promiscuous", "ruin", "sick", "sin", "trash",
                      "adulteries", "adulterous", "adulterer", "adulterers",
                      "dirty", "diseases", "diseased", "disgusts", 
                      "disgusted", "disgusting", "grossness", "promiscuity",
                      "promiscuities", "ruins", "ruined","ruining", 
                      "sickness", "sicknesses", "sins")
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
 # original_mfd$h2[i] = stemDocument(original_mfd$h2[i], language = "english")
 # original_mfd$f2[i] = stemDocument(original_mfd$f2[i], language = "english")
 # original_mfd$i2[i] = stemDocument(original_mfd$i2[i], language = "english")
 # original_mfd$a2[i] = stemDocument(original_mfd$a2[i], language = "english")
 # original_mfd$p2[i] = stemDocument(original_mfd$p2[i], language = "english")
  
  original_mfd$h2hi[i] = stemDocument(original_mfd$h2[i], language = "english")
  original_mfd$h2lo[i] = stemDocument(original_mfd$h2[i], language = "english")
  original_mfd$f2hi[i] = stemDocument(original_mfd$f2[i], language = "english")
  original_mfd$f2lo[i] = stemDocument(original_mfd$f2[i], language = "english")
  original_mfd$i2hi[i] = stemDocument(original_mfd$i2[i], language = "english")
  original_mfd$i2lo[i] = stemDocument(original_mfd$i2[i], language = "english")
  original_mfd$a2hi[i] = stemDocument(original_mfd$a2[i], language = "english")
  original_mfd$a2lo[i] = stemDocument(original_mfd$a2[i], language = "english")
  original_mfd$p2hi[i] = stemDocument(original_mfd$p2[i], language = "english")
  original_mfd$p2lo[i] = stemDocument(original_mfd$p2[i], language = "english")
}

# final$hsum = NA
# final$fsum = NA
# final$isum = NA
# final$asum = NA
# final$psum = NA

final$hhisum = NA
final$hlosum = NA
final$fhisum = NA
final$flosum = NA
final$ihisum = NA
final$ilosum = NA
final$ahisum = NA
final$alosum = NA
final$phisum = NA
final$plosum = NA

for (i in 1:nrow(final)) {
##first make a table of a response, unlisting everything
temp = as.data.frame(table(unlist(strsplit(final$stemmed[i], " "))))
##find the rows that match the mfd list and sum and save
final$hsum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$h2[original_mfd$h2 != "" & original_mfd$h2 != "NA"])])
final$hsumhi[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$h2hi[original_mfd$h2hi != "" & original_mfd$h2hi != "NA"])])
final$hsumlo[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$h2lo[original_mfd$h2lo != "" & original_mfd$h2lo != "NA"])])

final$fsum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$f2[original_mfd$f2 != "" & original_mfd$f2 != "NA"])])
final$fsumhi[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$f2hi[original_mfd$f2hi != "" & original_mfd$f2hi != "NA"])])
final$fsumlo[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$f2lo[original_mfd$f2lo != "" & original_mfd$f2lo != "NA"])])

final$isum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$i2[original_mfd$i2 != "" & original_mfd$i2 != "NA"])])
final$isumhi[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$i2hi[original_mfd$i2hi != "" & original_mfd$i2hi != "NA"])])
final$isumlo[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$i2lo[original_mfd$i2lo != "" & original_mfd$i2lo != "NA"])])

final$asum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$a2[original_mfd$a2 != "" & original_mfd$a2 != "NA"])])
final$asumhi[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$a2hi[original_mfd$a2hi != "" & original_mfd$a2hi != "NA"])])
final$asumlo[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$a2lo[original_mfd$a2lo != "" & original_mfd$a2lo != "NA"])])

final$psum[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$p2[original_mfd$p2 != "" & original_mfd$p2 != "NA"])])
final$psumhi[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$p2hi[original_mfd$p2hi != "" & original_mfd$p2hi != "NA"])])
final$psumlo[i] = sum(temp$Freq[temp$Var1 %in% unique(original_mfd$p2lo[original_mfd$p2lo != "" & original_mfd$p2lo != "NA"])])
}

final$hper = final$hsum / final$wordcount * 100
final$hperhi = final$hsumhi / final$wordcount * 100
final$hperlo = final$hsumlo / final$wordcount * 100
final$fper = final$fsum / final$wordcount * 100
final$fperhi = final$fsumhi / final$wordcount * 100
final$fperlo = final$fsumlo / final$wordcount * 100
final$iper = final$isum / final$wordcount * 100
final$iperhi = final$isumhi / final$wordcount * 100
final$iperlo = final$isumlo / final$wordcount * 100
final$aper = final$asum / final$wordcount * 100
final$aperhi = final$asumhi / final$wordcount * 100
final$aperlo = final$asumlo / final$wordcount * 100
final$pper = final$psum / final$wordcount * 100
final$pperhi = final$psumhi / final$wordcount * 100
final$pperlo = final$psumlo / final$wordcount * 100
#let's look at the means
tapply(final$hper, final$Source, mean)
tapply(final$hperhi, final$Source, mean)
tapply(final$hperlo, final$Source, mean)
tapply(final$fper, final$Source, mean)
tapply(final$fperhi, final$Source, mean)
tapply(final$fperlo, final$Source, mean)
tapply(final$iper, final$Source, mean)
tapply(final$iperhi, final$Source, mean)
tapply(final$iperlo, final$Source, mean)
tapply(final$aper, final$Source, mean)
tapply(final$aperhi, final$Source, mean)
tapply(final$aperlo, final$Source, mean)
tapply(final$pper, final$Source, mean)
tapply(final$pperhi, final$Source, mean)
tapply(final$pperlo, final$Source, mean)


##Attempt at MLM
mlm_data = final[ , -c(3:48) ]
library(reshape)
long_mlm = melt(mlm_data,
                id = c("X", "Source"))
colnames(long_mlm) = c("partno", "Source", "moraltype", "percent")
long_mlm$lean = factor(long_mlm$Source,
                       levels = names(table(long_mlm$Source)),
                       labels = c("Conservative1", "Conservative",
                                  "Liberal1", "Liberal"))
long_mlm$lean = gsub("1", "", long_mlm$lean)

##set up the analysis
library(nlme)
#####intercept only model####
##gls = generalized least squares
##ML = maximum likelihood
model1 = gls(percent ~ 1, #DV ~ IV (which is only the intercept; is y-average diff than 0?)
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

####random intercept only model####
##note we switched to LME function
model2 = lme(percent ~ 1, 
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno) #sets random intercept for each participant
summary(model2) #note changed Value in output; also, 
#Random effects tells how much intercept varies among Pps
anova(model1, model2) #tests if necessary to nest; Yes, we need to do MLM

####predictor model####
model3 = lme(percent ~ lean, #now we want to switch the 1 for IV of interest
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3)
anova(model1, model2, model3)

model3.1 = lme(percent ~ lean + moraltype, #now we want to switch the 1 for IV of interest
             data = long_mlm, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3.1)

model3.2 = lme(percent ~ lean * moraltype, #now we want to switch the 1 for IV of interest
               data = long_mlm, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model3.2)

##create separate datasets for each moral foundation use subset

##model 3 on each moral foundation separately 


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



setwd("C:/Users/kayla/Documents/GitHub/Will-Pilot/exp1_data")

#Merge Exp2 text files and code partisan lean
library(stringr)
file.names = dir("C:/Users/kayla/Documents/GitHub/Will-Pilot/exp2_data/", pattern = '.csv')
first = TRUE
for(i in 1:length(file.names))
{
  df = read.csv(file.names[i], stringsAsFactors = FALSE)
  colnames(df)[1] = "Source"
  df$Text = gsub('\n', ' ', df$Text)
  df$Text = gsub('\t', ' ', df$Text)
  event = str_extract(file.names[i], '_[a-z]+')
  event = str_replace(event, "_", '')
  df$Event = rep(event, nrow(df))
  if(df$Source=='BLAZE'|df$Source=='BREITBART'|df$Source=='FOX'|df$Source=='HANNITY'|df$Source=='RUSH')
  {df$PartisanLean = rep('Conservative', nrow(df))}
  if(df$Source=='HUFFPO'|df$Source=='NPR'|df$Source=='NY Times'|df$Source=='POLITICO'|df$Source=='SLATE')
  {df$PartisanLean = rep('Liberal', nrow(df))}
  if(first==TRUE)
  {
    df_full = df
    first = FALSE
  }
  df_full = rbind(df_full, df)
}

write.csv(df_full, "exp2_merged data.csv", row.names = FALSE)

#Code Partisan Lean for Exp1
dfO = read.csv("finaldata.csv", stringsAsFactors = FALSE)
df = read.csv("finaldata_LIWC.csv", stringsAsFactors = FALSE)
colnames(df)[1:6] = colnames(dfO)[1:6]
df$PartisanLean = ifelse(df$Source=="Breitbart"|df$Source=="Fox News", "Conservative", "Liberal")
write.csv(df, 'finaldata_LIWC.csv', row.names = FALSE)

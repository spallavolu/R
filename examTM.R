rm(list=ls(all=TRUE))
library(plyr)
library(tm)

stopWords <- stopwords("en")
class(stopWords)

df1 <- data.frame(id = seq(1,5,1), string1 = NA)
head(df1)
df1$string1[1] <- "This string is a string."
df1$string1[2] <- "This string is a slightly longer string."
df1$string1[3] <- "This string is an even longer string."
df1$string1[4] <- "This string is a slightly shorter string."
df1$string1[5] <- "This string is the longest string of all the other strings."

head(df1)
df1$string1 <- tolower(df1$string1)
df1
str1 <-  strsplit(df1$string1[5], " ")
str1

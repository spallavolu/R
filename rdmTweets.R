#### http://www.rdatamining.com/examples/text-mining
rm(list = ls())
setwd('C:/INSOFE CSE7206C/Day 10/2. Text Processing with RdmTweets.RData')
getwd()


#You need to install libcurl for RCurl (which devtools depends on).
#sudo apt-get install libcurl4-gnutls-dev
#install dependent libraries RCurl, httr
library(twitteR)
# retrieve the first 100 tweets (or all tweets if fewer than 100)
# from the user timeline of @rdatammining
# rdmTweets <- userTimeline("rdatamining", n=100)
load("rdmTweets.RData")
n <- length(rdmTweets)
rdmTweets[1:3]
str(rdmTweets)

#Converting the Tweets to a data frame and then a corpus:
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)
library(SnowballC)
library(tm)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(df$text))
summary(myCorpus)

#Changing the character case, removing punctuations and removing stop words
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords("english"), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#Stemming Words:
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:3])
# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)
myCorpus <- tm_map(myCorpus, PlainTextDocument)

# Print the first three documents in the built corpus
inspect(myCorpus[1:3])
length(myCorpus)
# Building a Document-Term Matrix
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
length(myDtm)
inspect(myDtm)
myDtm
# Frequent Terms and Associations
findFreqTerms(myDtm, lowfreq=10)
# which words are associated with "r"?
findAssocs(myDtm, 'r', 0.30)
# which words are associated with "mining"?
# Here "miners" is used instead of "mining",
# because the latter is stemmed and then completed to "miners". :-(
findAssocs(myDtm, 'miners', 0.30)

# Building a Word Cloud:
library(wordcloud)
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)

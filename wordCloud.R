# R script for creating Reddit word clouds using TFIDF weighting
# Change "sub" to your favorite subreddit
# Will take 3-5 mins on large subreddits
# Certain subs (controversial, NSFW) appear to be missing
# Thanks to David Chudzicki for the R starter code
##This code is all from hamelg all I did was change the subbreddit.

library(RSQLite)
require(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

sub <- "borrow"
max_words <- 75

db <- src_sqlite('../input/database.sqlite', create = F)

db_subset <- db %>% 
  tbl('May2015') %>% 
  filter(subreddit == sub)

db_subset <- data.frame(db_subset)

corp <- Corpus(VectorSource(db_subset$body))
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corp,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))
dtm_sparse <- removeSparseTerms(dtm, 0.995)
post_words <- as.data.frame(as.matrix(dtm_sparse))

total_words <- data.frame(words = colnames(post_words),
                          counts = colSums(post_words))


wordcloud(words = total_words$words,
          freq=total_words$counts, 
          max.words = max_words,
          color = brewer.pal(8,"Dark2"))

print(total_words$words)

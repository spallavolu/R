rm(list=ls(all=TRUE))
getwd()
setwd("/Users/ravan/R")
library("e1071")
library("caret")
library("klaR")
library("RWeka")
library("openNLP")
SIP<-read.csv("SIP_Sampx.csv",stringsAsFactors =F)
SIP1 <- SIP[,c(1,6,28)]
# # SIP1$SPAM <- 0
# # Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, model = NULL)
# ## Need sentence and word token annotations.
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# # a2 <- annotate(SIP1$ContentSummary, list(sent_token_annotator, word_token_annotator))
# # sentence <- SIP1$ContentSummary[1]
# a<-tagPOS(sentence, language = "en")
# nrow(SIP1)
# grepl("http", SIP1$ContentSummary[1])
# # SIP1 <- transform(SIP1, SIP1$SPAM=apply(SIP1, 2, function(x) grepl("http", SIP1$ContentSummary)))
for(i in 1:nrow(SIP1)){
  if(grepl("http", SIP1$ContentSummary[i]) == T)
    SIP1$SPAM[i] <- 1
  else
    SIP1$SPAM[i] <- 0
}
warnings()
SIP2 <- subset(SIP1,SIP1$SPAM==0,-c(SPAM))
str(SIP2)
library(tm)
library(SnowballC)
Sys.setlocale('LC_ALL','C')
corpusposts <- VCorpus(VectorSource(SIP2$ContentSummary))
inspect(corpusposts)
corpusposts <- tm_map(corpusposts, removeNumbers, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removePunctuation, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removeWords, stopwords("english"), lazy=TRUE)
corpusposts <- tm_map(corpusposts, stripWhitespace, lazy=TRUE)
corpusposts <- tm_map(corpusposts, PlainTextDocument, lazy=TRUE)

# myDtm <- t(TermDocumentMatrix(corpusposts, control = list(minWordLength = 1)))

dtm <- DocumentTermMatrix(corpusposts,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))
dtm_sparse <- removeSparseTerms(dtm, 0.995)
post_words <- as.data.frame(as.matrix(dtm_sparse))
post_words 
total_words <- data.frame(words = colnames(post_words),
                          counts = colSums(post_words))
library(wordcloud)
library(RColorBrewer)
max_words <- 75
wordcloud(words = total_words$words,
          freq=total_words$counts, 
          max.words = max_words,
          color = brewer.pal(8,"Dark2"))

print(total_words$words)


library(slam)
colTotals <-  col_sums(myDtm)
colTotals
myDtm1 <- myDtm[,which(colTotals > 5)]
myDtm1
# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Code CLASS VECTOR
model <- naiveBayes(as.matrix(myDtm),as.factor(SIP2$Lead))

rawtest<-read.csv("SIP_Sample.csv",stringsAsFactors =F)
rawtest1 <- rawtest[,c(1,6)]
Sys.setlocale('LC_ALL','C')
testposts <- Corpus(VectorSource(rawtest1$ContentSummary))

testposts <- tm_map(testposts, removeNumbers, lazy=TRUE)
testposts <- tm_map(testposts, removePunctuation, lazy=TRUE)
testposts <- tm_map(testposts, removeWords, stopwords("english"), lazy=TRUE)
testposts <- tm_map(testposts, stripWhitespace, lazy=TRUE)
testposts <- tm_map(testposts, PlainTextDocument, lazy=TRUE)
inspect(testposts)
mytestDtm <- t(TermDocumentMatrix(testposts, control = list(minWordLength = 1)))
mytestDtm

# PREDICTION
results <- predict(model,as.matrix(mytestDtm))

results
# apply annotators to Corpus
Corpus.tagged <- lapply(SIP2$ContentSummary, function(x){
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  #  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
  y2w <- subset(y2, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  }  )

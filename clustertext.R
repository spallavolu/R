rm(list=ls(all=TRUE))
getwd()
setwd("/Users/ravan/R")
SIP<-read.csv("SIP_Sample.csv",stringsAsFactors =F)
SIP1 <- SIP[,c(1,6)]
str(SIP1)
library(tm)
library(SnowballC)
corpusposts <- Corpus(VectorSource(SIP1$ContentSummary))

corpusposts <- tm_map(corpusposts, removeNumbers, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removePunctuation, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removeWords, stopwords("english"), lazy=TRUE)
corpusposts <- tm_map(corpusposts, stripWhitespace, lazy=TRUE)
corpusposts <- tm_map(corpusposts, PlainTextDocument, lazy=TRUE)
#write.csv(corpusposts,"corpusposts")

##### ----------Step4: Create Term Document Matrix-----------########
myDtm <- TermDocumentMatrix(corpusposts, control = list(minWordLength = 1))

library(slam)
dtmposts <- DocumentTermMatrix(corpusposts)
termFrequency <- colSums(as.matrix(dtmposts))
str(dtmposts)
class(dtmposts)
tdmposts <- TermDocumentMatrix(corpusposts)
str(tdmposts)

##### Calculate frequency of terms
sparsedtm <- removeSparseTerms(dtmposts,0.99)
dim(sparsedtm)
termfrequency <- colSums(as.matrix(sparsedtm))
dtm2 <- as.matrix(sparsedtm)
distmatrix <- dist(scale(dtm2))
fit <- hclust(distmatrix, method="ward.D")
rect.hclust(fit,k=5)
fitmeans <- kmeans(dtm2,centers=5)
which(fitmeans$fitmeans.cluster==5)
dataframeclust <- data.frame(fitmeans$cluster)
clust1 <- data.frame(which(dataframeclust$fitmeans.cluster==5))
clustdtm <- data.frame(dtm2)
names(clust1) <- "Docs"

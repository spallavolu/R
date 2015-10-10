rm(list=ls(all=TRUE))
library('e1071')
library('SparseM')
library('tm')

# LOAD DATA FROM CSV
SIP<-read.csv("SIP_Sampx.csv",stringsAsFactors =F)
SIP1<-read.csv("SIP_Sample.csv",stringsAsFactors =F)
str(SIP)

# CREATE DATA FRAME OF 1000 TRAINING ARTICLES AND 500
# TEST ARTICLES INCLUDING 'text' AND 'Code' (columns 4 and 6)
traindata <- as.data.frame(SIP[1:514,c(1,6,28)])
testdata  <- as.data.frame(SIP1[,c(1,6)])

# SEPARATE TEXT VECTOR TO CREATE Source(),
# Corpus() CONSTRUCTOR FOR DOCUMENT TERM
# MATRIX TAKES Source()
trainvector <- as.vector(traindata$ContentSummary)
testvector <- as.vector(testdata$ContentSummary)

# DEBUGGING
 is.vector(trainvector);
 is.vector(testvector);

# CREATE SOURCE FOR VECTORS
trainsource <- VectorSource(trainvector)
testsource <- VectorSource(testvector)

# CREATE CORPUS FOR DATA
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)

# STEM WORDS, REMOVE STOPWORDS, TRIM WHITESPACE
traincorpus <- tm_map(traincorpus,stripWhitespace,lazy=T)
traincorpus <- tm_map(traincorpus,PlainTextDocument)
system.time(
  traincorpus <- tm_map(traincorpus, removeWords,lazy=T)
)

testcorpus <- tm_map(testcorpus,stripWhitespace,lazy=T)
testcorpus <- tm_map(testcorpus,PlainTextDocument)
system.time(
  testcorpus <- tm_map(testcorpus, removeWords,lazy=T)
)

library(SnowballC)
# CREATE DOCUMENT TERM MATRIX
trainmatrix <- t(TermDocumentMatrix(traincorpus))
testmatrix <- t(TermDocumentMatrix(testcorpus))

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Code CLASS VECTOR
model <- naiveBayes(as.matrix(trainmatrix),as.factor(traindata$Code))

# PREDICTION
results <- predict(model,as.matrix(testmatrix))
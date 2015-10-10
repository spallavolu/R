#### http://www.rdatamining.com/examples/text-mining

setwd('E:/IseLabsB10/Text/')
getwd()

## Text mining with the R package tm
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
# Install XML package, in linux u need to run the "sudo apt-get install libxml2-dev" command
library(XML)
reuters <- Corpus(DirSource(reut21578),
                  readerControl = list(reader = readReut21578XML))

inspect(reuters)

## Convert to Plain Text Documents
reuters <- tm_map(reuters, PlainTextDocument)
str(reuters[[1]])

## Convert to Lower Case
reuters <- tm_map(reuters, content_transformer(tolower))
reuters[[1]]

## Remove Stopwords
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]][1]
## Remove Punctuations
reuters <- tm_map(reuters, removePunctuation)
reuters[[1]]
## Stemming
reuters <- tm_map(reuters, stemDocument,language="english")

reuters[[1]]
## Remove Numbers
reuters <- tm_map(reuters, removeNumbers)
reuters[[1]]
## Eliminating Extra White Spaces
reuters <- tm_map(reuters, stripWhitespace)
reuters[[1]]
## create a term document matrix
dtm <- DocumentTermMatrix(reuters)

inspect(dtm[1:20, 1:100])

findFreqTerms(dtm, 90)
findAssocs(dtm, "washington", .4)

## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:20, 1:926])


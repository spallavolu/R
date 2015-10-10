rm(list=ls(all=TRUE))
getwd()
setwd("/Users/ravan/R")
library("e1071")
library("caret")
library("klaR")

library("NLP")
library("RWeka")
library("openNLP")
SIP<-read.csv("SIP_Sampx.csv",stringsAsFactors =F)
SIP1 <- SIP[,c(1,6,28)]
SIP1 <- as.data.frame(SIP1)

for(i in 1:nrow(SIP1)){
  if(grepl("http", SIP1$ContentSummary[i]) == T)
    SIP1$SPAM[i] <- 1
  else
    SIP1$SPAM[i] <- 0
}

SIP2 <- subset(SIP1,SIP1$SPAM==0,-c(SPAM))
str(SIP2)

text<- SIP1$ContentSummary
class(text)
text<-as.String(text)
class(text)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
text_annotations <- annotate(text, list(sent_ann, word_ann))

pos_ann <- Maxent_POS_Tag_Annotator()



class(text_annotations)
text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
sents(text_doc) %>% head(2)
words(text_doc) %>% head(10)
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
text_annotations <- annotate(text, pipeline)
text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(text_doc, kind = "person")
entities(text_doc, kind = "location")
entities(text_doc, kind = "organization")

Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, model = NULL)

text_pos <- annotate(text, pos_ann, text_annotations)
text_pos
## Variant with POS tag probabilities as (additional) features.
head(annotate(text, Maxent_POS_Tag_Annotator(probs = TRUE), text_annotations))

## Determine the distribution of POS tags for word tokens.
text_postag <- subset(text_pos, type == "word")
tags <- sapply(text_postag$features, `[[`, "POS")
tags
table(tags)
## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", text[text_postag], tags)

## Extract pairs of word tokens and POS tags for second sentence:
text_pos_sen2 <- annotations_in_spans(subset(text_pos, type == "word"),
                              subset(text_pos, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", text[text_pos_sen2], sapply(text_pos_sen2$features, `[[`, "POS"))


library(tm)
library(SnowballC)
Sys.setlocale('LC_ALL','C')
corpusposts <- VCorpus(VectorSource(SIP$ContentSummary))
inspect(corpusposts)
corpusposts <- tm_map(corpusposts, removeNumbers, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removePunctuation, lazy=TRUE)
corpusposts <- tm_map(corpusposts, removeWords, stopwords("english"), lazy=TRUE)
corpusposts <- tm_map(corpusposts, stripWhitespace, lazy=TRUE)
corpusposts <- tm_map(corpusposts, PlainTextDocument, lazy=TRUE)

myDtm <- t(TermDocumentMatrix(corpusposts, control = list(minWordLength = 1)))
library(slam)
colTotals <-  col_sums(myDtm)
colTotals
myDtm1 <- myDtm[,which(colTotals > 5)]
myDtm1


# library(tm)
# library(SnowballC)
# library(koRpus)
# treetag(SIP1$ContentSummary[1], treetagger = "kRp.env", rm.sgml = TRUE,
#         lang = "kRp.env", apply.sentc.end = TRUE,
#         sentc.end = c(".", "!", "?", ";", ":"),
#         encoding = NULL, TT.options = NULL, debug = FALSE,
#         TT.tknz = TRUE, format = "file", stopwords = NULL,
#         stemmer = NULL)
# 
# tagged.results <- treetag(SIP1$ContentSummary[1], treetagger="manual", lang="en",
#                           TT.options=list(path="/Users/ravan/R/treetagger", preset="en"))
# 
# tagged.text <- treetag("Emma.txt", treetagger="manual", lang="en", 
#                        TT.options=c(path="~/TreeTagger", preset="en"))
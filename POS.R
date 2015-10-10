##########################################################
### --- Part-of-Speech tagging and syntactic parsing with R
##########################################################
### --- R script "Part-of-Speech tagging and syntactic parsing with R"
### --- Author: Martin Schweinberger
### --- This script aims at an automated approach to POS tagging
### --- a sample corpus.
##########################################################
### --- Prepare data
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages we need or which may be useful
# (to activate just delete the #)
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("openNLP")
install.packages("NLP")
### additional packages
install.packages("tm")
install.packages("stringr")
install.packages("gsubfn")
install.packages("plyr")
# load packages
library(NLP)
library(openNLP)
library(openNLPmodels.en)
### load additional packages
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
# to install openNLPmodels, please download an install the packages/models direktly from
# http://datacube.wu.ac.at/. To install these packages/models, simply enter
#install.packages("foo", repos = "http://datacube.wu.ac.at/", type = "source")
# into your R console. E.g. enter:
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# to install the file "openNLPmodels.en_1.5-1.tar.gz"
##########################################################
# specify path of corpus
pathname <- "/Users/ravan/R"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = pathname, pattern = NULL, all.files = T,
                          full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T) }  )
# Paste all elements of the corpus together
corpus.tmp <- lapply(corpus.tmp, function(x){
  x <- paste(x, collapse = " ")  }  )
# Clean corpus
corpus.tmp <- lapply(corpus.tmp, function(x) {
  x <- enc2utf8(x)  }  )
corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
corpus.tmp <- str_trim(corpus.tmp, side = "both") # remove spaces at beginning and end of strings
# inspect result
str(corpus.tmp)
#>chr [1:3] "This is the first sentence in the first file of the test corpus. This is a second sentence in the test corpus but I am too lazy"| __truncated__ ...


# convert corpus files into strings
Corpus <- lapply(corpus.tmp, function(x){
  x <- as.String(x)  }  )
###############################################################
# Start actual PoS-tagging
# apply annotators to Corpus
Corpus.tagged <- lapply(Corpus, function(x){
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

# inspect results
Corpus.tagged

#>[[1]]
#>[1] "This/DT is/VBZ the/DT first/JJ sentence/NN in/IN the/DT first/JJ file/NN of/IN the/DT test/NN corpus/NN ./. This/DT is/VBZ a/DT second/JJ 
#>sentence/NN in/IN the/DT test/NN corpus/NN but/CC I/PRP am/VBP too/RB lazy/JJ to/TO write/VB much/RB more/RBR so/RB this/DT has/VBZ to/TO suffice/VB 
#>./. well/RB ,/, one/CD more/JJR sentence/NN should/MD do/VB ./."
#>
#>[[2]]
#>[1] "This/DT is/VBZ a/DT second/JJ file/NN with/IN some/DT sample/NN content/NN ./. It/PRP will/MD be/VB used/VBN to/TO test/VB a/DT part-of-
#>speech/NN tagger/NN in/IN R./NNP I/PRP dont/VBP really/RB know/VB if/IN it/PRP works/VBZ but/CC I/PRP definitely/RB hope/VBP so/RB ./."
#>
#>[[3]]
#>[1] "Finally/RB ,/, this/DT is/VBZ the/DT last/JJ file/NN of/IN the/DT test/NN corpus/NN and/CC I/PRP really/RB dont/VBP want/VB to/TO write/VB a/DT 
#>lot/NN more/RBR ./. Since/IN I/PRP am/VBP quite/RB lazy/JJ ,/, this/DT is/VBZ the/DT last/JJ sentence/NN in/IN my/PRP$ tiny/JJ test/NN corpus/NN ./."

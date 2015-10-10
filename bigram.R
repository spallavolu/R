data(crude)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
crudetdm <- TermDocumentMatrix(crude, control=list(stripWhitespace = TRUE,
                                                   removePunctuation = TRUE,
                                                   removeNumbers = TRUE,
                                                   stopwords = TRUE,
                                                   removeSparseTerms = TRUE,
                                                   tokenize = BigramTokenizer))

ListAssoc <- lapply(crudetdm$dimnames$Terms, function(x) findAssocs(crudetdm, x, 0.9))
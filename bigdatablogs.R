rm(list=ls(all=TRUE))
## Importing the Data into R for Analysis
## In case of large data, the data set cannot be called directly into language 
## like R for analysis. We need to store the data to HDFS and then do the 
## processing. As the given data is in csv format, we can use Sqoop to import
## data into HDFS. In this case we can also tranfer the file using command
## copyfromlocal
getwd()
setwd("/Users/ravan/R")
### what should we know in sqoop. 
### Specify file to be transferred
### Read the file to sqoop and write it to HDFS
### Source Location of file
### Destination of file
blogposts <- read.csv(file="posts.csv", header = TRUE)
### We have to read the csv file from HDFS 
### There are two options:
### 1. As data is in tabular format and sqoop create hive class,
###    read it directly to HIVE
### 2. As the post data will be dealing with is text (semi structured),
###    we can use PIG
### What factors influence our selection
### Reading file by itself does not possess much difficulty. What is critical
### is the ease with which we can do text analysis using PIG or Hive
### For each step we do in R, we will see if PIG and HIVE are supporting and
### if we are to use any additional components

###############-------Step0: Initial Scanning of Data--------############
names(blogposts)
### Question in R: Why do additional columns come
blogposts <- blogposts[,c(-12,-13,-14,-15)]
names(blogposts)
nrow(blogposts)
### Commands required for
### 1. Seeing column names of csv file
### 2. Dropping some of the columns
### 3. See few records of the file

##############--------Step2: Cleaning of Data---------#################
### check if data in any cells is missing
### check if data in each cell matches its column type
### Remove rows with cells having missing data
### Remove rows with cells satisfying certain criteria
### Place text in one column in a row to other column
str(blogposts)
blogposts$Post[1]
sum(is.na(blogposts$Post))

## Program to
## Note the row ids of records whose Post_Length cell has non numeric entries
## Shift Text is Post_Length to Post

### For our purpose, this is done in PIG and columns are extracted
### Read data processed with PIG using R

####------------Step2: Subset the Posts Column--------#####
pigposts <- read.csv(file = "pigposts.csv", header = FALSE)
names(pigposts)
pigposts[,2] <- 1:9503
pigposts <- pigposts[,c(2,1)]
pigposts[2,2]
names(pigposts) <- c("blogid","blogposts")
write.csv(pigposts,"pigpostsR.csv")

#### ----------Step3:Preprocess Text Data in Posts--------- #######
### 1. Remove numbers 2. Remove Symbols  3. Remove Stopwords
### R provides libraries for each of these. But, how to do them using
### PIG and Hive

### First step is to create corpus and process it
library(tm)
corpusposts <- Corpus(VectorSource(pigposts$blogposts))
inspect(corpusposts[2])
corpusposts <- tm_map(corpusposts, removeNumbers)
corpusposts <- tm_map(corpusposts, removePunctuation)
corpusposts <- tm_map(corpusposts, removeWords, stopwords("english"))
corpusposts <- tm_map(corpusposts, stripWhitespace)
corpusposts <- tm_map(corpusposts, content_transformer(tolower))
write.csv(corpusposts,"corpusposts")

##### ----------Step4: Create Term Document Matrix-----------########
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
fit <- hclust(distmatrix, method="ward")
rect.hclust(fit,k=5)
fitmeans <- kmeans(dtm2,centers=5)
which(fitmeans$fitmeans.cluster==5)
dataframeclust <- data.frame(fitmeans$cluster)
clust1 <- data.frame(which(dataframeclust$fitmeans.cluster==5))
clustdtm <- data.frame(dtm2)
names(clust1) <- "Docs"
clust1dtm <- merge(clust1, dtm2, by.x=which.dataframeclust.fitmeans.cluster...5)

res <- unlist(lapply(clust1dtm, function(x) if(is.numeric(x))sum(x,na.rm=T)))
data.frame(t(res))

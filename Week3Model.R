#Make sure libraries are loaded
install.packages("reshape2")
#if (!require(reshape2)) install.packages(reshape2, dependencies = TRUE)
library(reshape2)
install.packages("tm")
#if (!require(tm)) install.packages(tm, dependencies = TRUE)
library(tm)
install.packages("RWeka")
#if (!require(RWeka)) install.packages(RWeka, dependencies = TRUE)
library(RWeka)
install.packages("ggplot2")
#if (!require(ggplot2)) install.packages(ggplot2, dependencies = TRUE)
library(ggplot2)
install.packages("wordcloud")
#if (!require(wordcloud)) install.packages(wordcloud, dependencies = TRUE)
library(wordcloud)
install.packages("quanteda")
#if (!require(quanteda)) install.packages(quanteda, dependencies = TRUE)
library(quanteda)

#Set file path names
doc_blogs <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Uncompressed\\en_US.blogs.txt"
doc_news <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Uncompressed\\en_US.news.txt"
doc_twitter <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Uncompressed\\en_US.twitter.txt"

#Clean the data before creating the corpus

#Read data from file into vector data structure
text2vector = function(filepath)  {
  con = file(filepath, "r")
  return(readLines(con, -1))
  close(con)
}

#Pull data from individual files into corresponding vector data structures
blogsData <- text2vector(doc_blogs)
newsData <- text2vector(doc_news)
twitterData <- text2vector(doc_twitter)


#function for cleaning data
Text_To_Clean <- function(text_blob) {
  # swap all sentence ends with code 'ootoo'
  text_blob <- gsub(pattern=';|\\.|!|\\?', x=text_blob, replacement='ootoo')
  
  # swap all apostrophe's with code 'ooaoo'
  text_blob <- gsub(pattern="\\'", x=text_blob, replacement='ooaoo')
  
  # remove all non-alpha text (numbers etc)
  text_blob <- gsub(pattern="[^a-zA-Z]", x=text_blob, replacement = ' ')
  
  # force all characters to lower case
  text_blob <- tolower(text_blob)
  
  #add apostrophes back in
  text_blob <- gsub(pattern="ooaoo", x=text_blob, replacement="'")
  
  # remove contiguous spaces
  text_blob <- gsub(pattern="\\s+", x=text_blob, replacement=' ')
  
  # replace "n't"s with " not"
  text_blob <- gsub(pattern="n\\'t", x=text_blob, replacement=' not')
  
  # replace "'d"s with " would"
  text_blob <- gsub(pattern="\\'d", x=text_blob, replacement=' would')
  
  # replace "ll"s with " will"
  text_blob <- gsub(pattern="\\'ll", x=text_blob, replacement=' will')
  
  # replace "'re"s with " are"
  text_blob <- gsub(pattern="\\'re", x=text_blob, replacement=' are')
  
  # split sentences by split code
  sentence_vector <- unlist(strsplit(x=text_blob, split='ootoo',fixed = TRUE))
  
  return (sentence_vector)
}

Line <- 64
print(Line)

corpus_blogs <- Text_To_Clean(blogsData)
corpus_news <- Text_To_Clean(newsData)
corpus_twitter <- Text_To_Clean(twitterData)

#Save for future use
saveRDS(corpus_blogs, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\blogs.txt")
saveRDS(corpus_news, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\news.txt")
saveRDS(corpus_twitter, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\twitter.txt")

Line <- 76
print(Line)

#create corpus from saved clean files
docs <- Corpus(DirSource("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean"))

## DocumentTermMatrix does the tokenizing and forms a document term matrix
dtm_monograms <- DocumentTermMatrix(docs) 
print(dtm_monograms)

freq_monograms <- colSums(as.matrix(dtm_monograms))
freq_monograms<-sort(freq_monograms, decreasing=TRUE)
head(freq_monograms, 20)

monograms <- data.frame(word=names(freq_monograms), freq=freq_monograms)

Line <- 96
print(Line)

BigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
dtm_bigrams<-DocumentTermMatrix(docs, control=list(tokenize=BigramTokenizer))
print(dtm_bigrams)

freq_bigrams <-sort(colSums(as.matrix(dtm_bigrams)), decreasing=TRUE)
head(freq_bigrams, 20)

bigrams <-data.frame(word=names(freq_bigrams), freq=freq_bigrams)

Line <- 125
print(Line)

TrigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
dtm_trigrams<-DocumentTermMatrix(docs, control=list(tokenize=TrigramTokenizer))
print(dtm_trigrams)


freq_trigrams <- sort(colSums(as.matrix(dtm_trigrams)), decreasing=TRUE)
head(freq_trigrams, 20)

trigrams <- data.frame(word=names(freq_trigrams), freq=freq_trigrams)

Line <- 110
print(Line)

quadgramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
dtm_quadgrams<-DocumentTermMatrix(docs, control=list(tokenize=quadgramTokenizer))
print(dtm_quadgrams)

freq_quadgrams <- sort(colSums(as.matrix(dtm_quadgrams)), decreasing=TRUE)
head(freq_quadgrams, 12)

quadgrams <- data.frame(word=names(freq_quadgrams), freq=freq_quadgrams)

Line <- 141
print(Line)

#Load saved N-grams

monograms$word <- as.character(monograms$word)
#bigrams$word <- strsplit((bigrams$word), " ")
#firstword <- sapply(bigrams$word, "[", 1)
#secondword <- sapply(bigrams$word, "[", 2)
#bigrams <- cbind(firstword, secondword, bigrams[, -1])
monograms <- as.data.frame(monograms)
names(monograms) <- c("firstword", "monogramCount")

Line <- 154
print(Line)

bigrams <- as.data.frame(bigrams)
bigrams$word <- as.character(bigrams$word)
bigrams$word <- strsplit((bigrams$word), " ")
firstword <- sapply(bigrams$word, "[", 1)
secondword <- sapply(bigrams$word, "[", 2)
#bigrams <- cbind(firstword, secondword, bigrams[, -1])
bigrams$firstword <- firstword
bigrams$secondword <- secondword
bigrams <- bigrams[, -1]

Line <- 167
print(Line)

names(bigrams) <- c("bigramCount", "firstword", "secondword")
bigrams$firstword <- as.character(bigrams$firstword)
bigrams$secondword <- as.character(bigrams$secondword)
bigrams$bigramCount <- as.numeric(bigrams$bigramCount)

Line <- 175
print(Line)

bigDataFrame <- merge(monograms, bigrams, by=c("firstword"))
bigDataFrame <- bigDataFrame[,c("firstword", "secondword", "monogramCount", "bigramCount")]

#compute bigram probability p(secondword|firstword) = bigramCount/monogramCount
bigDataFrame$bigramProb <- bigDataFrame$bigramCount/bigDataFrame$monogramCount

Line <- 184
print(Line)

predictNextWord <- function(inputstring) {
  #inputstring is strings of words - split into first, second, etc
  inputstring <- as.character(inputstring)
  orderedButSplitWords <- strsplit(inputstring, " ")
  n <- length(orderedButSplitWords[[1]])
  print(n)
  nexttolastword <- orderedButSplitWords[[1]][n-1]
  lastword <- orderedButSplitWords[[1]][n]
  
  last2words <- c(nexttolastword, lastword)
  
  print(orderedButSplitWords)
  print(nexttolastword)
  print(lastword)
  print(last2words[1])
  print(last2words[2])
  
  
  #now, look up the 
  return(last2words)
}

predictNextWord <- function(inputstring, df) {
  #inputstring is strings of words - split into first, second, etc
  inputstring <- as.character(inputstring)
  orderedButSplitWords <- strsplit(inputstring, " ")
  n <- length(orderedButSplitWords[[1]])
  print(n)
  nexttolastword <- orderedButSplitWords[[1]][n-1]
  lastword <- orderedButSplitWords[[1]][n]
  
  last2words <- c(nexttolastword, lastword)
  
  print(orderedButSplitWords)
  print(nexttolastword)
  print(lastword)
  print(last2words[1])
  print(last2words[2])
  
  
  #now, look up the probabilites in the df for each bigram
  new_df <- df[df$firstword==last2words[1] & df$secondword==last2words[2]]
  print(new_df)
  return(last2words)
}


Line <- 234
print(Line)

bigrams <- as.data.frame(bigrams)
bigrams$word <- as.character(bigrams$word)
bigrams$word <- strsplit((bigrams$word), " ")
firstword <- sapply(bigrams$word, "[", 1)
secondword <- sapply(bigrams$word, "[", 2)
#bigrams <- cbind(firstword, secondword, bigrams[, -1])
bigrams$firstword <- firstword
bigrams$secondword <- secondword
bigrams <- bigrams[, -1]

names(bigrams) <- c("bigramCount", "firstword", "secondword")
bigrams$firstword <- as.character(bigrams$firstword)
bigrams$secondword <- as.character(bigrams$secondword)
bigrams$bigramCount <- as.numeric(bigrams$bigramCount)

calculate_bigram_prob <- function() {
  # in the bigram dataframe I want to add a column that is p(w2|w1)
  # to do this I need to look up the frequency count in the monograms dataframe
  # P is equal to count C of w2|w1 divided by C(total # of w1 in corpus)
  # I need to search the monograms dtm for w1 to get index into monograms, and add monograms$freq value to bigrams column
  totalCountForWord <- monograms$freq[which(monograms$word=="the")]
  
}

bigrams$firstWordCount <- bigram_prob()
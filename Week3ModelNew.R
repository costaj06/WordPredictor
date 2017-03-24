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
library(devtools)
devtools::install_github("kbenoit/readtext")
library(readtext)


#Set file path names
doc_blogs <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\encoded Dataset\\en_US.blogs.txt"
doc_news <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\encoded Dataset\\en_US.news.txt"
doc_twitter <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\encoded Dataset\\en_US.twitter.txt"

#Clean the data before creating the corpus

#Read data from file into vector data structure
text2vector = function(filepath)  {
  con = file(filepath, "r")
  return(readLines(con, n=-1, encoding = "UTF-8"))
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
  
  # replace "can't"s with "cannot"
  text_blob <- gsub(pattern="can\\'t", x=text_blob, replacement='cannot')
  
  # replace "n't"s with " not"
  text_blob <- gsub(pattern="n\\'t", x=text_blob, replacement=' not')
  
  # replace "'d"s with " would"
  text_blob <- gsub(pattern="\\'d", x=text_blob, replacement=' would')
  
  # replace "ll"s with " will"
  text_blob <- gsub(pattern="\\'ll", x=text_blob, replacement=' will')
  
  # replace "'re"s with " are"
  text_blob <- gsub(pattern="\\'re", x=text_blob, replacement=' are')
  
  # replace "'m"s with " am"
  text_blob <- gsub(pattern="\\'m", x=text_blob, replacement=' am')
  
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
#docs <- corpus("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean")
#filepath <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\*.txt"
filepath <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\encoded Dataset\\*.txt"
files <- readtext(filepath, encoding = "UTF-8")
docs <- corpus(files)

## DocumentTermMatrix does the tokenizing and forms a document term matrix
#dtm_monograms <- DocumentTermMatrix(docs) 
dtm_monograms <- dfm(docs, removeNumbers = TRUE, removePunct = TRUE)
print(dtm_monograms)

freq_monograms <- colSums(as.matrix(dtm_monograms))
rm(dtm_monograms)
gc()
freq_monograms<-sort(freq_monograms, decreasing=TRUE)
head(freq_monograms, 20)

monograms <- data.frame(word=names(freq_monograms), freq=freq_monograms)
rm(freq_monograms)
gc()
monograms <- monograms[monograms$freq > 10,]
#Save for future use
saveRDS(monograms, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\monograms")
rm(monograms)
gc()

#BigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
#dtm_bigrams<-dfm(docs, control=list(tokenize=BigramTokenizer))

#bigram_tokens <- tokens_ngrams(docs, n=2)
#dtm_bigrams <- dfm(bigram_tokens)

dtm_bigrams <- dfm(docs, ngrams = 2, removeNumbers = TRUE, removePunct = TRUE)
print(dtm_bigrams)

freq_bigrams <-sort(colSums(as.matrix(dtm_bigrams)), decreasing=TRUE)
head(freq_bigrams, 20)
rm(dtm_bigrams)
gc()

bigrams <-data.frame(word=names(freq_bigrams), freq=freq_bigrams)
rm(freq_bigrams)
gc()
bigrams <- bigrams[bigrams$freq > 10,]

#Save for future use
saveRDS(bigrams, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\bigrams")
rm(bigrams)
gc()

#TrigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
#dtm_trigrams<-DocumentTermMatrix(docs, control=list(tokenize=TrigramTokenizer))
dtm_trigrams <- dfm(docs, ngrams = 3, removeNumbers = TRUE, removePunct = TRUE)
print(dtm_trigrams)


freq_trigrams <- sort(colSums(as.matrix(dtm_trigrams)), decreasing=TRUE)
head(freq_trigrams, 20)
rm(dtm_trigrams)
gc()


trigrams <- data.frame(word=names(freq_trigrams), freq=freq_trigrams)
rm(freq_trigrams)
gc()
trigrams <- trigrams[trigrams$freq >= 4,]
saveRDS(trigrams, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\trigrams")
rm(trigrams)
gc()

#quadgramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
#dtm_quadgrams<-DocumentTermMatrix(docs, control=list(tokenize=quadgramTokenizer))
dtm_quadgrams<- dfm(docs, ngrams = 4, removeNumbers = TRUE, removePunct = TRUE)
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

#is this needed? 
#monograms <- as.data.frame(monograms)
names(monograms) <- c("firstword", "monogramCount")

Line <- 154
print(Line)

#bigrams <- as.data.frame(bigrams)
bigrams$word <- as.character(bigrams$word)
bigrams$word <- strsplit((bigrams$word), "_")
firstword <- sapply(bigrams$word, "[", 1)
secondword <- sapply(bigrams$word, "[", 2)
#bigrams <- cbind(firstword, secondword, bigrams[, -1])
bigrams$firstword <- firstword
bigrams$secondword <- secondword
bigrams <- bigrams[, -1]
names(bigrams) <- c("bigramCount", "firstword", "secondword")

Line <- 167
print(Line)

#split trigrams into first, second and third word
trigrams$word <- as.character(trigrams$word)
trigrams$word <- strsplit((trigrams$word), "_")
firstword <- sapply(trigrams$word, "[", 1)
secondword <- sapply(trigrams$word, "[", 2)
thirdword <- sapply(trigrams$word, "[", 3)
trigrams$firstword <- firstword
trigrams$secondword <- secondword
trigrams$thirdword <- thirdword
trigrams <- trigrams[, -1]
names(trigrams) <- c("trigramCount", "firstword", "secondword", "thirdword")

Line <- 167
print(Line)

#split quadgrams into first, second, third and fourth word
quadgrams$word <- as.character(quadgrams$word)
quadgrams$word <- strsplit((quadgrams$word), "_")
firstword <- sapply(quadgrams$word, "[", 1)
secondword <- sapply(quadgrams$word, "[", 2)
thirdword <- sapply(quadgrams$word, "[", 3)
fourthword <- sapply(quadgrams$word, "[", 4)
quadgrams$firstword <- firstword
quadgrams$secondword <- secondword
quadgrams$thirdword <- thirdword
quadgrams$fourthword <- fourthword
quadgrams <- quadgrams[, -1]
names(quadgrams) <- c("quadgramCount", "firstword", "secondword", "thirdword", "fourthword")

Line <- 167
print(Line)

#******************************************************

#function for getting query
get_query <- function() {
  # get query from ?
  
  return (user_input)
}

query <- get_query()



#******************************************************

bigDataFrame <- merge(monograms, bigrams, by=c("firstword"))
#merge in trigrams
bigDataFrame <- merge(bigDataFrame, trigrams, by=c("firstword", "secondword"))


bigDataFrame <- bigDataFrame[,c("firstword", "secondword", "monogramCount", "bigramCount")]

#compute bigram probability p(secondword|firstword) = bigramCount/monogramCount
bigDataFrame$bigramProb <- bigDataFrame$bigramCount/bigDataFrame$monogramCount

#merge in trigrams
bigDataFrame <- merge(bigDataFrame, trigrams, by=c("firstword", "secondword"))

Line <- 184
print(Line)

# predictNextWord takes as an input a string of words, the monograms, bigrams, trigrams, and quadgrams dataframes, and returns?
  predictNextWord <- function(inputstring, monograms, bigrams, trigrams, quadgrams) {
    #inputstring is strings of words - split into first, second, etc
    inputstring <- as.character(inputstring)
    tokenedWords <- strsplit(inputstring, " ")
    n <- length(tokenedWords[[1]])
    lastword <- tokenedWords[[1]][n]
    secondlastword <- tokenedWords[[1]][n-1]
    thirdlastword <- tokenedWords[[1]][n-2]
    
    if (n>=3) {
      #create a temporary dataframe containing potential matching quadgrams
      qgdf <- quadgrams[quadgrams$firstword==tokenedWords[[1]][n-2] & quadgrams$secondword==tokenedWords[[1]][n-1] & quadgrams$thirdword==tokenedWords[[1]][n], ]
      #for each matching case (i.e., each row), calculate probabilites of next word: 
      # P = quadgramCount / sum of counts of all potential matching quadrams
      qgdf$P <- qgdf$quadgramCount / sum(qgdf$quadgramCount)
      #generate weighted probability - highest weight because highest level of apriori information about query string
      qgW <- 0.8
      qgdf$WP <- qgW * qgdf$P
      #sort in descending order of probability
      qgdf <- qgdf[order(-qgdf$P), ]
    }
    
    if (n>=2) {
      #create a temporary dataframe containing potential matching trigrams
      tgdf <- trigrams[trigrams$firstword==tokenedWords[[1]][n-1] & trigrams$secondword==tokenedWords[[1]][n], ]
      #for each matching case (i.e., each row), calculate probabilites of next word: 
      # P = trigramCount / sum of counts of all potential matching trigrams
      tgdf$P <- tgdf$trigramCount / sum(tgdf$trigramCount)
      #generate weighted probability - relatively higher weight because relatively more apriori information about query string
      tgW <- 0.6
      tgdf$WP <- tgW * tgdf$P
      #sort in descending order of probability
      tgdf <- tgdf[order(-tgdf$P), ]
    }
    if (n>=1) {
      #create a temporary dataframe containing potential matching bigrams
      bgdf <- bigrams[bigrams$firstword==tokenedWords[[1]][n], ]
      #for each matching case (i.e., each row), calculate probabilites of next word: 
      # P = bigramCount / sum of counts of all potential matching birams
      bgdf$P <- bgdf$bigramCount / sum(bgdf$bigramCount)
      #generate weighted probability - lower weight because less apriori information about query string
      bgW <- 0.4
      bgdf$WP <- bgW * bgdf$P
      #sort in descending order of probability
      bgdf <- bgdf[order(-bgdf$P), ]
    }
    
    if (n>=0) {
      #create a temporary dataframe containing potential matching monograms
      mgdf <- monograms[1:10, ]
      #for each matching case (i.e., each row), calculate probabilites of next word: 
      # P = monogramCount / sum of counts of all potential matching birams
      mgdf$P <- mgdf$monogramCount / sum(mgdf$monogramCount)
      #generate weighted probability - lower weight because least amount of prior information about query string
      mgW <- 0.2
      mgdf$WP <- mgW * mgdf$P
      #sort in descending order of weighted probability
      mgdf <- mgdf[order(-mgdf$P), ]
    }
    
    
    #Extract top 10 most likely candidates from each temporary quad-, tri-, bi- and monogram dataframe
    #if (n==0){candidates <- mgdf[1:10,mgdf$]}
    mgvars <- c("WP", "firstword")
    mgdf <-mgdf[mgvars]
    names(mgdf) <- c("WP", "candidate")
    
    
    candidates <- mgdf[1:10,]
    if (n>=1) {
      bgvars <- c("WP", "secondword")
      bgdf <-bgdf[bgvars]
      names(bgdf) <- c("WP", "candidate")
      candidates <- rbind(candidates, bgdf[1:10,])
    }  
    if (n>=2) {
      tgvars <- c("WP", "thirdword")
      tgdf <-tgdf[tgvars]
      names(tgdf) <- c("WP", "candidate")
      candidates <- rbind(candidates, tgdf[1:10,])
    } 
    if (n>=3) {
      qgvars <- c("WP", "fourthword")
      qgdf <-qgdf[qgvars]
      names(qgdf) <- c("WP", "candidate")
      candidates <- rbind(candidates, qgdf[1:10,])
    }
    #Sort final candidates by weighted probabilities
    candidates <- candidates[order(-candidates$WP), ]
    
    #return most likely candidate (highest weighted probability)
    return(candidates[, 2])
  }
  
  
#  n <- length(tokenedWords[[1]])
#  # predictNextWord takes as an input a string of words, the monograms, bigrams, trigrams, and quadgrams dataframes, and returns?
#  predictNextWord <- function(inputstring, monograms, bigrams, trigrams, quadgrams) {
#    #inputstring is strings of words - split into first, second, etc
#    inputstring <- as.character(inputstring)
#    n <- length(tokenedWords[[1]])
#    lastword <- tokenedWords[[1]][n]
#    secondlastword <- tokenedWords[[1]][n-1]
#    thirdlastword <- tokenedWords[[1]][n-2]
#    
#    if (n>=3) {
#      #create a temporary dataframe containing potential matching quadgrams
#      qgdf <- quadgrams[quadgrams$firstword==tokenedWords[[1]][n-2] & quadgrams$secondword==tokenedWords[[1]][n-1] & quadgrams$thirdword==tokenedWords[[1]][n], ]
#      #for each matching case (i.e., each row), calculate probabilites of next word: 
#      # P = quadgramCount / sum of counts of all potential matching quadrams
#      qgdf$P <- qgdf$quadgramCount / sum(qgdf$quadgramCount)
#      #generate weighted probability - highest weight because highest level of apriori information about query string
#      qgW <- 0.8
#      qgdf$WP <- qgW * qgdf$P
#      #sort in descending order of probability
#      qgdf <- qgdf[order(-qgdf$P), ]
#    }
#    
#    if (n>=2) {
#      #create a temporary dataframe containing potential matching trigrams
#      tgdf <- trigrams[trigrams$firstword==tokenedWords[[1]][n-1] & trigrams$secondword==tokenedWords[[1]][n], ]
      #for each matching case (i.e., each row), calculate probabilites of next word: 
      # P = trigramCount / sum of counts of all potential matching trigrams
#      tgdf$P <- tgdf$trigramCount / sum(tgdf$trigramCount)
#      #generate weighted probability - relatively higher weight because relatively more apriori information about query string
#      tgW <- 0.6
#      tgdf$WP <- tgW * tgdf$P
#      #sort in descending order of probability
#      tgdf <- tgdf[order(-tgdf$P), ]
#    }
#    if (n>=1) {
#      #create a temporary dataframe containing potential matching bigrams
#      bgdf <- bigrams[bigrams$firstword==tokenedWords[[1]][n], ]
#      #for each matching case (i.e., each row), calculate probabilites of next word: 
#      # P = bigramCount / sum of counts of all potential matching birams
#      bgdf$P <- bgdf$bigramCount / sum(bgdf$bigramCount)
#      #generate weighted probability - lower weight because less apriori information about query string
#      bgW <- 0.4
#      bgdf$WP <- bgW * bgdf$P
#      #sort in descending order of probability
#      bgdf <- bgdf[order(-bgdf$P), ]
#    }
#    
#    if (n>=0) {
#      #create a temporary dataframe containing potential matching monograms
#      mgdf <- monograms[1:10, ]
#      #for each matching case (i.e., each row), calculate probabilites of next word: 
#      # P = monogramCount / sum of counts of all potential matching birams
#      mgdf$P <- mgdf$monogramCount / sum(mgdf$monogramCount)
#      #generate weighted probability - lower weight because least amount of prior information about query string
#      mgW <- 0.2
#      mgdf$WP <- mgW * mgdf$P
#      #sort in descending order of weighted probability
#      mgdf <- mgdf[order(-mgdf$P), ]
#    }
#    
#    
#    #Extract top 10 most likely candidates from each temporary quad-, tri-, bi- and monogram dataframe
#    #if (n==0){candidates <- mgdf[1:10,mgdf$]}
#    mgvars <- c("WP", "firstword")
#    mgdf <-mgdf[mgvars]
#    names(mgdf) <- c("WP", "candidate")
#    
#    
#    candidates <- mgdf[1:10,]
#    if (n>0) {
#      bgvars <- c("WP", "secondword")
#      bgdf <-bgdf[bgvars]
#      names(bgdf) <- c("WP", "candidate")
#      candidates <- rbind(candidates, bgdf[1:10,])
#    }  
#    if (n>1) {
#      tgvars <- c("WP", "thirdword")
#      tgdf <-tgdf[tgvars]
#      names(tgdf) <- c("WP", "candidate")
#      candidates <- rbind(candidates, tgdf[1:10,])
#    } 
#    if (n>1) {
#      qgvars <- c("WP", "fourthword")
#      qgdf <-qgdf[qgvars]
#      names(qgdf) <- c("WP", "candidate")
#      candidates <- rbind(candidates, qgdf[1:10,])
#    }
#    #Sort final candidates by weighted probabilities
#    candidates <- candidates[order(-candidates$WP), ]
#    
#    #return most likely candidate (highest weighted probability)
#    return(candidates[1, 2])
#  }
#  
#  
#  lastword <- tokenedWords[[1]][n]
#  secondlastword <- tokenedWords[[1]][n-1]
#  thirdlastword <- tokenedWords[[1]][n-2]

#if (n>=3) {
#  #create a temporary dataframe containing potential matching quadgrams
#  qgdf <- quadgrams[quadgrams$firstword==tokenedWords[[1]][n-2] & quadgrams$secondword==tokenedWords[[1]][n-1] & quadgrams$thirdword==tokenedWords[[1]][n], ]
#  #for each matching case (i.e., each row), calculate probabilites of next word: 
#  # P = quadgramCount / sum of counts of all potential matching quadrams
#  qgdf$P <- qgdf$quadgramCount / sum(qgdf$quadgramCount)
#  #generate weighted probability - highest weight because highest level of apriori information about query string
#  qgW <- 0.8
#  qgdf$WP <- qgW * qgdf$P
#  #sort in descending order of probability
#  qgdf <- qgdf[order(-qgdf$P), ]
#  }

#  if (n>=2) {
#    #create a temporary dataframe containing potential matching trigrams
#    tgdf <- trigrams[trigrams$firstword==tokenedWords[[1]][n-1] & trigrams$secondword==tokenedWords[[1]][n], ]
#    #for each matching case (i.e., each row), calculate probabilites of next word: 
#    # P = trigramCount / sum of counts of all potential matching trigrams
#    tgdf$P <- tgdf$trigramCount / sum(tgdf$trigramCount)
#    #generate weighted probability - relatively higher weight because relatively more apriori information about query string
#    tgW <- 0.6
#    tgdf$WP <- tgW * tgdf$P
#    #sort in descending order of probability
#    tgdf <- tgdf[order(-tgdf$P), ]
#  }
#  if (n>=1) {
#    #create a temporary dataframe containing potential matching bigrams
#    bgdf <- bigrams[bigrams$firstword==tokenedWords[[1]][n], ]
#    #for each matching case (i.e., each row), calculate probabilites of next word: 
#    # P = bigramCount / sum of counts of all potential matching birams
#    bgdf$P <- bgdf$bigramCount / sum(bgdf$bigramCount)
#    #generate weighted probability - lower weight because less apriori information about query string
#    bgW <- 0.4
#    bgdf$WP <- bgW * bgdf$P
#    #sort in descending order of probability
#    bgdf <- bgdf[order(-bgdf$P), ]
#  }
#  
#  if (n>=0) {
#    #create a temporary dataframe containing potential matching monograms
#    mgdf <- monograms[1:10, ]
#    #for each matching case (i.e., each row), calculate probabilites of next word: 
#    # P = monogramCount / sum of counts of all potential matching birams
#    mgdf$P <- mgdf$monogramCount / sum(mgdf$monogramCount)
#    #generate weighted probability - lower weight because least amount of prior information about query string
#    mgW <- 0.2
#    mgdf$WP <- mgW * mgdf$P
#    #sort in descending order of weighted probability
#    mgdf <- mgdf[order(-mgdf$P), ]
#    }

#  
#  #Extract top 10 most likely candidates from each temporary quad-, tri-, bi- and monogram dataframe
##if (n==0){candidates <- mgdf[1:10,mgdf$]}
#  mgvars <- c("WP", "firstword")
#  mgdf <-mgdf[mgvars]
#  names(mgdf) <- c("WP", "candidate")
#  
#  
#  candidates <- mgdf[1:10,]
#  if (n>0) {
#    bgvars <- c("WP", "secondword")
#    bgdf <-bgdf[bgvars]
#    names(bgdf) <- c("WP", "candidate")
#    candidates <- rbind(candidates, bgdf[1:10,])
#  }  
#  if (n>1) {
#    tgvars <- c("WP", "thirdword")
#    tgdf <-tgdf[tgvars]
#    names(tgdf) <- c("WP", "candidate")
#    candidates <- rbind(candidates, tgdf[1:10,])
#  } 
#  if (n>1) {
#    qgvars <- c("WP", "fourthword")
#    qgdf <-qgdf[qgvars]
#    names(qgdf) <- c("WP", "candidate")
#    candidates <- rbind(candidates, qgdf[1:10,])
#  }
#    #Sort final candidates by weighted probabilities
#  candidates <- candidates[order(-candidates$WP), ]
#  
#  #return most likely candidate (highest weighted probability)
#  return(candidates[1, 2])
#}
#  
#
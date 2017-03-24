monograms <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\app\\ngrams\\monograms")
bigrams <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\app\\ngrams\\bigrams")
trigrams <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\app\\ngrams\\trigrams")
quadgrams <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\app\\ngrams\\quadgrams")


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
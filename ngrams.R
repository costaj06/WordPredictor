library(quanteda)
library(readtext)
filepath <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\encoded Dataset\\*.txt"
files <- readtext(filepath, encoding = "UTF-8")
docs <- corpus(files)
rm(files)
gc()

dtm_monograms <- dfm(docs, removeNumbers = TRUE, removePunct = TRUE)
print(dtm_monograms)
Document-feature matrix of: 3 documents, 694,705 features (57.2% sparse).
 
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

#Make Quadrams
dtm_quadgrams <- dfm(docs, ngrams = 4, removeNumbers = TRUE, removePunct = TRUE)
print(dtm_quadgrams)
freq_quadgrams <- sort(colSums(as.matrix(dtm_quadgrams)), decreasing=TRUE)
head(freq_quadgrams, 20)
rm(dtm_quadgrams)
gc()

quadgrams <- data.frame(word=names(freq_quadgrams), freq=freq_quadgrams)
rm(freq_quadgrams)
gc()
quadgrams <- quadgrams[quadgrams$freq >= 4,]
saveRDS(quadgrams, file="C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\quadgrams")
rm(quadgrams)
gc()
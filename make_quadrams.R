library(quanteda)
library(readtext)

filepath <- "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\twitter\\*.txt"
files <- readtext(filepath, encoding = "UTF-8")
docs <- corpus(files)
rm(files)
gc()

quadgrams<- dfm(docs, ngrams = 4, removeNumbers = TRUE, removePunct = TRUE)
rm(docs)
gc()

quadgrams <- sort(colSums(as.matrix(quadgrams)), decreasing=TRUE)
head(quadgrams, 12)

quadgrams <- data.frame(word=names(quadgrams), freq=quadgrams)
quadgrams <- quadgrams[quadgrams$freq >= 2,]

saveRDS(quadgrams, file = "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\quadgrams_twitter")

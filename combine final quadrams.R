A <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\quadgrams_blogs")
B <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\quadgrams_news")
C <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\quadgrams_twitter")

#A <- rbind(A, B)
#qg <- aggregate(A, by = list("word"), FUN = "sum")

library(plyr)
df <- list(A, B, C)
qg <- do.call(rbind.fill,df)
quadgrams <- aggregate(qg$freq, by=list(Category=qg$word), FUN=sum)
names(quadgrams) <- c("word", "freq")
quadgrams <- quadgrams[order(-quadgrams$freq), ]
saveRDS(quadgrams, file = "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\quadgrams")


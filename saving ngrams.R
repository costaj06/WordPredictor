mg <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\monograms")
bg <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\bigrams")
tg <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\trigrams")
qg <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\quadgrams")

tg <- readRDS("C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\Clean\\trigrams")

saveRDS(mg, file = "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\monograms")
saveRDS(bg, file = "C:\\Users\\jcosta\\Dropbox\\DataScience\\Capstone\\Dataset\\ngrams\\bigrams")

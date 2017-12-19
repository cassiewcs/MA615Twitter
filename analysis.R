library(wordcloud)
library(tidytext)
library(stringr)
##Word Cloud##
## read csv files
setwd("~/Documents/R/twitter project")
thort<-read.csv("Thortext.csv", header = TRUE, sep = ",")
capt<-read.csv("Captext.csv", header = TRUE, sep = ",")
avent<-read.csv("Avengerstext.csv", header = TRUE, sep = ",")

##Thor##
#data manipulation
thortext= sapply(thort$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# remove retweet entities
thortext = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', thortext)
# remove at people
thortext = gsub('@\\w+', '', thortext)
# remove punctuation
thortext = gsub('[[:punct:]]', '', thortext)
# remove numbers
thortext = gsub('[[:digit:]]', '', thortext)
# remove html links
thortext = gsub('http\\w+', '', thortext)
# remove unnecessary spaces
thortext = gsub('[ \t]{2,}', '', thortext)
thortext = gsub('^\\s+|\\s+$', '', thortext)

th_corpus = Corpus(VectorSource(thortext))
tdm = TermDocumentMatrix(
  th_corpus,
  control = list(
    stopwords = c("just","now","much","ever","got","http","can","rt","htt",stopwords("english")),
    tolower = TRUE))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)
write.csv(dm, "wc1.csv", row.names = FALSE)
thorwc<-wordcloud(dm$word, dm$freq, random.order = FALSE, min.freq=5,colors = brewer.pal(8, "Dark2"))
thorwc

##CaptainAmerica
#data manipulation
captext= sapply(capt$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# remove retweet entities
captext = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', captext)
# remove at people
captext = gsub('@\\w+', '', captext)
# remove punctuation
captext = gsub('[[:punct:]]', '', captext)
# remove numbers
captext = gsub('[[:digit:]]', '', captext)
# remove html links
captext = gsub('http\\w+', '', captext)
# remove unnecessary spaces
captext = gsub('[ \t]{2,}', '', captext)
captext = gsub('^\\s+|\\s+$', '', captext)

cap_corpus = Corpus(VectorSource(captext))
tdmc = TermDocumentMatrix(
  cap_corpus,
  control = list(
    stopwords = c("just","now","much","ever","got","http","can","rt","htt","httpstco",stopwords("english")),
    tolower = TRUE))
mc = as.matrix(tdmc)
word_freqsc = sort(rowSums(mc), decreasing = TRUE) 
# create a data frame with words and their frequencies
dmc = data.frame(word = names(word_freqsc), freq = word_freqsc)
write.csv(dmc, "wc2.csv", row.names = FALSE)
capwc<-wordcloud(dmc$word, dmc$freq, random.order = FALSE, min.freq=5,colors = brewer.pal(8, "Dark2"))
capwc

##Avengers##
#data manipulation
aventext= sapply(avent$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
# remove retweet entities
aventext = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', aventext)
# remove at people
aventext = gsub('@\\w+', '', aventext)
# remove punctuation
aventext = gsub('[[:punct:]]', '', aventext)
# remove numbers
aventext = gsub('[[:digit:]]', '', aventext)
# remove html links
aventext = gsub('http\\w+', '', aventext)
# remove unnecessary spaces
aventext = gsub('[ \t]{2,}', '', aventext)
aventext = gsub('^\\s+|\\s+$', '', aventext)

aven_corpus = Corpus(VectorSource(aventext))
tdma = TermDocumentMatrix(
  aven_corpus,
  control = list(
    stopwords = c("just","now","much","ever","got","http","can","rt","htt","may",stopwords("english")),
    tolower = TRUE))
ma = as.matrix(tdma)
word_freqsa = sort(rowSums(ma), decreasing = TRUE) 
# create a data frame with words and their frequencies
dma = data.frame(word = names(word_freqsa), freq = word_freqsa)
write.csv(dma, "wc3.csv", row.names = FALSE)
avenwc<-wordcloud(dma$word, dma$freq, random.order = FALSE, min.freq=5,colors = brewer.pal(8, "Dark2"))
avenwc



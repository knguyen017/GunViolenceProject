GunViolence <- read.csv("~/Documents/GunViolence.csv", comment.char="#")
View(GunViolence)
install.packages("tm")
library(tm)
gv_corpus <- VCorpus(VectorSource(GunViolence$text))
print gv_corpus
print(gv_corpus)
inspect(gv_corpus[1:2])
as.character(gv_corpus[[1]])
lapply(gv_corpus[1:2],as.character)
gv_corpus_clean <- tm_map(gv_corpus,content_transformer(tolower))
as.character(gv_corpus[[1]])
library(stringr)
gv_corpus_clean <- tolower(str_trim(gv_corpus))
as.character(gv_corpus_clean[[1]])
gv_corpus_clean2 <- tm_map(gv_corpus_clean,content_transformer(tolower))
gv_corpus_clean <- tolower(str_trim(gv_corpus))
gv_corpus_clean <- tm_map(gv_corpus,content_transformer(tolower))
gv_corpus_clean2 <- tm_map(gv_corpus_clean,content_transformer(tolower))
gv_corpus_clean <- tm_map(gv_corpus, removeNumbers)
gv_corpus_clean$text <- iconv(enc2utf8(gv_corpus_clean$text),sub="byte")
GunViolence <- read.csv("~/Documents/GunViolence.csv", encoding="UTF8", comment.char="#")
View(GunViolence)
gv_corpus <- VCorpus(VectorSource(GunViolence$text))
print(gv_corpus)
inspect(gv_corpus[1:2])
as.character(gv_corpus[[1]])
lapply(gv_corpus[1:2], as.character)
gv_corpus_clean <- tm_map(gv_corpus,content_transformer(tolower))
gv_corpus_clean <- tm_map(gv_corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
as.character(gv_corpus_clean[[1]])
gv_corpus_clean <- tm_map(gv_corpus,content_transformer(tolower))
gv_corpus_clean <- tm_map(gv_corpus, content_transformer(tolower(gv_corpus$text) iconv(enc2utf8(gv_corpus$text), sub = "byte")))
gv_corpus_clean <- tm_map(gv_corpus, content_transformer(tolower iconv(enc2utf8(gv_corpus$text), sub = "byte")))
gv_corpus_clean <- tm_map(gv_corpus, content_transformer(stri_trans_tolower))
library(stringi)
gv_corpus_clean <- tm_map(gv_corpus, content_transformer(stri_trans_tolower))
as.character(gv_corpus_clean[[1]])
gv_corpus_clean <- tm_map(gv_corpus_clean, removeNumbers)
gv_corpus_clean <- tm_map(gv_corpus_clean, removeWords, stopwords())
gv_corpus_clean2 <- tm_map(gv_corpus_clean, function(x) iconv(x, to='UTF-8-MAC', sub='byte')
x
gv_corpus_clean2 <- tm_map(gv_corpus_clean, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
gv_corpus_clean2 <- tm_map(gv_corpus_clean2, removeWords, stopwords())
gv_corpus_clean2 <- tm_map(gv_corpus_clean2,removePunctuation)
library(SnowballC)
gv_corpus_clean2 <- tm_map(gv_corpus_clean2, stemDocument)
gv_corpus_clean2 <- tm_map(gv_corpus_clean2, stripWhitespace)
as.character(gv_corpus[1:3])
as.character(sms_corpus_clean[1:3])
as.character(gv_corpus_clean1[1:3])
as.character(gv_corpus_clean2[1:3])
gv_dtm <- DocumentTermMatrix(gv_corpus_clean3)
gv_dtm <- DocumentTermMatrix(gv_corpus_clean2)
gv_corpus_clean2 <- tm_map(gv_corpus_clean, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
as.character(gv_corpus_clean2[1:3])
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
x))})
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
gv_corpus_clean2<- tm_map(gv_corpus_clean,toSpace,"[^[:graph:]]")
as.character(gv_corpus_clean2[1:3])
clean_tweet = gsub("&amp", "", GunViolence)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
clean_tweet <- str_replace_all(clean_tweet," "," ")
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
clean_tweet
head(clean_tweet$text)
gv.text <- tolower(GunViolence)
gv.text <- gsub("rt", "", gv.text)
gv.text <- gsub("@\\w+", "", gv.text)
gv.text <- gsub("[[:punct:]]", "", gv.text)
gv.text <- gsub("http\\w+", "", gv.text)
gv.text <- gsub("[ |\t]{2,}", "", gv.text)
gv.text <- gsub("^ ", "", gv.text)
gv.text <- gsub(" $", "", gv.text)
gv.text.corpus <- Corpus(VectorSource(gv.text))
gv.text.corpus <- tm_map(gv.text.corpus, function(x)removeWords(x,stopwords()))
install.packages("wordcloud")
library(wordcloud)
wordcloud(gv.text.corpus,min.freq = 2,scale=c(7, 0.5), colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
tweets.text <- sapply(GunViolence, function(x) x$getText())
df2 <- subset(GunViolence, select = c(2))
head(df2)
tweets.text <- tolower(df2)
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
head(tweets.text)
head(GunViolence)
head(df2)
GunViolence <- read.csv("~/Documents/GunViolence.csv", encoding="UTF-8-MAC", comment.char="#")
View(GunViolence)
gv_c <- VCorpus(VectorSource(GunViolence$text))
print(gv_c)
inspect(gv_c[1:2])
tweets.text <- sapply(GunViolence, function(x) x$getText())
df2 <- subset(GunViolence, select = c(2))
tweets.text <- tolower(GunViolence)
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(1,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
GunViolence <- read.csv("~/Documents/GunViolence.csv", comment.char="#")
View(GunViolence)
gv <- read.delim("~/Documents/gv.txt", comment.char="#")
View(gv)
tweets.text <- tolower(gv)
head(tweets.text)
t<- tolower(gv)
head(t)
gv_new <- Corpus(VectorSource(GunViolence))
gv_new <- Corpus(VectorSource(gv))
inspect(docs)
inspect(gv_new)
glimpse(gv_new)
gv_new <- VCorpus(gv)
gv_new
gv <- read.delim("~/Documents/gv.csv", comment.char="#", stringsAsFactors=FALSE)
View(gv)
tweets.text <- tolower(gv)
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
savehistory("~/GV_RScript.R")

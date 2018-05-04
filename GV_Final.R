#gathering twitter data
install.packages("twitteR")
library(twitteR)
setup_twitter_oauth(***,***,***,***) #* used in place of twitter token
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-24')
#move to dataframe
tweets.df <-twListToDF(tweets)
write.csv("tweets1.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-25')
write.csv("tweets2.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-26')
write.csv("tweets3.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-27')
write.csv("tweets4.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-28')
write.csv("tweets5.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-29')
write.csv("tweets6.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-30')
write.csv("tweets7.csv")
tweets <- searchTwitter("#gunviolence OR #gun OR gun violence OR #marchforourlives OR #gunsafety", n=2000,since='2018-03-31')
write.csv("tweets8.csv")
#combined all files together and only kept column with actual tweet
#reading in my gun violence twitter file
gv <- read.delim("~/Documents/GunViolence_NoDups.csv", comment.char="#", stringsAsFactors=FALSE)
#installing and downloading different packages
install.packages("tm")
library(tm)
library(wordcloud)
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
library(SnowballC)
library(dplyr)
library(plyr)
library(cluster)
install.packages("dfm")
library(dfm)
install.packages("quanteda")
library(quanteda)
require(devtools)
library(caret)
library(maxent)
library(ggplot2)
library(fpc)
#cleaning the data to remove symbols, common words, and punction marks.
tweets.text <- tolower(gv)
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

##exploring dataset
freq <- colSums(as.matrix(review_gv))
length(freq)
ord <- order(freq)
m <- as.matrix(review_gv)
dim(m)
write.csv(m, file="DocumentsTermMatrix.csv")
#word frequency
freq <- colSums(as.matrix(dtm))
freq
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq,14)
findFreqTerms(dtm, lowfreq = 50)
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
code <- tm_map(gv_corpus, f, "[!\"#$%&'*+,./)(:;<=>?@\][\\^`{|}~]„")
wf <- data.frame(word=names(freq),freq-freq)
head(wf)
#plot word frequencies
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
geom_bar(stat = "identity") +
theme(axis.text.x=element_text(angle=45, hjust=1))
p
#find relationship between terms
findAssocs(review_gv, c("marchforourlives", "gunviolence"),corlimit=0.85)
#create wordcloud
set.seed(142)
wordcloud(names(freq),freq,min.freq=5)
wordcloud(names(freq),freq,min.freq=100)
#beginning sentiment analysis
sentiment <- analyzeSentiment(gv)
convertToBinaryResponse(sentiment)$SentimentQDAP
#created sa.csv to change "negative" to -1, "positive" to 1, and "neutral" to 0
write.csv(sentiment,"sa.csv")
#reading file back in
responseSA <- read.csv("~/Desktop/sa.csv", sep="")
#changing file to dataframe
as.data.frame(responseSA)
#updating datatype to numeric
sa <- as.numeric(unlist(responseSA))
#sentiment analysis
compareToResponse(sentiment, sa)
#plotting results of sentiment analysis
plotSentimentResponse(sentiment$SentimentQDAP, sa)
#creating matrix, finding best % to optimize clustering 
dtmss_gv <- removeSparseTerms(gv_dtm, 0.15)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.75)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.99)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.80)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.88)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.90)
dtmss_gv
dtmss_gv <- removeSparseTerms(gv_dtm, 0.97)
dtmss_gv
#hierarchical clustering
d <- dist(t(dtmss_gv), method="euclidian")
fit <- hclust(d=d,method="complete")
fit
plot(fit, hang=-1)
plot.new()
plot(fit, hang = -1)
groups <- cutree(fit,k=6)
rect.hclust(fit, k=6, border="red")
#kmeans (finding best fit)
d <- dist(t(dtmss_gv), method="euclidian")
kfit <- kmeans(d,2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
kfit <- kmeans(d,5)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
kfit <- kmeans(d,4)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
kfit <- kmeans(d,3)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
savehistory("~/GV_Week7.r")

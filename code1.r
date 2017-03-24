##### code 1 #########

# install.packages("twitteR")
# install.packages("RCurl")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("SnowballC")
#install.packages("dplyr")
#install.packages("plyr")

# libraries
library(plyr)
library(dplyr)
library(syuzhet)
library(SnowballC)
library(RCurl)
library(twitteR)
library(tm)
library(wordcloud)
library(plotrix)
library(rlist)

# access twitter API
consumer_key <- '$$$$$$$$$$$$$$$$$$$$$$$$$$'
consumer_secret <- '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
access_token <- '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
access_secret <- '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)

#downloading tweets
up_ele_tweets <- searchTwitter("#UPElection2017",n = 3900, lang = "en",resultType = "recent")
upEleTweets <- up_ele_tweets
up_ele_tweets <- upEleTweets


# writing tweets as dataframe
dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\dpy.csv")



up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())
write.csv(up_ele_tweets_text,"D:\\Documents\\Desktop\\Data Science\\Twitter\\up_ele_tweets_text.csv")


sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
s5 <- sum_tex5
sum_tx6 <- gsub("RT  ","", sum_tex5)

write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\26feb\\sum_txt6.csv")

# data frame is not good for text so convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

pal <- brewer.pal(8,"Dark2")

wordcloud(up_ele_clean,min.freq = 125,max.words = Inf, width=1000,height=1000,random.order  = TRUE,colors = pal)

up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")

wordcloud(up_ele_clean_up_ele,min.freq = 75,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)


mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL
library(ggplot2)
ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection")




tdm <- TermDocumentMatrix(up_ele_clean)
# <- DocumentTermMatrix(up_ele_clean, control = list(weighting = weightTfIdf))
mydata.df <- as.data.frame(inspect(tdm))
count<- as.data.frame(rowSums(mydata.df))
count$word = rownames(count)
colnames(count) <- c("count","word" )
count<-count[order(count$count, decreasing=TRUE), ]
write.csv(count,"D:\\Documents\\Desktop\\Data Science\\Twitter\\26feb\\count.csv")


View(tdm)


#####################################################################


#BJP


######################################################################


##### Twitter #########



up_ele_tweets <- searchTwitter("#UPElection2017+bjp",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and bjp")


###################################################################

#akhilesh

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017+akhilesh",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and akhilesk")

###################################################################

#modi

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + modi",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and modi")


###################################################################

#muslims

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + muslims",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and muslims")




###################################################################

#congress

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + congress",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and congress")


###################################################################

#pappu

####################################################################


up_ele_tweets <- searchTwitter("#UPElection2017 + pappu",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and pappu")



###################################################################

#mayawati

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + mayawati",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and mayawati")


###################################################################

#bsp

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + bsp",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 

# pal <- brewer.pal(8,"Dark2")
# 
# wordcloud(up_ele_clean,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)
# 
# up_ele_clean_up_ele <- tm_map(up_ele_clean, removeWords, "upelection")
# 
# wordcloud(up_ele_clean_up_ele,min.freq = 5,max.words = Inf, width=1000,height=1000,random.order  = FALSE,colors = pal)

mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and bsp")


###################################################################

#rahul

####################################################################3


up_ele_tweets <- searchTwitter("#UPElection2017 + rahul",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 


mysentiment <- get_nrc_sentiment(sum_tx6)

sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)

rownames(sentimentscore)<- NULL

ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("Total sentimets based on tweets of UPElection and rahul")



##code 2

#===============================================================================
        #Sentiment Score
#===============================================================================  

# writing tweets as dataframe
dpy <- ldply(upEleTweets,function(t) t$toDataFrame())
text <- sapply(up_ele_tweets, function(x) x$getText())

hu.liu.pos = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AAA_Go_Y3kJxQACFaVBem__ea/positive-words.txt?dl=1');
hu.liu.neg = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AABTGWHitlRZcddq1pPXOSqca/negative-words.txt?dl=1');

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}

# df <- as.data.frame(up_ele_clean_up_ele)
# 
# clean <- ldply(up_ele_clean_up_ele,function(t) t$toDataFrame())
dataframe<-data.frame(text=unlist(sapply(up_ele_clean_up_ele, `[`, "content")), 
                      stringsAsFactors=F)


#text <- sapply(dataframe, function(x) x$getText())


result = score.sentiment(dataframe$text,hu.liu.pos,hu.liu.neg)
dpy$score <- result$score
dpy$final_score <- dpy$score * dpy$retweetCount

colSums(dpy$final_score, dims = 2)
dpy$abs_score <- abs(dpy$final_score)
abs_count <- colSums(dpy[,-c(1:17)])
neg_col <- which(dpy$final_score <= 0)
neg_sum <- colSums(dpy[neg_col,-c(1:17)])
pos_sum <- colSums(dpy[-neg_col,-c(1:17)])
typeof(pos_sum)
per <- (pos_sum[2] / abs_count[2])*100

# Simple Pie Chart
slices <- c(pos_sum[2],neg_sum[2])

pos_twt <- as.character(pos_sum[2])
neg_twt <- as.character(neg_sum[2])

chart_pos_header <- paste("Positive(",pos_twt, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_twt,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)



# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 


install.packages("plotrix")
library(plotrix)
pie3D(slices,labels=lbls,explode=0.1,labelcex=1,
      main="Up Election 2017 General Sentiments %")




###################################################################

#pappu

####################################################################


up_ele_tweets <- searchTwitter("#UPElection2017 + pappu",n = 10000, lang = "en",resultType = "recent")
length(up_ele_tweets)

upEleTweets <- up_ele_tweets


dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 


############ START: Sentiment ######
dataframe<-data.frame(text=unlist(sapply(up_ele_clean, `[`, "content")), 
                      stringsAsFactors=F)

result = score.sentiment(dataframe$text,hu.liu.pos,hu.liu.neg)
dpy$score <- result$score
dpy$final_score <- dpy$score * dpy$retweetCount

dpy$abs_score <- abs(dpy$final_score)
abs_count <- colSums(dpy[,-c(1:17)])
neg_col <- which(dpy$final_score <= 0)
neg_sum <- colSums(dpy[neg_col,-c(1:17)])
pos_sum <- colSums(dpy[-neg_col,-c(1:17)])

per <- (pos_sum[2] / abs_count[2])*100

# Simple Pie Chart
slices <- c(pos_sum[2],neg_sum[2])

pos_twt <- as.character(pos_sum[2])
neg_twt <- as.character(neg_sum[2])

chart_pos_header <- paste("Positive(",pos_twt, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_twt,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)



# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 




pie3D(slices,labels=lbls,explode=0.1,labelcex=1,
      main="Up Election 2017 pappu Sentiments %")

############ END: Sentiment ######

################################### BJP plus MODI ####################

###################################################################

#modi

####################################################################3


up_ele_tweets_modi <- searchTwitter("#UPElection2017 + modi",n = 10000, lang = "en")
up_ele_tweets_bjp <- searchTwitter("#UPElection2017 + bjp",n = 10000, lang = "en")
up_ele_tweets_1 <- unique(append(up_ele_tweets_modi,up_ele_tweets_bjp))

up_ele_tweets_modi1 <- searchTwitter("election + modi",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_bjp1 <- searchTwitter("election + bjp",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_2 <- unique(append(up_ele_tweets_modi,up_ele_tweets_bjp))
up_ele_tweets <- unique(append(up_ele_tweets_2,up_ele_tweets_1))


up_ele_tweets_modi <- searchTwitter("#UPElection2017 + amit + shah",n = 10000, lang = "en",resultType = "recent")

dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 
up_ele_clean <- tm_map(up_ele_clean, removeWords, "bjp") # Removing #bjp as it is obviously will be there in 
up_ele_clean <- tm_map(up_ele_clean, removeWords, "modi") # Removing #modi as it is obviously will be there in 


############ START: Sentiment ######
dataframe<-data.frame(text=unlist(sapply(up_ele_clean, `[`, "content")), 
                      stringsAsFactors=F)

result = score.sentiment(dataframe$text,hu.liu.pos,hu.liu.neg)
dpy$score <- result$score
dpy$final_score <- dpy$score * dpy$retweetCount

dpy$abs_score <- abs(dpy$final_score)
abs_count <- colSums(dpy[,-c(1:17)])
neg_col <- which(dpy$final_score <= 0)
neg_sum <- colSums(dpy[neg_col,-c(1:17)])
pos_sum <- colSums(dpy[-neg_col,-c(1:17)])

per <- (pos_sum[2] / abs_count[2])*100

# Simple Pie Chart
slices <- c(pos_sum[2],neg_sum[2])

pos_twt <- as.character(pos_sum[2])
neg_twt <- as.character(neg_sum[2])

chart_pos_header <- paste("Positive(",pos_twt, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_twt,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)



# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 




pie3D(slices,labels=lbls,explode=0.1,labelcex=1,
      main="Up Election 2017 modi + bjp Sentiments %")

############ END: Sentiment ######



################################### sp + congress ####################

###################################################################

#akhilesh + rahul

####################################################################3


up_ele_tweets_akhilesh <- searchTwitter("#UPElection2017 + akhilesh",n = 10000, lang = "en")
up_ele_tweets_rahul <- searchTwitter("#UPElection2017 + rahul",n = 10000, lang = "en")
up_ele_tweets_sp <- searchTwitter("#UPElection2017 + SP",n = 10000, lang = "en")
up_ele_tweets_cong <- searchTwitter("#UPElection2017 + congress",n = 10000, lang = "en")
up_ele_tweets_samajwadi <- searchTwitter("#UPElection2017 + smajwadi",n = 10000, lang = "en")
up_ele_tweets_cycle <- searchTwitter("#UPElection2017 + cycle",n = 10000, lang = "en")
up_ele_tweets_samajwadi_party <- searchTwitter("#UPElection2017 + smajwadi + party",n = 10000, lang = "en")


up_ele_tweets1 <- unique(append(up_ele_tweets_akhilesh,up_ele_tweets_rahul))
up_ele_tweets2 <- unique(append(up_ele_tweets1,up_ele_tweets_sp))
up_ele_tweets3 <- unique(append(up_ele_tweets2,up_ele_tweets_cong))
up_ele_tweets4 <- unique(append(up_ele_tweets3,up_ele_tweets_samajwadi))
up_ele_tweets5 <- unique(append(up_ele_tweets4,up_ele_tweets_cycle))
up_ele_tweetss <- unique(append(up_ele_tweets5,up_ele_tweets_samajwadi_party))


up_ele_tweets_akhilesh <- searchTwitter("election + akhilesh",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_rahul <- searchTwitter("election + rahul",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_sp <- searchTwitter("election + SP",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_cong <- searchTwitter("election + congress",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_samajwadi <- searchTwitter("election + smajwadi",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_cycle <- searchTwitter("election + cycle",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_samajwadi_party <- searchTwitter("election + smajwadi + party",n = 10000, lang = "en",resultType = "recent")


up_ele_ttweets1 <- unique(append(up_ele_tweets_akhilesh,up_ele_tweets_rahul))
up_ele_ttweets2 <- unique(append(up_ele_ttweets1,up_ele_tweets_sp))
up_ele_ttweets3 <- unique(append(up_ele_ttweets2,up_ele_tweets_cong))
up_ele_ttweets4 <- unique(append(up_ele_ttweets3,up_ele_tweets_samajwadi))
up_ele_ttweets5 <- unique(append(up_ele_ttweets4,up_ele_tweets_cycle))
up_ele_ttweets <- unique(append(up_ele_ttweets5,up_ele_tweets_samajwadi_party))

up_ele_tweets <- unique(append(up_ele_ttweets,up_ele_tweetss))



dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 
up_ele_clean <- tm_map(up_ele_clean, removeWords, "bjp") # Removing #bjp as it is obviously will be there in 
up_ele_clean <- tm_map(up_ele_clean, removeWords, "modi") # Removing #modi as it is obviously will be there in 


############ START: Sentiment ######
dataframe<-data.frame(text=unlist(sapply(up_ele_clean, `[`, "content")), 
                      stringsAsFactors=F)

result = score.sentiment(dataframe$text,hu.liu.pos,hu.liu.neg)
dpy$score <- result$score
dpy$final_score <- dpy$score * dpy$retweetCount

dpy$abs_score <- abs(dpy$final_score)
abs_count <- colSums(dpy[,-c(1:17)])
neg_col <- which(dpy$final_score <= 0)
neg_sum <- colSums(dpy[neg_col,-c(1:17)])
pos_sum <- colSums(dpy[-neg_col,-c(1:17)])

per <- (pos_sum[2] / abs_count[2])*100

# Simple Pie Chart
slices <- c(pos_sum[2],neg_sum[2])

pos_twt <- as.character(pos_sum[2])
neg_twt <- as.character(neg_sum[2])

chart_pos_header <- paste("Positive(",pos_twt, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_twt,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)



# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 




pie3D(slices,labels=lbls,explode=0.1,labelcex=1,
      main="Up Election 2017 sp + cong Sentiments %")

############ END: Sentiment ######



################################### BSp  ####################

###################################################################

#mayawati + bsp

####################################################################3


up_ele_tweets_mayawati <- searchTwitter("#UPElection2017 + mayawati",n = 10000, lang = "en")
up_ele_tweets_bsp <- searchTwitter("#UPElection2017 + bsp",n = 10000, lang = "en")
up_ele_tweets_bahujan <- searchTwitter("#UPElection2017 + bahujan",n = 10000, lang = "en")

up_ele_tweets1 <- unique(append(up_ele_tweets_mayawati,up_ele_tweets_bsp))
up_ele_tweets_o <- unique(append(up_ele_tweets_bahujan,up_ele_tweets1))

up_ele_tweets_mayawati1 <- searchTwitter("election + mayawati",n = 10000, lang = "en", resultType = "recent")
up_ele_tweets_bsp1 <- searchTwitter("election + bsp",n = 10000, lang = "en",resultType = "recent")
up_ele_tweets_bahujan1 <- searchTwitter("election + bahujan",n = 10000, lang = "en",resultType = "recent")

up_ele_tweets11 <- unique(append(up_ele_tweets_mayawati1,up_ele_tweets_bsp1))
up_ele_tweets_o1 <- unique(append(up_ele_tweets_bahujan1,up_ele_tweets11))

up_ele_tweets <- unique(append(up_ele_tweets_o1,up_ele_tweets_o))



dpy <- ldply(up_ele_tweets,function(t) t$toDataFrame())
#write.csv(dpy,"D:\\Documents\\Desktop\\Data Science\\Twitter\\bjp\\1.csv")

up_ele_tweets_text <- sapply(up_ele_tweets, function(x) x$getText())

sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",up_ele_tweets_text)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

#write.csv(sum_tx6,"D:\\Documents\\Desktop\\Data Science\\Twitter\\sum_txt6.csv")

# data frame is not good for text convert it corpus
up_ele_corpus <- Corpus(VectorSource(sum_tx6))
up_ele_clean <- tm_map(up_ele_corpus, content_transformer(tolower)) #converting everything to lower cases
up_ele_clean <- tm_map(up_ele_clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
up_ele_clean <- tm_map(up_ele_clean, removeNumbers)
up_ele_clean <- tm_map(up_ele_clean, stripWhitespace)
up_ele_clean <- tm_map(up_ele_clean, removeWords, "UPElection2017") # Removing #UPElection2017 as it is obviously will be there in 


############ START: Sentiment ######
dataframe<-data.frame(text=unlist(sapply(up_ele_clean, `[`, "content")), 
                      stringsAsFactors=F)

result = score.sentiment(dataframe$text,hu.liu.pos,hu.liu.neg)
dpy$score <- result$score
dpy$final_score <- dpy$score * dpy$retweetCount

dpy$abs_score <- abs(dpy$final_score)
abs_count <- colSums(dpy[,-c(1:17)])
neg_col <- which(dpy$final_score <= 0)
neg_sum <- colSums(dpy[neg_col,-c(1:17)])
pos_sum <- colSums(dpy[-neg_col,-c(1:17)])

per <- (pos_sum[2] / abs_count[2])*100

# Simple Pie Chart
slices <- c(pos_sum[2],neg_sum[2])

pos_twt <- as.character(pos_sum[2])
neg_twt <- as.character(neg_sum[2])

chart_pos_header <- paste("Positive(",pos_twt, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_twt,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)



# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 




pie3D(slices,labels=lbls,explode=0.1,labelcex=1,
      main="Up Election 2017 BSP + mayawati Sentiments %")

############ END: Sentiment ######




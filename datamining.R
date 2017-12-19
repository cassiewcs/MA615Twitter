library(devtools)
library(twitteR)
library(RCurl)
library(ROAuth)
library(streamR)
library(base64enc)
library(data.table)
library(dplyr)
library(tm)
library(wordcloud)

##Twitter setup##
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_key <- 	"ojFsRKLGGTA9LiVctggB9gSD1"
api_secret <- "9Dhl5q46Fq2Fbo4mZmfnLtxSGk80Nybfpqp8vgtR0J1AZHvUud"
access_token <- "938168999833231360-0zt9Oh805wzuH6QUJXa1tKjjQvUO2o9"
access_token_secret <- "xACXgq5vrkuqtDDtawDKTFDsHtnN0RYn5OGkr9beb2RUU"

my_oauth <- OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret,requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")

setup_twitter_oauth(api_key, 
                    api_secret, 
                    access_token, 
                    access_token_secret)

##Data mining and parsing tweets##
load("my_oauth.Rdata")
setwd("~/Documents/R/twitter project")
#ThorRagnarok
filterStream( file.name="Thor.json",
              track = "ThorRagnarok", 
              language = "en",
              locations = c(-125,25,-66,50),  
              timeout = 600, oauth=my_oauth)
Thor.df <- parseTweets("Thor.json") #13759 tweets have been parsed.
thor.df<-as.data.table(Thor.df)
#CaptainAmericaCivilWar
filterStream( file.name="Captain.json",
              track = "CaptainAmericaCivilWar", 
              locations = c(-125,25,-66,50),  
              timeout = 600, oauth=my_oauth)
Cap.df <- parseTweets("Captain.json") #16468 tweets have been parsed.
cap.df<-as.data.table(Cap.df)
#InfinityWar
filterStream( file.name="Avengers.json",
              track = "InfinityWar", 
              locations = c(-125,25,-66,50),  
              timeout = 600, oauth=my_oauth)
inf.df <- parseTweets("Avengers.json") #16892 tweets have been parsed.
inf.df<-as.data.table(inf.df)

##Data mining for keywords and saving tweets##
#Thor
thort<-searchTwitter('@thorofficial',n=1000,lang = 'en')
thor.text = lapply(thort, function(t) t$getText())
thor.t <- data.frame(matrix(unlist(thor.text), nrow=1000, byrow=T),
                      stringsAsFactors=FALSE)
colnames(thor.t) <- "text"
thor.t$id <- c(1:nrow(thor.t))
write.csv(thor.t,"Thortext.csv",row.names = FALSE)
#CaptainAmerica
capt<-searchTwitter('@CaptainAmerica',n=1000,lang = 'en')
cap.text = lapply(capt, function(t) t$getText())
cap.t <- data.frame(matrix(unlist(cap.text), nrow=1000, byrow=T),
                      stringsAsFactors=FALSE)
colnames(cap.t) <- "text"
cap.t$id <- c(1:nrow(cap.t))
write.csv(cap.t,"Captext.csv",row.names = FALSE)
#Avengers
avent<-searchTwitter('@Avengers',n=1000,lang = 'en')
aven.text = lapply(avent, function(t) t$getText())
aven.t <- data.frame(matrix(unlist(aven.text), nrow=1000, byrow=T),
                      stringsAsFactors=FALSE)
colnames(aven.t) <- "text"
aven.t$id <- c(1:nrow(aven.t))
write.csv(aven.t,"Avengerstext.csv",row.names = FALSE)


##Data cleaning for mapping##
thor_tweets<-thor.df %>% 
  select(text, retweet_count, favorited, retweeted, 
                                created_at, verified,location, description, 
                                user_created_at, statuses_count, followers_count,
                                favourites_count, name, time_zone, friends_count,
                                place_lat, place_lon)
cap_tweets<-cap.df %>% 
  select(text, retweet_count, favorited, retweeted, 
                                created_at, verified,location, description, 
                                user_created_at, statuses_count, followers_count,
                                favourites_count, name, time_zone, friends_count,
                                place_lat, place_lon)
inf_tweets<-inf.df %>% 
  select(text, retweet_count, favorited, retweeted, 
                                created_at, verified,location, description, 
                                user_created_at, statuses_count, followers_count,
                                favourites_count, name, time_zone, friends_count,
                                place_lat, place_lon)
#delete NAs for lon/lat coordinates
thor_tweets1<-thor_tweets[complete.cases(thor_tweets$place_lat),]
cap_tweets1<-cap_tweets[complete.cases(cap_tweets$place_lat),]
inf_tweets1<-inf_tweets[complete.cases(inf_tweets$place_lat),]
#save as csv file
write.csv(thor_tweets1, "Thor3_tweets.csv", row.names = FALSE)
write.csv(cap_tweets1, "Cap3_tweets.csv", row.names = FALSE)
write.csv(inf_tweets1, "Avengers3_tweets.csv", row.names = FALSE)
#read csv
thor<-read.csv("Thor3_tweets.csv",header = TRUE, sep = ",")
thor<-data.frame(thor)
cap<-read.csv("Cap3_tweets.csv",header = TRUE, sep = ",")
cap<-data.frame(cap)
aven<-read.csv("Avengers3_tweets.csv",header = TRUE, sep = ",")
aven<-data.frame(aven)

##Data mining and cleaning for emoji##
#reference:https://prismoji.com/2017/02/06/emoji-data-science-in-r-tutorial/
ht1 <- '#ThorRagnarok' 
tweets.raw1 <- searchTwitter(ht1, n = 3000, lang = 'en')
df1 <- twListToDF(strip_retweets(tweets.raw1, strip_manual = TRUE, strip_mt = TRUE))
df1$hashtag <- ht1
df1$created <- as.POSIXlt(df1$created)
df1$text <- iconv(df1$text, 'latin1', 'ASCII', 'byte')
df1$url <- paste0('https://twitter.com/', df1$screenName, '/status/', df1$id)
names(df1)[12]<-"retweets"
df1.a <- subset(df1, select = c(text, created, url, latitude, longitude, retweets, hashtag))
df1$username <- substr(substr(df1$url, 21, nchar(as.character(df1$url))), 1, 
                       nchar(substr(df1$url, 21, nchar(as.character(df1$url))))-26);
tweets.full1 <- df1
tweets.full1$X <- NULL
tweets.full1$z <- 1
tweets.full1$created <- as.POSIXlt(tweets.full1$created)
tweets.dupes1 <- tweets.full1[duplicated(tweets.full1$url), ]
tweets1 <- tweets.full1[!duplicated(tweets.full1$url), ]
tweets1$z <- 1
tweets1$created <- as.POSIXct(tweets1$created)
tweets1 <- arrange(tweets1, url)
row.names(tweets1) <- NULL
tweets1$tweetid <- as.numeric(row.names(tweets1))
nrow(tweets1)
write.csv(tweets1, "tweets1.csv", row.names = FALSE)

#CaptainAmericaCivilWar
ht2<-"CaptainAmericaCivilWar"
tweets.raw2 <- searchTwitter(ht2, n = 30000, lang = 'en')
df2 <- twListToDF(strip_retweets(tweets.raw2, strip_manual = TRUE, strip_mt = TRUE))
df2$hashtag <- ht2
df2$created <- as.POSIXlt(df2$created)
df2$text <- iconv(df2$text, 'latin1', 'ASCII', 'byte')
df2$url <- paste0('https://twitter.com/', df2$screenName, '/status/', df2$id)
names(df2)[12]<-"retweets"
df2.a <- subset(df2, select = c(text, created, url, latitude, longitude, retweets, hashtag))
df2$username <- substr(substr(df2$url, 21, nchar(as.character(df2$url))), 1, 
                       nchar(substr(df2$url, 21, nchar(as.character(df2$url))))-26);
tweets.full2 <- df2
tweets.full2$X <- NULL
tweets.full2$z <- 1
tweets.full2$created <- as.POSIXlt(tweets.full2$created)
tweets.dupes2 <- tweets.full2[duplicated(tweets.full2$url), ]
tweets2 <- tweets.full2[!duplicated(tweets.full2$url), ]
tweets2$z <- 1
tweets2$created <- as.POSIXct(tweets2$created)
tweets2 <- arrange(tweets2, url)
row.names(tweets2) <- NULL
tweets2$tweetid <- as.numeric(row.names(tweets2))
nrow(tweets2)
write.csv(tweets2, "tweets2.csv", row.names = FALSE)

#InfinityWar
ht3<-"InfinityWar"
tweets.raw3 <- searchTwitter(ht3, n = 3000, lang = 'en')
df3 <- twListToDF(strip_retweets(tweets.raw3, strip_manual = TRUE, strip_mt = TRUE))
df3$hashtag <- ht3
df3$created <- as.POSIXlt(df3$created)
df3$text <- iconv(df3$text, 'latin1', 'ASCII', 'byte')
df3$url <- paste0('https://twitter.com/', df3$screenName, '/status/', df3$id)
names(df3)[12]<-"retweets"
df3.a <- subset(df3, select = c(text, created, url, latitude, longitude, retweets, hashtag))
df3$username <- substr(substr(df3$url, 21, nchar(as.character(df3$url))), 1, 
                       nchar(substr(df3$url, 21, nchar(as.character(df3$url))))-26);
tweets.full3 <- df3
tweets.full3$X <- NULL
tweets.full3$z <- 1
tweets.full3$created <- as.POSIXlt(tweets.full3$created)
tweets.dupes3 <- tweets.full3[duplicated(tweets.full3$url), ]
tweets3 <- tweets.full3[!duplicated(tweets.full3$url), ]
tweets3$z <- 1
tweets3$created <- as.POSIXct(tweets3$created)
tweets3 <- arrange(tweets3, url)
row.names(tweets3) <- NULL
tweets3$tweetid <- as.numeric(row.names(tweets3))
nrow(tweets3)
write.csv(tweets3, "tweets3.csv", row.names = FALSE)

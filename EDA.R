library(sp)
library(RgoogleMaps)
library(ggmap)
library(tidyverse)
library(maptools)
library(datasets)
library(tigris)
library(leaflet)
library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)
library(png)
library(gridExtra)

##Map##
thor<-read.csv("Thor3_tweets.csv",header = TRUE, sep = ",")
thor<-data.frame(thor)
cap<-read.csv("Cap3_tweets.csv",header = TRUE, sep = ",")
cap<-data.frame(cap)
aven<-read.csv("Avengers3_tweets.csv",header = TRUE, sep = ",")
aven<-data.frame(aven)
#Thor
thor_map <- leaflet(thor) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash;
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')
thor_map %>% addCircles(~place_lon, ~place_lat,
                        popup=thor$place_lon, weight =5, radius=40,
                      color="red", stroke = TRUE, fillOpacity = 0.5)

#CaptainAmerica
cap_map <- leaflet(cap) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash;
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')
cap_map %>% addCircles(~place_lon, ~place_lat,
                        popup=cap$place_lon, weight =5, radius=50,
                        color="royalblue", stroke = TRUE, fillOpacity = 0.8)

#Avengers
aven_map <- leaflet(aven) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash;
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')
aven_map %>% addCircles(~place_lon, ~place_lat,
                        popup=thor$lon, weight =5, radius=50,
                        color="thistle", stroke = TRUE, fillOpacity = 0.8)


##Emoji##
tweets1<-read.csv("tweets1.csv",header = TRUE, sep = ",")
tweets1<-data.frame(tweets1)
tweets2<-read.csv("tweets2.csv",header = TRUE, sep = ",")
tweets2<-data.frame(tweets2)
tweets3<-read.csv("tweets3.csv",header = TRUE, sep = ",")
tweets3<-data.frame(tweets3)

#Emoji dictionary
emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F)#Lauren Ancona; https://github.com/laurenancona/twimoji/tree/master/twitterEmojiProject
emdict.la <- emdict.la[-1, ]; row.names(emdict.la) <- NULL; names(emdict.la) <- c('unicode', 'bytes', 'name'); emdict.la$emojiid <- row.names(emdict.la);
emdict.jpb <- read.csv('emDict.csv', header = F) #Jessica Peterka-Bonetta; http://opiateforthemass.es/articles/emoticons-in-R/
emdict.jpb <- emdict.jpb[-1, ]; row.names(emdict.jpb) <- NULL; names(emdict.jpb) <- c('name', 'bytes', 'rencoding'); emdict.jpb$name <- tolower(emdict.jpb$name);
emdict.jpb$bytes <- NULL;
emojis <- merge(emdict.la, emdict.jpb, by = 'name');  emojis$emojiid <- as.numeric(emojis$emojiid); emojis <- arrange(emojis, emojiid);

##Thor##
#create full tweets by emojis matrix
df.s <- matrix(NA, nrow = nrow(tweets1), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets1$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s);df.t <- data.frame(df.s); df.t$tweetid <- tweets1$tweetid;
# merge in hashtag data from original tweets dataset
df.a <- subset(tweets1, select = c(tweetid, hashtag));
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1;df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;
## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets1)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets1) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
# print summary stats
subset(emojis.count.p, rank <= 10);
num.tweets <- nrow(tweets1); df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1); num.tweets.with.emojis <- length(df.t[df.t > 0]); num.emojis <- sum(emojis.count$count);
min(tweets1$created); max(tweets1$created); median(tweets1$created);
num.tweets; num.tweets.with.emojis; round(100 * (num.tweets.with.emojis / num.tweets), 1); num.emojis; nrow(emojis.count);
#MAKE BAR CHART OF TOP EMOJIS 
df.plot <- subset(emojis.count.p, rank <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd("~/Documents/R/twitter project/ios_9_3_emoji_files")
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k; 
df.plot <- arrange(df.plot, name);
write.csv(df.plot, "emoji1.csv", row.names = FALSE)
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  labs(title="Emoji for ThorRagnarok")+
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1;

##Captain America
#create full tweets by emojis matrix
df.s <- matrix(NA, nrow = nrow(tweets2), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets2$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s);df.t <- data.frame(df.s); df.t$tweetid <- tweets2$tweetid;
# merge in hashtag data from original tweets dataset
df.a <- subset(tweets2, select = c(tweetid, hashtag));
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1;df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;
## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets2)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets2) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
# print summary stats
subset(emojis.count.p, rank <= 10);
num.tweets <- nrow(tweets2); df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1); num.tweets.with.emojis <- length(df.t[df.t > 0]); num.emojis <- sum(emojis.count$count);
min(tweets2$created); max(tweets2$created); median(tweets2$created);
num.tweets; num.tweets.with.emojis; round(100 * (num.tweets.with.emojis / num.tweets), 1); num.emojis; nrow(emojis.count);
#MAKE BAR CHART OF TOP EMOJIS 
df.plot <- subset(emojis.count.p, rank <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd("~/Documents/R/twitter project/ios_9_3_emoji_files")
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k; 
df.plot <- arrange(df.plot, name);
write.csv(df.plot, "emoji2.csv", row.names = FALSE)
g2 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  labs(title="Emoji for CaptainAmericaCivilWar")+
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g2;


##InfinityWar##
df.s <- matrix(NA, nrow = nrow(tweets3), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets3$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s);df.t <- data.frame(df.s); df.t$tweetid <- tweets3$tweetid;
# merge in hashtag data from original tweets dataset
df.a <- subset(tweets3, select = c(tweetid, hashtag));
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1;df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;
## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets3)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets3) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
# print summary stats
subset(emojis.count.p, rank <= 10);
num.tweets <- nrow(tweets3); df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1); num.tweets.with.emojis <- length(df.t[df.t > 0]); num.emojis <- sum(emojis.count$count);
min(tweets3$created); max(tweets3$created); median(tweets3$created);
num.tweets; num.tweets.with.emojis; round(100 * (num.tweets.with.emojis / num.tweets), 1); num.emojis; nrow(emojis.count);
#MAKE BAR CHART OF TOP EMOJIS 
df.plot <- subset(emojis.count.p, rank <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd("~/Documents/R/twitter project/ios_9_3_emoji_files")
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k; 
df.plot <- arrange(df.plot, name);
write.csv(df.plot, "emoji3.csv", row.names = FALSE)
g3 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  labs(title="Emoji for InfinityWar")+
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g3
grid.arrange(g1,g2,g3,ncol=3)

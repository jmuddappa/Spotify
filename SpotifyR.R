#Spotify API Test with Shiny UI

#install.packages("spotifyr")
#install.packages("tidyverse")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("httr")


library(spotifyr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library("tm")
library("SnowballC")
library(tidytext)
library(httr)
library(tidyverse)

#using client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = '6e7cff976750491eafd9f25577e3e12c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '5c925185ba3d4e6d90726be944bfe77a')

artist_name <- 'Vicetone'
artist_data <- get_artist_audio_features(artist_name)
count(artist_data)
db <-artist_data %>% arrange(-album_popularity) %>% select(album_name, album_popularity) 
#db <- db[- grep("Remastered", db$track_name),]
#db <- db[- grep("live", db$track_name),]
db = as.data.frame(db[-1,])
unique(db)
plot(db)

#Take a user and generate a wordcloud of their favorite words in songs

playlists <- get_user_playlists('lisashirai')
playlist_tracks <- get_playlist_tracks(playlists)
text <- unique(playlist_tracks$track_name)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("feat", "remix","live","radio","edit","cover","mix")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

set.seed(1234)
wordcloud(words=d$word, freq = d$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


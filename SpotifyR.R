#Spotify API Test

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

artist_name <- 'joy division'
artist <- get_artist_audio_features(artist_name)
db <-joy %>% arrange(-valence) %>% select(track_name, valence) 
db = as.data.frame(db[-1,])
db
plot(db$valence)

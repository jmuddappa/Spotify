library(shiny)
library(spotifyr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(tidytext)
library(httr)
library(tidyverse)
options(shiny.sanitize.errors = TRUE)

server <- function(input, output) {
  getTermMatrix <- function(user)
  {
    Sys.setenv(SPOTIFY_CLIENT_ID = YOUR_CLIENT_ID)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = YOUR_SECRET_ID)
    playlists <- get_user_playlists(user)
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
    docs <- tm_map(docs, removeWords, c("feat", "remix","live","radio","edit","cover","album","version","featuring")) 
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
  }
  terms <- reactive({
    input$update
    isolate({
      #setProgress(message="Processing user...")
      validate(
        need(input$username1 != "", "Please input your username")
        )
      
      getTermMatrix(input$username1)
    })
  })
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), freq = v, min.freq = 1, scale=c(2,0.5),
                  max.words=input$max, random.order=FALSE, rot.per=0.3, 
                  colors=brewer.pal(8, "Dark2"))
    #wordcloud_rep(names(v), v, scale=c(4,0.5),
    #wordcloud_rep(names(v), v, scale=c(4,0.5),
    #min.freq = input$freq, max.words=input$max,
    #colors=brewer.pal(8, "Dark2"))
  })
}

#Shiny application leveraging Spotify API to obtain list of songs in an input users playlist 
#to generate a word cloud of the most frequent words found in their playlist
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
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Wordcloud"),
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        textInput("username1", "Username","lisashirai"),
        actionButton("update", "Change"),
        hr(),
        sliderInput("freq",
                    "Minimum Frequency:",
                    min = 1,  max = 50, value = 15),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("plot"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  getTermMatrix <- function(user)
  {
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
    docs <- tm_map(docs, removeWords, c("feat", "remix","live","radio","edit","cover")) 
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
      getTermMatrix(input$username1)
    })
  })
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), freq = v, min.freq = input$freq,
              max.words=input$max, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    #wordcloud_rep(names(v), v, scale=c(4,0.5),
    #min.freq = input$freq, max.words=input$max,
    #colors=brewer.pal(8, "Dark2"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


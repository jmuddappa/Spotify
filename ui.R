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
      #sliderInput("freq",
      #           "Minimum Frequency:",
      #          min = 1,  max = 50, value = 1),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 100,  value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot"))
  )
)

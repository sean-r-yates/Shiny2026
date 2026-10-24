library(shiny)

server <- function(input, output, session) {
  
  analysis <- eventReactive(input$analyse, {
    
    # Get the article from user
    text <- trimws(input$article)
    
    if (text == "") {
      return("Please enter an article.")
    }
    
    # Split the article into words
    words <- unlist(
      strsplit(tolower(text), "[^[:alnum:]']+")
    )
    
    words <- words[nzchar(words)]
    
    # Split into sentences
    sentences <- unlist(
      strsplit(text, "[.!?]+")
    )
    
    sentences <- sentences[nzchar(trimws(sentences))]
    
    # Calculate stuff
    word_count <- length(words)
    
    unique_words <- length(unique(words))
    
    sentence_count <- length(sentences)
    
    # Display results
    paste0(
      "Total words: ", word_count,
      "\nUnique words: ", unique_words,
      "\nSentences: ", sentence_count
    )
    
  })
  
  output$stats <- renderText({
    
    if (input$analyse == 0) {
      return("No article analysed yet.")
    }
    
    analysis()
    
  })
  
}

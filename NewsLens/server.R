
library(shiny)

server <- function(input, output, session) {
  
  # ---- MAIN ANALYSIS ----
  
  analysis <- eventReactive(input$analyse, {
    
    text <- trimws(input$article)
    
    validate(
      need(nzchar(text), "Please paste an article first.")
    )
    
    # Tokenise the text 
    words <- tibble::tibble(text = text) |>
      tidytext::unnest_tokens(word, text) |>
      dplyr::pull(word)
    
    # Count sentences
    sentences <- unlist(strsplit(text, "[.!?]+"))
    sentences <- sentences[nzchar(trimws(sentences))]
    
    # Calculate basic statistics
    stats <- list(
      word_count = length(words),
      unique_words = length(unique(words)),
      sentence_count = length(sentences)
    )
    
    # Pass the processed words to sentiment.R
    sentiment <- analyse_sentiment(words)
    
    # Store all results together
    return(list(
      words = words,
      stats = stats,
      sentiment = sentiment
    ))
    
  })
  
  
  # ---- STATISTICS OUTPUT ----
  
  output$stats <- renderText({
    
    if (input$analyse == 0) {
      return("No article analysed yet.")
    }
    
    result <- analysis()$stats
    
    paste0(
      "Total words: ", result$word_count,
      "\nUnique words: ", result$unique_words,
      "\nSentences: ", result$sentence_count
    )
    
  })
  
  
  # ---- SENTIMENT OUTPUT ----
  
  output$sentiment_summary <- renderText({
    
    if (input$analyse == 0) {
      return("No article analysed yet.")
    }
    
    result <- analysis()$sentiment
    
    paste0(
      "Positive words: ", result$positive,
      "\nNegative words: ", result$negative,
      "\nSentiment balance: ", result$balance,
      "\nInterpretation: ", result$label
    )
    
  })
  
  
  # ---- SENTIMENT CHART ----
  
  output$sentiment_plot <- renderPlot({
    
    req(input$analyse > 0)
    
    result <- analysis()$sentiment
    
    validate(
      need(
        result$positive + result$negative > 0,
        "No sentiment words detected."
      )
    )
    
    counts <- c(
      Positive = result$positive,
      Negative = result$negative
    )
    
    barplot(
      counts,
      main = "Sentiment Breakdown",
      xlab = "Sentiment",
      ylab = "Matched Words",
      col = c("steelblue", "tomato")
    )
    
  })
  
}

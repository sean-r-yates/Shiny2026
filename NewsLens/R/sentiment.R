
bing_dictionary <- tidytext::get_sentiments("bing")

analyse_sentiment <- function(words) {
  
  # words is already a vector of tokenised words
  
  if (!is.character(words)) {
    stop("Expected a character vector of words.")
  }
  
  # Count words first, then match with the dictionary
  matched_words <- tibble::tibble(word = words) |>
    dplyr::count(word, name = "n") |>
    dplyr::inner_join(bing_dictionary, by = "word") |>
    dplyr::arrange(dplyr::desc(n))
  
  # Calculate sentiment totals
  positive <- sum(
    matched_words$n[matched_words$sentiment == "positive"]
  )
  
  negative <- sum(
    matched_words$n[matched_words$sentiment == "negative"]
  )
  
  balance <- positive - negative
  
  # Interpret the results
  if (positive + negative == 0) {
    label <- "No sentiment words detected"
  } else if (balance > 0) {
    label <- "More positive words"
  } else if (balance < 0) {
    label <- "More negative words"
  } else {
    label <- "Equal positive and negative matches"
  }
  
  return(list(
    positive = positive,
    negative = negative,
    balance = balance,
    label = label,
    word_counts = matched_words
  ))
  
}

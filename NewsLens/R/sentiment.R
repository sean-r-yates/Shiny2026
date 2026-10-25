bing_dictionary <- tidytext::get_sentiments("bing")

analyse_sentiment <- function(word_counts) {
  stopifnot(is.data.frame(word_counts),
            all(c("word", "n") %in% names(word_counts)))

  matched_words <- dplyr::inner_join(
    word_counts, bing_dictionary, by = "word"
  )

  positive <- sum(matched_words$n[matched_words$sentiment == "positive"])
  negative <- sum(matched_words$n[matched_words$sentiment == "negative"])
  total <- positive + negative

  score <- if (total == 0) NA_real_ else (positive - negative) / total

  label <- if (is.na(score)) {
    "No sentiment matches"
  } else if (score < -0.15) {
    "More negative"
  } else if (score > 0.15) {
    "More positive"
  } else {
    "Balanced"
  }

  list(
    positive = positive,
    negative = negative,
    score = score,
    label = label,
    matched_words = matched_words[, c("word", "sentiment")]
  )
}

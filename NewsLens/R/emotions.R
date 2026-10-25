emotion_categories <- c(
  "anger", "anticipation", "disgust", "fear",
  "joy", "sadness", "surprise", "trust"
)

nrc_dictionary <- tidytext::get_sentiments("nrc") |>
  dplyr::filter(sentiment %in% emotion_categories)

analyse_emotions <- function(word_counts) {
  stopifnot(is.data.frame(word_counts),
            all(c("word", "n") %in% names(word_counts)))

  totals <- dplyr::inner_join(word_counts, nrc_dictionary, by = "word") |>
    dplyr::group_by(sentiment) |>
    dplyr::summarise(count = sum(n), .groups = "drop")

  counts <- tibble::tibble(emotion = emotion_categories) |>
    dplyr::left_join(totals, by = c("emotion" = "sentiment")) |>
    dplyr::mutate(count = dplyr::coalesce(count, 0L))

  list(counts = counts)
}

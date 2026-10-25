library(shiny)
library(highcharter)

# ---- Article highlighting ----
highlight_article <- function(text, matched_words) {
  # Highlight only words the Bing dictionary actually matched.
  positions <- gregexpr("[[:alpha:]]+(?:['’][[:alpha:]]+)*", text, perl = TRUE)[[1]]

  if (positions[1] == -1) {
    return(htmltools::HTML(htmltools::htmlEscape(text)))
  }

  lengths <- attr(positions, "match.length")
  sentiment_by_word <- setNames(matched_words$sentiment, matched_words$word)
  pieces <- character()
  next_character <- 1L

  for (i in seq_along(positions)) {
    first <- positions[i]
    last <- first + lengths[i] - 1L

    if (first > next_character) {
      pieces <- c(pieces, htmltools::htmlEscape(
        substr(text, next_character, first - 1L)
      ))
    }

    word <- substr(text, first, last)
    escaped_word <- htmltools::htmlEscape(word)
    sentiment <- unname(sentiment_by_word[tolower(word)])

    if (!is.na(sentiment) && sentiment == "positive") {
      pieces <- c(pieces, paste0(
        '<mark class="bg-success bg-opacity-25">', escaped_word, '</mark>'
      ))
    } else if (!is.na(sentiment) && sentiment == "negative") {
      pieces <- c(pieces, paste0(
        '<mark class="bg-danger bg-opacity-25">', escaped_word, '</mark>'
      ))
    } else {
      pieces <- c(pieces, escaped_word)
    }

    next_character <- last + 1L
  }

  if (next_character <= nchar(text)) {
    pieces <- c(pieces, htmltools::htmlEscape(
      substr(text, next_character, nchar(text))
    ))
  }

  htmltools::HTML(paste0(pieces, collapse = ""))
}

server <- function(input, output, session) {

  # ---- Initial Analysis ----
  analysis <- reactiveVal(NULL)

  observeEvent(input$analyse, {
    text <- input$article

    if (is.null(text) || !nzchar(trimws(text))) {
      showNotification("Paste an article first.", type = "warning")
      return()
    }

    words <- tibble::tibble(text = text) |>
      tidytext::unnest_tokens(word, text) |>
      dplyr::pull(word)

    word_counts <- tibble::tibble(word = words) |>
      dplyr::count(word, name = "n")

    sentences <- strsplit(text, "[.!?]+")[[1]]
    sentiment <- analyse_sentiment(word_counts)

    analysis(list(
      stats = c(
        words = length(words),
        unique = nrow(word_counts),
        sentences = sum(nzchar(trimws(sentences)))
      ),
      sentiment = sentiment,
      emotions = analyse_emotions(word_counts)$counts,
      highlighted_text = highlight_article(text, sentiment$matched_words)
    ))
  })

  observeEvent(input$new_article, {
    analysis(NULL)
  })

  # ---- Article input ----
  output$article_button <- renderUI({
    if (is.null(analysis())) {
      actionButton("analyse", "Analyse article", class = "btn-primary")
    } else {
      actionButton("new_article", "New article", class = "btn-outline-primary")
    }
  })

  output$article_view <- renderUI({
    result <- analysis()

    if (is.null(result)) {
      return(textAreaInput(
        "article", NULL, height = "455px", width = "100%",
        placeholder = "Paste an article...", resize = "none"
      ))
    }

    div(
      class = "form-control overflow-auto",
      style = "height: 455px; white-space: pre-wrap; overflow-wrap: break-word;",
      result$highlighted_text
    )
  })

  # ---- Statistics ----
  output$word_count <- renderText({
    req(analysis())
    analysis()$stats["words"]
  })

  output$unique_words <- renderText({
    req(analysis())
    analysis()$stats["unique"]
  })

  output$sentence_count <- renderText({
    req(analysis())
    analysis()$stats["sentences"]
  })

  # ---- Sentiment Graph ----
  #gauge
  output$sentiment_gauge <- renderHighchart({
    req(analysis())
    score <- analysis()$sentiment$score
    gauge_value <- if (is.na(score)) list() else list(round(score * 100))

    highchart() |>
      hc_chart(type = "gauge", backgroundColor = "transparent",
               spacing = c(0, 0, 0, 0)) |>
      hc_title(text = NULL) |>
      hc_pane(
        startAngle = -135, endAngle = 135,
        center = c("50%", "53%"), size = "88%",
        background = list(backgroundColor = "#e9ecef",
                          innerRadius = "85%", outerRadius = "94%",
                          shape = "arc", borderWidth = 0)
      ) |>
      hc_yAxis(
        min = -100, max = 100,
        lineWidth = 0, tickLength = 0, tickWidth = 0,
        minorTicks = FALSE, minorTickLength = 0, minorTickWidth = 0,
        tickPositions = list(), gridLineWidth = 0,
        labels = list(enabled = FALSE),
        title = list(text = NULL),
        plotBands = list(
          list(from = -100, to = -15, color = "#d65c57",
               innerRadius = "85%", outerRadius = "94%"),
          list(from = -15, to = 15, color = "#98a1ab",
               innerRadius = "85%", outerRadius = "94%"),
          list(from = 15, to = 100, color = "#18BC9C",
               innerRadius = "85%", outerRadius = "94%")
        )
      ) |>
      hc_plotOptions(
        gauge = list(
          dataLabels = list(enabled = FALSE),
          dial = list(radius = "90%", baseWidth = 5, topWidth = 1,
                      rearLength = "0%", backgroundColor = "#334155"),
          pivot = list(radius = 6, backgroundColor = "#334155")
        )
      ) |>
      hc_add_series(type = "gauge", name = "Sentiment", data = gauge_value) |>
      hc_tooltip(enabled = FALSE) |>
      hc_credits(enabled = FALSE)
  })

  output$sentiment_value <- renderUI({
    req(analysis())
    result <- analysis()$sentiment
    score <- result$score

    colour <- if (is.na(score) || abs(score) <= 0.15) {
      "text-secondary"
    } else if (score < 0) {
      "text-danger"
    } else {
      "text-success"
    }

    value <- if (is.na(score)) "—" else sprintf("%+.0f", score * 100)

    tagList(
      div(class = paste("fs-4 fw-bold", colour), value),
      div(class = "small text-secondary", result$label),
      div(class = "small text-secondary", "Sentiment balance (−100 to +100)")
    )
  })

  # lil line on the bottom
  output$sentiment_scale <- renderUI({
    req(analysis())
    result <- analysis()$sentiment

    if (is.na(result$score)) {
      return(p(class = "text-center text-secondary small",
               "No sentiment words detected."))
    }

    position <- max(2, min(98, 50 * (result$score + 1)))

    tagList(
      div(
        class = "position-relative",
        div(
          class = "progress", style = "height: 12px;",
          div(class = "progress-bar bg-danger", style = "width: 42.5%;"),
          div(class = "progress-bar bg-secondary", style = "width: 15%;"),
          div(class = "progress-bar bg-success", style = "width: 42.5%;")
        ),
        span(class = "position-absolute top-50 translate-middle rounded-circle bg-white border border-dark",
             style = sprintf("left: %.1f%%; width: 18px; height: 18px;", position))
      ),
      div(class = "d-flex justify-content-between text-secondary small mt-2",
          span("Negative"), span("Neutral"), span("Positive")),
      p(class = "text-center text-secondary small mt-2 mb-0",
        sprintf("%s positive / %s negative word matches",
                result$positive, result$negative))
    )
  })

  # ---- Emotional analysis ----
  emotion_rows <- function(counts, categories, colour) {
    maximum <- max(1, counts$count)

    tagList(lapply(categories, function(emotion) {
      value <- counts$count[match(emotion, counts$emotion)]

      div(
        class = "mb-3",
        div(class = "d-flex justify-content-between mb-1",
            span(tools::toTitleCase(emotion)), strong(value)),
        div(class = "progress", style = "height: 12px;",
            div(class = paste("progress-bar", colour),
                style = sprintf("width: %.1f%%;", 100 * value / maximum),
                role = "progressbar",
                `aria-valuenow` = value,
                `aria-valuemin` = 0,
                `aria-valuemax` = maximum))
      )
    }))
  }

  output$negative_emotions <- renderUI({
    req(analysis())
    emotion_rows(analysis()$emotions,
                 c("fear", "anger", "sadness", "disgust"), "bg-danger")
  })

  output$positive_emotions <- renderUI({
    req(analysis())
    emotion_rows(analysis()$emotions,
                 c("trust", "joy", "anticipation", "surprise"), "bg-success")
  })
}

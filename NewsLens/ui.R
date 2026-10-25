library(shiny)
library(bslib)

ui <- page_fluid(
  title = "NewsLens",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  h2("NewsLens"),
  p("Explore how news articles use language."),

  layout_columns(
    col_widths = c(5, 7),

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Article input"),
        uiOutput("article_button", inline = TRUE)
      ),
      card_body(
        div(style = "height: 470px; overflow: hidden;",
            uiOutput("article_view"))
      )
    ),

    div(
      card(
        card_header("Article statistics"),
        card_body(
          div(class = "d-flex justify-content-between mb-2",
              "Words:", textOutput("word_count", inline = TRUE)),
          div(class = "d-flex justify-content-between mb-2",
              "Unique words:", textOutput("unique_words", inline = TRUE)),
          div(class = "d-flex justify-content-between",
              "Sentences:", textOutput("sentence_count", inline = TRUE))
        )
      ),

      card(
        class = "mt-3",
        card_header("Language analysis"),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),

            div(
              h6(class = "text-danger fw-bold", ":( Negative emotions"),
              uiOutput("negative_emotions")
            ),
            div(
              highcharter::highchartOutput("sentiment_gauge", height = "240px"),
              div(class = "text-center", uiOutput("sentiment_value"))
            ),
            div(
              h6(class = "text-success fw-bold", ":) Positive emotions"),
              uiOutput("positive_emotions")
            )
          ),
          div(class = "mt-3", uiOutput("sentiment_scale"))
        )
      )
    )
  )
)

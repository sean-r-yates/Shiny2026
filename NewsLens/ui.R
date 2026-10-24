
library(shiny)

ui <- fluidPage(
  
  titlePanel("NewsLens"),
  
  textAreaInput(
    inputId = "article",
    label = "Paste a news article:",
    rows = 10,
    width = "100%"
  ),
  
  actionButton(
    inputId = "analyse",
    label = "Analyse Article"
  ),
  
  hr(),
  
  h3("Article Statistics"),
  
  verbatimTextOutput("stats")
  
)

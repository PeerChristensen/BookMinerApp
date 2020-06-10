
library(shiny)
library(epubr)
library(tidyverse)

# File
file <- "DaVinciCode.epub"
df <- epub(file)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    
           titlePanel(h1("ePub-A-Lula!",align="center")),
    fluidRow(
        column(3,
               actionButton("upload", "Upload ePub"),HTML('&nbsp;'),"This app will..")),
    splitLayout(h4("Author",align="center"),
                h4("Title",align="center"),
                h4("Genre",align="center"),
                h4("Publisher",align="center"),
                h4("ISBN",align="center"),
                h4("Date",align="center")),
    splitLayout(
        h5(textOutput("author"),align="center"),
        h5(textOutput("title"),align="center"),
        h5(textOutput("genre"),align="center"),
        h5(textOutput("publisher"),align = "center"),
        h5(textOutput("isbn"),align = "center"),
        h5(textOutput("date"),align = "center")),
    hr(),
    splitLayout(
        h3("Readability",align="left"),
        h3("Lexical Diversity",align="left")
        ),
    hr(),
    fluidRow(
        column(4, h3("Keywords",align="center")),
        column(8, h3("Plot sentiments",align="center"))
    ),
    fluidRow(
        column(4, h3("Keywords",align="center")),
        column(8, h3("Plot sentiments",align="center"))
    )
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$author <- renderText(df$creator)
    output$title <- renderText(df$title)
    output$genre <- renderText(df$subject)
    output$publisher <- renderText(df$publisher)
    output$isbn <- renderText(df$identifier)
    output$isbn <- renderText(df$identifier)
    output$date <- renderText(as.character(as.Date(df$date)))
}

# Run the application 
shinyApp(ui = ui, server = server)

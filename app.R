
library(shiny)
library(epubr)
library(tidyverse)
library(shinythemes)
library(textrank)
library(udpipe)
library(sentimentr)
library(data.table)
library(zoo)

# Files
file <- "DaVinciCode.epub"
df <- epub(file)
anno <- read_csv("anno.csv")

#sentiments
df_row <- tibble(text = paste(df$data[[1]]$text,collapse = ","))
sentiments <- sentiment(get_sentences(df_row$text))

sentiments$part <- cut(sentiments$sentence_id, breaks = 1000,labels=1:1000)

sentiments <- sentiments %>%
    group_by(part) %>%
    summarise(m = mean(sentiment)) %>%
    mutate(rollmean = rollmean(m, k = 50, fill = 0, align = "right"))

# Keywords
stats <- textrank_keywords(anno$lemma, 
                           relevant = anno$pos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")

stats <- subset(stats$keywords, ngram > 1 & freq >= 5)

top_tr <- stats %>%
    top_n(5,freq)

stats2 <- keywords_rake(x = anno, 
                        term = "token", group = c("sentence_id"),
                        relevant = anno$pos %in% c("NOUN", "ADJ"),
                        ngram_max = 4)

top_rake <-stats2 %>%
    filter(freq >=5) %>%
    top_n(5,rake) %>% 
    select(-rake)

keywords <- rbind(top_tr,top_rake) %>%
    distinct(keyword, .keep_all = T) %>%
    arrange(desc(freq)) %>%
    mutate(order = row_number())

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    
           titlePanel(h1("ePub-A-Lula!",align="center")),
    fluidRow(column(12,actionButton("upload", "Upload ePub"),align="center",style='padding:20px;')),
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
        column(5, h3("Keywords",align="center")),
        column(7, h3("Plot sentiments",align="center"))
    ),
    fluidRow(
        column(5, plotOutput("keywords",width = "90%"),align="center"),
        column(7, plotOutput("sentiment",width = "90%"),align="center")
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
    output$keywords <- renderPlot({
        keywords %>% 
            ggplot(aes(order,rev(freq))) +
            geom_col() +
            coord_flip() +
            scale_x_continuous(breaks = keywords$order,
                               labels = keywords$keyword) 
    })
    output$sentiment <- renderPlot({
        sentiments %>%
            ggplot(aes(as.numeric(part),rollmean)) +
            geom_smooth(se=F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

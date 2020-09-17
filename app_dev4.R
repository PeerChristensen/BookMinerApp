options(shiny.maxRequestSize = 50*1024^2)

# Shiny
library(shiny)
library(shinythemes)
library(shinybusy)
library(flexdashboard)
# Text
library(textrank)
library(spacyr)
library(tidytext)
library(sentimentr)
library(udpipe)
library(quanteda)
#library(syuzhet)
# Data
library(igraph)
library(epubr)
library(data.table)
library(tidyverse)
library(networkD3)
library(widyr)
library(pdftools)
library(h2o)
library(FNN)

css <- HTML("
.html-widget.gauge svg {
  height: 400px;
  width: 800px;
}")

red <- "#C41A24"
#use_python("/Users/peerchristensen/.pyenv/versions/3.7.3")
spacy_initialize(model="en_core_web_lg")
h2o.init()

titles <- read_csv("/Users/peerchristensen/Desktop/Projects/DocSimilarity/titles.csv")

#df <- epub("DaVinciCode.epub")
#df <- df$data[[1]]

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("slate"),
                tags$head(tags$style(css)),
                add_busy_spinner(spin = "fading-circle",margin=c(40,40),color="snow"),
                # ----------------------------------------------------------------------
                # Title
                titlePanel(h1("Book Miner v. 1",align="center",
                              style='color: snow;
                         font-family: Lato;
                         font-size: 70px;
                         font-weight: bold;
                         background-color:#C41A24;
                         padding-bottom: 40px;
                         padding-top: 40px')),
                # ----------------------------------------------------------------------
                # File upload
                fluidRow(
                  column(12,
                         fileInput("file", h2("Upload text"),accept = c(".epub",".pdf"),
                                   placeholder="epub or pdf",width="25%"),align="center", 
                         style='padding:20px;
                                color: snow;
                                font-size: 18px;
                                font-weight: bold;
                                font-family: Lato;')),
                # ----------------------------------------------------------------------
                # Meta data 
                splitLayout(
                  h2("Author",align="center",
                     style='color: snow;font-family: Lato;'),
                  h2("Title",align="center",
                     style='color: snow;font-family: Lato;'),
                  h2("Genre",align="center",
                     style='color: snow; font-family: Lato;'),
                  h2("Publisher",align="center",
                     style='color: snow; font-family: Lato;'),
                  h2("ISBN",align="center",
                     style='color: snow;font-family: Lato;'),
                  h2("Date",align="center",
                     style='color: snow;font-family: Lato;')),
                splitLayout(
                  h3(textOutput("author"),align="center",
                     style='color: snow;font-family: Lato;'),
                  h3(textOutput("title"),align="center",
                     style='color: snow;font-family: Lato;'),
                  h3(textOutput("genre"),align="center",
                     style='color: snow;font-family: Lato;'),
                  h3(textOutput("publisher"),align = "center",
                     style='color: snow;font-family: Lato;'),
                  h3(textOutput("isbn"),align = "center",
                     style='color: snow;font-family: Lato;'),
                  h3(textOutput("date"),align = "center",
                     style='color: snow;font-family: Lato;')),
                hr(),
                # ----------------------------------------------------------------------
                # Similar books and best-seller score
                fluidRow(
                  column(6,
                         h2("Similar books",align="center",
                            style='color: snow; font-weight: bold; font-family: Lato;')),
                  column(6,
                         h2("Best-seller score",align="center",
                            style='color: snow; font-weight: bold;font-family: Lato;'))
                ),
                fluidRow(
                  column(6,
                         tableOutput("similarity"),align="center",
                         style = "font-size:200%"),
                  column(6,
                         gaugeOutput("gauge",width="100%"),align="center")
                  
                ),
                hr(),
                # ----------------------------------------------------------------------
                # Readability
                fluidRow(
                  column(12,
                         h2("Readability",align="center",
                            style='color: snow; font-weight: bold;font-family: Lato;')
                  )
                ),
                fluidRow(
                  column(2,
                         h3("Difficult"),align="right",
                         style='color: snow; font-weight: bold;font-family: Lato;'),
                  column(8,
                         plotOutput("readability",width = "100%",height = "170px"),align="center"),
                  column(2,
                         h3("Easy"),align="left",
                         style='color: snow; font-weight: bold;font-family: Lato;')
                ),
                hr(),
                # ----------------------------------------------------------------------
                # Plot
                fluidRow(
                  column(6,
                         h2("Mood",align="center",
                            style='color: snow; font-weight: bold; font-family: Lato;')),
                  column(6,
                         h2("Tempo",align="center",
                            style='color: snow; font-weight: bold;font-family: Lato;'))
                ),
                fluidRow(
                  column(6, plotOutput("sentiment",width = "90%"),align="center"),
                  column(6, plotOutput("tempo",width="90%"),align="center")
                ),
                hr(),
                # ----------------------------------------------------------------------
                # Named entities
                fluidRow(
                  column(12, 
                         h2("Named entities"),align="center",
                         style='color: snow; font-weight: bold;font-family: Lato;')),
                fluidRow(
                  column(12,offset=2,align="center",
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Frequency",
                                      plotOutput("ner",width = "100%",height = "1000px")),
                             tabPanel("Co-occurence Network",
                                      forceNetworkOutput("cooc",width = "100%",height = "1200px")),
                             tabPanel("Keywords",
                                      plotOutput("keywords",width = "100%",height = "700px"))
                           ),style='color: snow;font-family: Lato;
                         font-size: 25px')
                  )
                ),
                hr()
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------

server <- function(input, output) {
  
  # ----------------------------------------------------------------------
  # input data
  observe({
    
    file = input$file
    
    if (is.null(file)) {
      return(NULL)
    }
    
    # ----------------------------------------------------------------------
    # if epub
    if (tools::file_ext(file$datapath) == "epub") {
      
      df <- epub(file$datapath)
      #df <- df$data[[1]]
      
      # Meta data
      meta  <- df
      output$author    <- renderText(meta$creator)
      output$title     <- renderText(meta$title)
      output$genre     <- renderText(meta$subject)
      output$publisher <- renderText(meta$publisher)
      output$isbn      <- renderText(meta$identifier)
      output$date      <- renderText(as.character(as.Date(meta$date)))
      
      df <- df$data[[1]]
      
    } 
    # ----------------------------------------------------------------------
    # if pdf
    else if (tools::file_ext(file$datapath) == "pdf"){
      df <- pdf_text(file$datapath)
      df <- tibble(text=df)
      meta = NULL
    }
    
    # ----------------------------------------------------------------------
    # one row
    df_row <- tibble(text = paste(df,collapse = ","))
    
    # ----------------------------------------------------------------------
    # annotated
    anno <- spacy_parse(df$text)
    
    # tokens
    tokens <- anno %>%
      filter(pos !="PUNCT") %>%
      select(token) %>%
      summarise(text = paste(token,collapse = " ")) %>%
      as.h2o() %>%
      h2o.tokenize(split= " ")
    
    # word2vec
    w2v_model <- h2o.loadModel(list.files("/Users/peerchristensen/Desktop/Projects/DocSimilarity/models",pattern="Word2Vec",full.names = T))
    vecs <- h2o.importFile("/Users/peerchristensen/Desktop/Projects/DocSimilarity/vectors.csv")
    vecs_new <- h2o.transform_word2vec(w2v_model, tokens, aggregate_method = "AVERAGE")
    
    ind <- knnx.index(vecs, vecs_new, k=6) %>% as.vector()
    dist <- knnx.dist(vecs, vecs_new, k=6) %>% as.vector()
    
    # ----------------------------------------------------------------------
    # Similar books
    
    output$similarity <- renderTable({
      
    titles[ind,] %>%
        mutate(dist = dist) %>%
        filter(dist > .001 & Title != meta$title) %>%
        head(5) %>%
        select(-dist)
    },spacing ="l",hover = T,striped = T,width="80%")
    
    # ----------------------------------------------------------------------
    # best-seller
    
    output$gauge = renderGauge({
      gauge(95,#input$value, 
            min = 0, 
            max = 100, 
            symbol = '%',
            sectors = gaugeSectors(success = c(70, 100), 
                                   warning = c(30, 50),
                                   danger = c(0, 30)))
    })
    
    # ----------------------------------------------------------------------
    # keywords
    
    stats <- textrank_keywords(anno$token,
                               relevant = anno$pos %in% c("NOUN", "ADJ"),
                               ngram_max = 5, sep = " ")
    stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
    top_tr <- stats %>%
      top_n(5,freq) %>%
      select(-ngram)
    
    # RAKE
    stats2 <- keywords_rake(x = anno,
                            term = "lemma", group = c("doc_id"),
                            relevant = anno$pos %in% c("NOUN", "ADJ"),
                            ngram_max = 8)
    top_rake <-stats2 %>%
      filter(freq >=5) %>%
      top_n(5,rake) %>%
      select(-rake,-ngram) %>%
      top_n(3,freq)
    
    # NPs
    np <- spacy_extract_nounphrases(df$text)
    
    top_np <- np %>%
      filter(length>2) %>%
      group_by(text) %>%
      count() %>%
      arrange(desc(n)) %>%
      ungroup() %>%
      top_n(5,n) %>%
      rename(keyword = text, freq = n) %>%
      mutate(keyword = str_trim(str_replace(keyword,"the ","")))
    
    keywords <- rbind(top_tr,top_rake,top_np) %>%
      distinct(keyword, .keep_all = T) %>%
      arrange(desc(freq)) %>%
      filter(freq >= 10) %>%
      mutate(order = rev(row_number()))
    
    output$keywords  <- renderPlot({
      keywords %>%
        ggplot(aes(order,freq)) +
        geom_col(fill=red,width=.7) +
        coord_flip() +
        scale_x_continuous(breaks = keywords$order,
                           labels = keywords$keyword) +
        theme(axis.text = element_text(family = "Lato",
                                       colour = "snow",size = 16),
              axis.title = element_blank(),
              plot.background = element_rect(fill="#272B30",
                                             color = "#272B30", size = 0),
              panel.background = element_rect(fill="#272B30",
                                              color = "#272B30", size = 0),
              panel.grid = element_blank())
    })
    
    # ----------------------------------------------------------------------
    # Sentiments
    
    sentiments      <- sentiment(get_sentences(df_row$text))
    sentiments$part <- cut(sentiments$sentence_id, breaks = 1000,labels=1:1000)
    
    sentiments <- sentiments %>%
      group_by(part) %>%
      summarise(m = mean(sentiment)) %>%
      mutate(rollmean = frollmean(m, n = 50, fill = 0, align = "right"))
    
    output$sentiment <- renderPlot({
      sentiments %>%
        ggplot(aes(as.numeric(part),rollmean)) +
        geom_col(colour="grey50",alpha=.01,width=.2) +
        geom_smooth(se=F,colour=red,size=2.5,method="gam") +
        theme_void() +
        theme(plot.background  = element_rect(fill="#272B30"),
              panel.background = element_rect(fill="#272B30",
                                              color = "#272B30", size = 0),
              panel.grid = element_blank())
    })
    
    # ----------------------------------------------------------------------
    # Tempo
    
    sentences <- df %>%
      unnest_tokens(text,text,"sentences") %>%
      mutate(sentence_id = row_number())
    
    sentences$part <- as.numeric(cut(sentences$sentence_id, breaks = 1000,labels=1:1000))
    sentences$length <- sentences$text %>% str_count("\\W+")
    
    output$tempo <- renderPlot({
      
      sentences %>%
        group_by(part) %>%
        summarise(m = median(length)) %>%
        mutate(m=scale(m)) %>%
        mutate(rollmean = zoo::rollmean(m, k = 50, fill = 0, align = "right")) %>%
        #mutate(rollmean = rollmean) %>%
        ggplot(aes(part,-rollmean)) +
        geom_col(colour="grey50",alpha=.01,width=.2) +
        geom_smooth(se=F,colour="forestgreen",size=2.5,method="gam") +
        theme_void() +
        theme(plot.background = element_rect(fill="#272B30"),
              panel.background = element_rect(fill="#272B30"),
              panel.grid = element_blank())
    })
    
    # ----------------------------------------------------------------------
    # readability
    
    dfCorpus <- quanteda::corpus(df_row,  text_field = "text")
    readability <- quanteda::textstat_readability(dfCorpus,
                                                  measure = c("Flesch")) %>%
      as_tibble() %>%
      select(Flesch) %>%
      mutate(row="a")
    output$readability <- renderPlot( {
      readability %>%
        ggplot(aes(row,max)) +
        geom_col(width=.4, fill = "#1a9850",aes(row,100),colour="lightgrey") +
        geom_col(width=.4, fill = "#a6d96a",aes(row,90),colour="lightgrey") +
        geom_col(width=.4, fill = "#d9ef8b",aes(row,80),colour="lightgrey") +
        geom_col(width=.4, fill = "#fee08b",aes(row,70),colour="lightgrey") +
        geom_col(width=.4, fill = "#fdae61",aes(row,60),colour="lightgrey") +
        geom_col(width=.4, fill = "#f46d43",aes(row,50),colour="lightgrey") +
        geom_col(width=.4, fill = "#d73027",aes(row,30),colour="lightgrey") +
        geom_hline(yintercept = readability$Flesch, size = 9,colour="lightgrey") +
        geom_hline(yintercept = readability$Flesch, size = 7,colour="snow") +
        coord_flip() +
        theme_void() +
        theme(plot.margin = margin(2, 0, 2, 0, "cm"),
              plot.background = element_rect(fill="#272B30",
                                             color = "#272B30", size = 0),
              panel.background = element_rect(fill="#272B30",
                                              color = "#272B30", size = 0))
    })
    
    # ----------------------------------------------------------------------
    # NER
    
    ents_full <- entity_extract(anno) %>%
      filter(entity_type %in% c("GPE","FAC","NORP","PERSON")) %>%
      mutate(entity = str_replace(entity,"the_","")) %>%
      mutate(entity = str_replace_all(entity,"_"," "))
    
    ents <- ents_full %>%
      group_by(entity_type) %>%
      count(entity) %>%
      group_by(entity) %>%
      arrange(desc(n)) %>%
      top_n(1,n) %>%
      group_by(entity_type) %>%
      count(entity) %>%
      top_n(8,n) %>%
      ungroup()
    
    ents_plot <- ents %>%
      arrange(entity_type, -n) %>%
      filter(n>=2) %>%
      mutate(order = rev(row_number()),
             colour = case_when(entity_type == "FAC"    ~ "goldenrod",
                                entity_type == "GPE"    ~ "forestgreen",
                                entity_type == "NORP"   ~ "blue3",
                                entity_type == "PERSON" ~ "#C41A24")) 
    
    pairs <- ents_full %>%
      widyr::pairwise_count(entity, doc_id, sort = TRUE) %>%
      filter(item1 %in% ents$entity,
             item2 %in% ents$entity)
    
    network <- pairs %>%
      #top_frac(.1) %>%
      filter(n>1) %>%
      mutate(row = row_number()) %>%
      filter(row <= 100) %>%
      select(-row) %>%
      # top_n(200,n) %>%
      graph_from_data_frame() %>%
      igraph_to_networkD3()
    
    network$nodes <- network$nodes %>%
      left_join(ents, by = c("name" = "entity")) 
    
    # ----------------------------------------------------------------------
    # Bar plots
    
    facet_labs <- c("NORP" = "Communities", "PERSON" = "Persons","FAC" = "Place names",
                    "GPE" = "Geo-political locations")
    output$ner <- renderPlot({
      ents_plot %>%
        ggplot(aes(order,n)) +
        geom_col(width=.7,fill=ents_plot$colour) +
        scale_x_continuous(
          breaks = ents_plot$order,
          labels = ents_plot$entity,
          expand = c(0,0)) +
        facet_wrap(~entity_type,scales="free",
                   labeller = labeller(entity_type = facet_labs)) +
        coord_flip() +
        theme_void() +
        theme(axis.text.y =element_text(size=16,family="Lato",colour = "snow"),
              strip.text = element_text(size=20,family="Lato",vjust=7),
              panel.spacing = unit(2, "cm"),
              strip.text.x = element_text(margin = margin(t = 30),colour="snow"),
              axis.text.x = element_blank(),
              plot.margin = margin(1, 0, 0, 0, "cm"),
              plot.background = element_rect(fill="#272B30",
                                             color = "#272B30", size = 0),
              panel.background = element_rect(fill="#272B30",
                                              color = "#272B30", size = 0))
    })
    
    # ----------------------------------------------------------------------
    # Cooccurrence plot
    
    output$cooc <- renderForceNetwork({
      
      my_color <- 'd3.scaleOrdinal() .domain(["PERSON", "NORP","GPE","FAC"]) .range(["#C41A24", "blue" , "green","yellow"])'
      
      
      forceNetwork(Links = network$links, Nodes = network$nodes,
                   Source = 'source', Target = 'target',
                   NodeID = 'name', Group = 'entity_type',
                   Value='value', Nodesize = 'n',fontSize=25,
                   colourScale = my_color, zoom = T,opacity = 0.9,
                   fontFamily = "Lato", 
                   linkColour = "snow",
                   linkDistance = networkD3::JS("function(d) { return 15*d.value; }"))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


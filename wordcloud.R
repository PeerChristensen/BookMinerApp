# wordcloud

librarry(quanteda)

# some code
set.seed(10)
dfmat1 <- dfm(corpus_subset(data_corpus_inaugural, President == "Obama"),
              remove = stopwords("english"), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 3)

col <- sapply(seq(0.1, 1, 0.1), function(x) adjustcolor("#1F78B4", x))
textplot_wordcloud(dfmat1, adjust = 0.5, random_order = FALSE,
                   color = col, rotation = FALSE)

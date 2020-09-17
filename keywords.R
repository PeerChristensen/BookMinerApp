# key words

library(tidyverse)
library(spacyr)
library(textrank)
#library(tidytext)
#library(udpipe)
library(epubr)

df <- epub("circle.epub")
df <- df$data[[1]]

spacy_initialize(model="en_core_web_lg")
# tokens <-spacy_tokenize(
#   df$text,
#   what = c("sentence"),
#   remove_punct = TRUE,
#   remove_url = TRUE,
#   remove_numbers = TRUE,
#   remove_separators = TRUE,
#   remove_symbols = TRUE,output="data.frame") %>%
#   as_tibble() %>%
#   mutate(sentence_id = row_number())
# anno <- anno %>%
#   filter(pos != "PRON")
#anno3 <- spacy_tokenize(df$text,what="sentence",remove_punct = T,output="data.frame")
#anno <- spacy_parse(tokens$token)
anno <- spacy_parse(df$text)

# Textrank
stats <- textrank::textrank_keywords(anno$token, 
                           relevant = anno$pos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")

stats <- subset(stats$keywords, ngram > 1 & freq >= 5)

top_tr <- stats %>%
  #filter(!str_detect(keyword,c("-PRON-"))) %>%
  top_n(5,freq) %>%
  mutate(keyword = str_replace_all(keyword," .",""))
top_tr

# RAKE
stats2 <- udpipe::keywords_rake(x = anno, 
                       term = "lemma", group = c("sentence_id"),
                       relevant = anno$pos %in% c("NOUN","ADJ"),
                       ngram_max = 8)


top_rake <-stats2 %>%
  filter(freq >=5,ngram>1,#!str_detect(keyword,c("-PRON-"))
     ) %>%
  top_n(5,rake) %>% 
  select(-rake)
top_rake

keywords <- rbind(top_tr,top_rake) %>%
  distinct(keyword, .keep_all = T) %>%
  arrange(desc(freq)) %>%
  mutate(order = row_number())

keywords %>% 
  ggplot(aes(order,rev(freq))) +
  geom_col() +
  coord_flip() +
  scale_x_continuous(breaks = keywords$order,
                     labels = keywords$keyword) 

# -----------------------------------------------------
# working but slow
# 
# library(udpipe)
# library(textrank)
# ud_model <- udpipe_download_model(language = "english")
# ud_model <- udpipe_load_model(ud_model$file_model)
# x <- udpipe_annotate(ud_model, x = df$sentences)
# x <- as.data.frame(x)
# 
# # Textrank
# stats <- textrank_keywords(x$token, 
#                            relevant = x$upos %in% c("NOUN", "ADJ"), 
#                            ngram_max = 4, sep = " ")
# 
# stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
# 
# top_tr <- stats %>%
#   top_n(5,freq)
# top_tr
# # RAKE
# stats2 <- keywords_rake(x = x, 
#                         term = "lemma", group = c("sentence_id"),
#                         relevant = x$upos %in% c("NOUN", "ADJ"),
#                         ngram_max = 4)
# 
# top_rake <-stats2 %>%
#   filter(freq >=5) %>%
#   top_n(5,rake) %>% 
#   select(-rake)

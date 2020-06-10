# key words

library(tidyverse)
library(spacyr)
library(textrank)
library(tidytext)
library(udpipe)

df <- read_csv("DaVinciCode.csv")

df <- df %>%
  unnest_tokens(output = sentences, input = text,token = "sentences",to_lower = F,
  ) %>%
  mutate(sentences = tm::removePunctuation(sentences))

anno <- spacy_parse(df$sentences)

# Textrank

stats <- textrank_keywords(anno$lemma, 
                           relevant = anno$pos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")

stats <- subset(stats$keywords, ngram > 1 & freq >= 5)

top_tr <- stats %>%
  top_n(5,freq)

# RAKE
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

keywords %>% 
  ggplot(aes(order,rev(freq))) +
  geom_col() +
  coord_flip() +
  scale_x_continuous(breaks = keywords$order,
                     labels = keywords$keyword) 
  
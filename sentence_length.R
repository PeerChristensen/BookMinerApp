#sentence length

library(tidyverse)
library(quanteda)
#library(sentida)
library(sentimentr)
library(zoo)
library(tidytext)
library(spacyr)

df <- epub("kvinden_i_buret.epub")
df <- df$data[[1]]

#spacy_initialize("en_core_web_lg")
# sent <- spacy_tokenize(
#   df$text,
#   what = c("sentence"),
#   remove_punct = TRUE,
#   remove_url = TRUE,
#   remove_numbers = TRUE,
#   remove_separators = TRUE,
#   remove_symbols = TRUE,output="data.frame") %>%
#   as_tibble() %>%
#   mutate(sentence_id = row_number())

sent <- syuzhet::get_sentences(df$text) %>% 
  as_tibble() %>%
  rename(text = value) %>%
  mutate(sentence_id = row_number())

sent$part <- as.numeric(cut(sent$sentence_id, breaks = 1000,labels=1:1000))
sent$length <- sent$text %>% str_count("\\W+")

df <- sent %>%
  group_by(part) %>%
  summarise(m = median(length)) %>%
  mutate(m=scale(m)) %>%
  mutate(rollmean = rollmean(m, k = 50, fill = 0, align = "right")) %>%
  mutate(rollmean = rollmean)

df %>%
  ggplot(aes(part,rollmean)) +
  geom_col() +
  geom_smooth(se=F,colour="forestgreen") +
  theme_void() +
  theme(plot.background = element_rect(fill="#272B30"),
        panel.background = element_rect(fill="#272B30"),
        panel.grid = element_blank())

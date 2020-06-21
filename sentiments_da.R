# sentiments danish

#Sys.setlocale(category = "LC_ALL", locale = "UTF-8") 

library(tidyverse)
library(quanteda)
#library(sentida)
library(sentimentr)
library(sentida)
library(zoo)
library(epubr)

df <- epub("kvinden_i_buret.epub")
df <- df$data[[1]]

# single row
df <- tibble(text = paste(df$text,collapse = ","))

sentences <- get_sentences(df$text) %>% 
  unlist() %>% 
  as_tibble() %>%
  mutate(sentence_id = row_number())  %>%
  rename(text = value)

sentences$part <- cut(sentences$sentence_id, breaks = 1000,labels=1:1000)

vsentida <- Vectorize(sentida)

sentences$sentiment <- vsentida(sentences$text,output="total")
                                
# method1 - no grouping
# sentiments2 <- sentiments %>%
#   # group_by(part) %>%
#   #summarise(m = rollmean(sentiment,k=100)) %>%
#   mutate(rollmean = rollmean(sentiment, k = 1000, fill = 0, align = "right")) %>%
#   as.data.table()
# 
# sentiments2 %>%
#   ggplot(aes(as.numeric(part),rollmean)) +
#   geom_line() +
#   geom_smooth(se=F)

# method2 - with grouping

sentiments <- sentences %>%
  group_by(part) %>%
  summarise(m = mean(sentiment)) %>%
  mutate(rollmean = rollmean(m, k = 50, fill = 0, align = "right"))

sentiments %>%
  ggplot(aes(as.numeric(part),rollmean)) +
  geom_col() +
  geom_smooth(se=F) +
  theme_void() +
  theme(plot.background = element_rect(fill="#272B30"),
        panel.background = element_rect(fill="#272B30"),
        panel.grid = element_blank())




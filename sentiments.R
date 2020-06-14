# sentiment analysis

library(tidyverse)
library(quanteda)
#library(sentida)
library(sentimentr)
library(zoo)

df <- read_csv("DaVinciCode.csv")

# single row
df <- tibble(text = paste(df$text,collapse = ","))

# sentimentr
sentiments <- sentiment(get_sentences(df$text))

sentiments$part <- cut(sentiments$sentence_id, breaks = 1000,labels=1:1000)

# method1 - no grouping
sentiments2 <- sentiments %>%
  # group_by(part) %>%
  #summarise(m = rollmean(sentiment,k=100)) %>%
  mutate(rollmean = rollmean(sentiment, k = 1000, fill = 0, align = "right")) %>%
  as.data.table()

sentiments2 %>%
  ggplot(aes(as.numeric(part),rollmean)) +
  geom_line() +
  geom_smooth(se=F)

# method2 - with grouping

sentiments3 <- sentiments %>%
  group_by(part) %>%
  summarise(m = mean(sentiment)) %>%
  mutate(rollmean = rollmean(m, k = 50, fill = 0, align = "right"))
  
sentiments3 %>%
  ggplot(aes(as.numeric(part),rollmean)) +
  geom_col() +
  geom_smooth(se=F) +
  theme_void() +
  theme(plot.background = element_rect(fill="#272B30"),
        panel.background = element_rect(fill="#272B30"),
        panel.grid = element_blank())




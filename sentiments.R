# sentiment analysis

library(tidyverse)
library(quanteda)
#library(sentida)
library(sentimentr)

df <- read_csv("DaVinciCode.csv")

# single row
df <- tibble(text = paste(df$text,collapse = ","))

# sentimentr
sentiments <- sentiment(get_sentences(df$text))

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
sentiments$part <- cut(sentiments$sentence_id, breaks = 1000,labels=1:1000)

sentiments3 <- sentiments %>%
  group_by(part) %>%
  summarise(m = mean(sentiment)) %>%
  mutate(rollmean = rollmean(m, k = 50, fill = 0, align = "right"))
  
sentiments3 %>%
  ggplot(aes(as.numeric(part),rollmean)) +
  geom_line() +
  geom_smooth(se=F)




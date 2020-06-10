# readability

library(tidyverse)
library(quanteda)

df <- read_csv("DaVinciCode.csv")

# single row
df <- tibble(text = paste(df$text,collapse = ","))

dfCorpus <- corpus(df,  text_field = "text")

textstat_readability(dfCorpus,
                     measure = c("Flesch.Kincaid"))

# lexical diversity
df_dfm <- dfm(dfCorpus)

textstat_lexdiv(df_dfm,measure = c("TTR", "CTTR", "K"))

